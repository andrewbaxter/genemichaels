use anyhow::anyhow;
use clap::Parser;
use genemichaels::{
    format_str,
    FormatConfig,
    Comment,
};
use std::{
    fs,
    io::{
        Read,
    },
    path::PathBuf,
    process,
    str::FromStr,
    fmt::Display,
    result,
};
use syn::File;

type Result<T> = result::Result<T, anyhow::Error>;

#[derive(Clone)]
enum Offable<T> {
    Off,
    On(T),
}

impl<T: Display> std::fmt::Display for Offable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Offable::Off => "off".fmt(f),
            Offable::On(x) => x.fmt(f),
        }
    }
}

impl<E: std::error::Error + Send + Sync + 'static, T: FromStr<Err = E> + Clone + Display> FromStr for Offable<T> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        if s == "off" {
            Ok(Self::Off)
        } else {
            Ok(Self::On(T::from_str(s)?))
        }
    }
}
#[derive(Clone)]
struct On;

impl std::fmt::Display for On {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "on".fmt(f)
    }
}

#[derive(Debug)]
struct OnErr(String);

impl Display for OnErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for OnErr {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl FromStr for On {
    type Err = OnErr;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        if s == "on" {
            Ok(On)
        } else {
            Err(OnErr(format!("[{}] not allowed, must be on or off", s)))
        }
    }
}

#[derive(Parser)]
struct Args {
    #[arg(help = "Files to format in place; if none specified formats stdin and writes result to stdout")]
    files: Vec<PathBuf>,
    #[arg(short, long, default_value_t = FormatConfig::default().max_width)]
    line_length: usize,
    #[arg(short, long, help = "Won't emit any output")]
    quiet: bool,
    #[arg(long, help = "For any node that's split, all parent nodes must also be split")]
    root_splits: bool,
    #[arg(
        long,
        help = "Always split {} groups with >= this number of children; disable with `off`",
        default_value_t = match FormatConfig::default().split_brace_threshold {
            Some(x) => Offable::On(x),
            None => Offable::Off,
        },
    )]
    split_brace_threshold: Offable<usize>,
    #[arg(
        long,
        help = "Always split #[] attributes; disable with `off`",
        default_value_t = match FormatConfig::default().split_attributes {
            true => Offable::On(On),
            false => Offable::Off,
        },
    )]
    split_attributes: Offable<On>,
    #[arg(
        long,
        help = "Always split where clauses; disable with `off`",
        default_value_t = match FormatConfig::default().split_attributes {
            true => Offable::On(On),
            false => Offable::Off,
        },
    )]
    split_where: Offable<On>,
    #[arg(
        long,
        help = "Use a max comment length relative to start of comment (i.e. ignoring indentation); disable with `off`",
        default_value_t = match FormatConfig::default().comment_width {
            Some(x) => Offable::On(x),
            None => Offable::Off,
        },
    )]
    comment_length: Offable<usize>,
    #[arg(
        long,
        help = "Problems formatting comments are fatal; disable with `false`",
        default_value_t = match FormatConfig::default().comment_errors_fatal {
            true => Offable::On(On),
            false => Offable::Off,
        },
    )]
    comment_errors_fatal: Offable<On>,
}

fn skip(src: &str) -> bool {
    src.lines().take(5).any(|l| l.contains("`nogenemichaels`"))
}

fn process(config: &FormatConfig, source: &str) -> Result<String> {
    let res = format_str(source, config)?;
    if !res.lost_comments.is_empty() {
        return Err(
            anyhow!(
                "The following comments were missed during formatting: {:?}",
                res.lost_comments.values().flatten().collect::<Vec<&Comment>>()
            ),
        );
    }
    match syn::parse_str::<File>(&res.rendered) {
        Ok(_) => { },
        Err(e) => {
            eprintln!("{}", res.rendered);
            return Err(
                anyhow!(
                    "Rendered document couldn't be re-parsed in verification step: {}:{}: {}",
                    e.span().start().line,
                    e.span().start().column,
                    e
                ),
            );
        },
    };
    Ok(res.rendered)
}

fn main() {
    let args = Args::parse();
    let config = FormatConfig {
        max_width: args.line_length,
        root_splits: args.root_splits,
        split_brace_threshold: match args.split_brace_threshold {
            Offable::Off => None,
            Offable::On(n) => Some(n),
        },
        split_attributes: match args.split_attributes {
            Offable::Off => false,
            Offable::On(_) => true,
        },
        split_where: match args.split_where {
            Offable::Off => false,
            Offable::On(_) => true,
        },
        comment_width: match args.comment_length {
            Offable::Off => None,
            Offable::On(n) => Some(n),
        },
        comment_errors_fatal: match args.comment_errors_fatal {
            Offable::Off => false,
            Offable::On(_) => true,
        },
    };
    if args.files.is_empty() {
        let res = || -> Result<()> {
            let mut source = Vec::new();
            std::io::stdin().read_to_end(&mut source)?;
            let source = String::from_utf8(source)?;
            if skip(&source) {
                print!("{}", source);
                anyhow::Ok(())
            } else {
                let out = process(&config, &source)?;
                print!("{}", out);
                anyhow::Ok(())
            }
        };
        match res() {
            Ok(_) => { },
            Err(e) => {
                eprintln!("Error formatting stdin: {:?}", e);
                process::exit(1);
            },
        };
    } else {
        let mut failed = false;
        for file in &args.files {
            let res = || -> Result<()> {
                let source = String::from_utf8(fs::read(file)?)?;
                if skip(&source) {
                    eprintln!("Skipping {}", &file.to_string_lossy());
                    return Ok(());
                }
                eprintln!("Formatting {}", &file.to_string_lossy());
                let out = process(&config, &source)?;
                fs::write(file, out.as_bytes())?;
                Ok(())
            };
            match res() {
                Ok(_) => { },
                Err(e) => {
                    eprintln!("Error formatting {}: {:?}", &file.to_string_lossy(), e);
                    failed = true;
                },
            };
        }
        if failed {
            process::exit(1);
        }
    }
}
