use anyhow::{
    anyhow,
    Result,
};
use clap::Parser;
use genemichaels::{
    es,
    format_str,
    FormatConfig,
    Comment,
};
use std::{
    fs,
    io::Read,
    path::PathBuf,
    process,
    str::FromStr,
    fmt::Display,
};
use syn::File;

fn main() {
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

    impl<
        E: std::error::Error + Send + Sync + 'static,
        T: FromStr<Err = E> + Clone + Display,
    > FromStr for Offable<T> {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
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

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if s == "on" {
                return Ok(On)
            } else {
                return Err(OnErr(format!("[{}] not allowed, must be on or off", s)))
            }
        }
    }

    #[derive(Parser)]
    struct Args {
        files: Vec<PathBuf>,
        #[arg(short, long, default_value_t = 120)]
        line_length: usize,
        #[arg(short, long, help = "Modify the file in place rather than print to stdout")]
        write: bool,
        #[arg(long, help = "For any node that's split, all parent nodes must also be split")]
        root_splits: bool,
        #[arg(
            long,
            help = "Always split {} groups with >= this number of children; disable with `off`",
            default_value_t = Offable::On(1),
        )]
        split_brace_threshold: Offable<usize>,
        #[arg(long, help = "Always split #[] attributes; disable with `false`", default_value_t = Offable::On(On))]
        split_attributes: Offable<On>,
        #[arg(
            long,
            help =
                "Use a max comment length relative to start of comment (i.e. ignoring indentation); disable with `off`",
            default_value_t = Offable::On(80),
        )]
        comment_length: Offable<usize>,
        #[arg(
            long,
            help = "Problems formatting comments are fatal; disable with `false`",
            default_value_t = Offable::On(On),
        )]
        comment_errors_fatal: Offable<On>,
    }

    let args = Args::parse();

    fn process(config: &FormatConfig, source: &str) -> Result<String> {
        let res = format_str(source, config)?;
        if !res.lost_comments.is_empty() {
            return Err(
                anyhow!(
                    "The following comments were missed during formatting: {:?}",
                    res.lost_comments.values().flat_map(|x| x).collect::<Vec<&Comment>>()
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

    let config = FormatConfig{
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
        match es!({
            if args.write {
                return Err(anyhow!("Can't update file when source is passed via stdin (no path specified)"));
            }
            let mut source = Vec::new();
            std::io::stdin().read_to_end(&mut source)?;
            let out = process(&config, &String::from_utf8(source)?)?;
            print!("{}", out);
            Ok(())
        }) {
            Ok(_) => { },
            Err(e) => {
                eprintln!("Error formatting stdin: {:?}", e);
                process::exit(1);
            },
        };
    } else {
        let mut failed = false;
        for file in &args.files {
            match es!({
                if args.write {
                    eprintln!("Formatting {}", &file.to_string_lossy());
                }
                let out = process(&config, &String::from_utf8(fs::read(file)?)?)?;
                if args.write {
                    fs::write(&file, out.as_bytes())?;
                } else {
                    print!("{}", out);
                }
                Ok(())
            }) {
                Ok(_) => { },
                Err(e) => {
                    eprintln!("Error formatting {:?}: {:?}", file, e);
                    failed = true;
                },
            };
        }
        if failed {
            process::exit(1);
        }
    }
}
