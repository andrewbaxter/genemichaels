use anyhow::anyhow;
use cargo_manifest::Manifest;
use clap::Parser;
use genemichaels::{
    format_str,
    print_error_text,
    print_skipping_text,
    Comment,
    FormatConfig,
};
use std::{
    env::current_dir,
    ffi::OsStr,
    fmt::Display,
    fs,
    io::Read,
    path::{
        Path,
        PathBuf,
    },
    process,
    result,
    str::FromStr,
    thread,
    time,
};
use syn::File;

type Result<T> = result::Result<T, anyhow::Error>;
const CARGO_TOML: &str = "Cargo.toml";

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

#[derive(Parser, Clone)]
struct Args {
    #[arg(help = "Files to format in place; if none specified formats stdin and writes result to stdout")]
    files: Vec<PathBuf>,
    #[arg(short, long, help = "Won't emit any output")]
    quiet: bool,
    #[arg(short, long, help = "Formats the entire workspace")]
    workspace: bool,
    #[arg(short, long, help = "Formats the current folder")]
    folder: bool,
    #[arg(short, long, default_value_t = FormatConfig::default().max_width)]
    line_length: usize,
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
            return Err(
                anyhow!(
                    "Rendered document couldn't be re-parsed in verification step at {}:{}: {}\n\n{}",
                    e.span().start().line,
                    e.span().start().column,
                    e,
                    res
                        .rendered
                        .lines()
                        .enumerate()
                        .skip(e.span().start().line.checked_sub(5).unwrap_or(0))
                        .take(10)
                        .map(|(ln, l)| format!("{:0>4} {}", ln + 1, l))
                        .collect::<Vec<String>>()
                        .join("\n")
                ),
            );
        },
    };
    Ok(res.rendered)
}

fn process_workspace_file(args: &Args, config: FormatConfig, file_path: PathBuf) {
    let res = || -> Result<()> {
        let source = fs::read_to_string(file_path.clone())?;
        if skip(&source) {
            print_skipping_text();
            eprintln!("{}", &file_path.to_string_lossy());
            return Result::Ok(());
        }
        fs::write(&file_path, process(args, &config, &source)?.as_bytes())?;
        Ok(())
    };
    match res() {
        Ok(_) => {
            eprintln!("\x1B[1;32m   Formatted\x1B[0;22m {}", file_path.as_path().to_string_lossy());
        },
        Err(e) => {
            print_error_text();
            eprintln!("formatting file {}: {:?}", file_path.to_string_lossy(), e);
        },
    }
}

fn main() {
    let args = Args::parse();
    let config = FormatConfig {
        quiet: args.quiet,
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
    if args.folder {
        let res = || -> Result<()> {
            {
                let inst = time::Instant::now();
                eprintln!("\x1B[1;32m  Formatting\x1B[0;22m folder...");
                let c_dir = current_dir()?;
                let manifest = cargo_manifest::Manifest::from_path(c_dir.join(CARGO_TOML))?;
                process_cargo_toml(c_dir, manifest, &args, config);
                eprintln!(
                    "\x1B[1;32m    Finished\x1B[0;22m folder formatting successfully in {:.2}s",
                    time::Instant::now().duration_since(inst).as_secs_f64()
                );
                Result::Ok(())
            }
        };
        match res() {
            Ok(_) => { },
            Err(e) => {
                print_error_text();
                eprintln!("formatting: {:?}", e);
                process::exit(1);
            },
        };
    } else if args.workspace {
        let res = || -> Result<()> {
            {
                let inst = time::Instant::now();
                eprintln!("\x1B[1;32m  Formatting\x1B[0;22m workspace...");
                let mut workspace_cargo_toml = None;
                let c_dir = current_dir()?;
                let mut at: Option<&Path> = Some(&c_dir);
                while let Some(d) = at.take() {
                    if d.join(CARGO_TOML).exists() &&
                        cargo_manifest::Manifest::from_path(d.join(CARGO_TOML))
                            .ok()
                            .and_then(|m| m.workspace)
                            .is_some() {
                        workspace_cargo_toml = Some(d.join(CARGO_TOML));
                        break;
                    }
                    at = d.parent();
                }
                let wct = workspace_cargo_toml.ok_or_else(|| anyhow::anyhow!("No workspace found!"))?;
                let manifest = cargo_manifest::Manifest::from_path(wct)?;
                process_cargo_toml(c_dir.clone(), manifest, &args, config);
                eprintln!(
                    "\x1B[1;32m    Finished\x1B[0;22m workspace formatting successfully in {:.2}s",
                    time::Instant::now().duration_since(inst).as_secs_f64()
                );
                Result::Ok(())
            }
        };
        match res() {
            Ok(_) => { },
            Err(e) => {
                print_error_text();
                eprintln!("formatting: {:?}", e);
                process::exit(1);
            },
        };
    } else if args.files.is_empty() {
        let inst = time::Instant::now();
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
                if !args.quiet {
                    print_error_text();
                    eprintln!("formatting stdin: {:?}", e);
                }
                process::exit(1);
            },
        };
        eprintln!(
            "\x1B[1;32m    Finished\x1B[0;22m workspace formatted successfully in {:.2}s",
            time::Instant::now().duration_since(inst).as_secs_f64()
        );
    } else {
        let inst = time::Instant::now();
        let mut failed = false;
        for file in &args.files {
            let res = || -> Result<()> {
                let source = String::from_utf8(fs::read(file)?)?;
                if skip(&source) {
                    print_skipping_text();
                    eprintln!("{}", &file.to_string_lossy());
                    return Ok(());
                }
                if !args.quiet {
<<<<<<< HEAD
                    eprintln!("Formatting {}", &file.to_string_lossy());
                }
                let out = process(&config, &source)?;
=======
                    eprintln!("\x1B[1;32m  Formatting\x1B[0;22m {}", &file.to_string_lossy());
                };
                let out = process(&args, &config, &source)?;
>>>>>>> 5185f5c (init workspace finder)
                fs::write(file, out.as_bytes())?;
                Ok(())
            };
            match res() {
                Ok(_) => { },
                Err(e) => {
                    if !args.quiet {
                        print_error_text();
                        eprintln!("formatting {}: {:?}", &file.to_string_lossy(), e);
                    }
                    failed = true;
                },
            };
        }
        eprintln!(
            "\x1B[1;32m    Finished\x1B[0;22m workspace formatted successfully in {:.2}s",
            time::Instant::now().duration_since(inst).as_secs_f64()
        );
        if failed {
            process::exit(1);
        }
    }
}

fn process_cargo_toml(path: PathBuf, manifest: Manifest, args: &Args, config: FormatConfig) {
    let dirs = match manifest.workspace.map(|wkspc| wkspc.members) {
        Some(mut mb) => {
            if let Some(bins) = manifest.bin {
                for bin in bins {
                    mb.push(
                        match bin
                            .path
                            .and_then(|p| p.parse::<PathBuf>().ok())
                            .and_then(|p| p.parent().map(|pp| pp.to_path_buf())) {
                            Some(p) => p.to_string_lossy().to_string(),
                            None => {
                                continue;
                            },
                        },
                    )
                }
            }
            if let Some(lib) = manifest.lib {
                if let Some(p) =
                    lib
                        .path
                        .and_then(|p| p.parse::<PathBuf>().ok())
                        .and_then(|p| p.parent().map(|pp| pp.to_path_buf())) {
                    mb.push(p.to_string_lossy().to_string())
                }
            }
            if let Some(benches) = manifest.bench {
                for bench in benches {
                    mb.push(
                        match bench
                            .path
                            .and_then(|p| p.parse::<PathBuf>().ok())
                            .and_then(|p| p.parent().map(|pp| pp.to_path_buf())) {
                            Some(p) => p.to_string_lossy().to_string(),
                            None => {
                                continue;
                            },
                        },
                    )
                }
            }
            if let Some(tests) = manifest.test {
                for test in tests {
                    mb.push(
                        match test
                            .path
                            .and_then(|p| p.parse::<PathBuf>().ok())
                            .and_then(|p| p.parent().map(|pp| pp.to_path_buf())) {
                            Some(p) => p.to_string_lossy().to_string(),
                            None => {
                                continue;
                            },
                        },
                    )
                }
            }
            if let Some(examples) = manifest.example {
                for example in examples {
                    mb.push(
                        match example
                            .path
                            .and_then(|p| p.parse::<PathBuf>().ok())
                            .and_then(|p| p.parent().map(|pp| pp.to_path_buf())) {
                            Some(p) => p.to_string_lossy().to_string(),
                            None => {
                                continue;
                            },
                        },
                    )
                }
            }
            mb
        },
        // if no members, assume single folder workspace
        None => Vec::from([String::new()]),
    };

    // solves the situation where if the bin/ is inide src/ file doesn't get formatted
    // twice, open to better solution
    let mut formatted_files = Vec::new();
    for dir in dirs {
        let mut threads = Vec::new(); // moved threads inside first loop to improve print output
        let current_working_folder = path.join(dir);
        for f in walkdir::WalkDir::new(&current_working_folder) {
            match f {
                Ok(file) => {
                    let file_path = file.path().to_path_buf();
                    if !formatted_files.contains(&file_path) && file_path.extension() == Some(OsStr::new("rs")) {
                        formatted_files.push(file_path);
                        let args = args.clone();
                        threads.push(thread::spawn(move || {
                            process_workspace_file(&args, config, file.path().to_path_buf());
                        }));
                    }
                },
                Err(e) => {
                    eprintln!("Error opening file {}: {}", current_working_folder.to_string_lossy(), e);
                    continue;
                },
            }
        }
        for thread in threads {
            let _ = thread.join();
        };
    }
}
