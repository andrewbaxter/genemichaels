use anyhow::{anyhow, Result};
use clap::Parser;
use genemichaels::{es, format_str, FormatConfig};
use std::{fs, io::Read, path::PathBuf, process};
use syn::File;

// TODO readme: install/usage with vs code, how it works,
fn main() {
    #[derive(Parser)]
    struct Args {
        files: Vec<PathBuf>,
        #[arg(short, long, default_value_t = 120)] line_length: usize,
        #[arg(short, long, help = "Modify the file in place rather than print to stdout")] write: bool,
        #[arg(long, help = "By default, if any node is split all parents will be split too; this disables that")]
        dont_root_splits: bool,
        #[arg(long, help = "Don't initially split any node with comments")] dont_split_comments: bool,
    }

    let args = Args::parse();

    fn process(config: &FormatConfig, source: &str) -> Result<String> {
        let res = format_str(source, config)?;
        if !res.lost_comments.is_empty() {
            return Err(anyhow!("The following comments were missed during formatting: {:?}", res.lost_comments));
        }
        match syn::parse_str::<File>(&res.rendered) {
            Ok(_) => { },
            Err(e) => {
                println!("{}", res.rendered);
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

    let config =
        FormatConfig {
            max_width: args.line_length,
            root_splits: !args.dont_root_splits,
            split_comments: !args.dont_split_comments,
        };
    if args.files.is_empty() {
        match es!(
            {
                if args.write {
                    return Err(anyhow!("Can't update file when source is passed via stdin (no path specified)"));
                }
                let mut source = Vec::new();
                std::io::stdin().read_to_end(&mut source)?;
                let out = process(&config, &String::from_utf8(source)?)?;
                print!("{}", out);
                Ok(())
            }
        ) {
            Ok(_) => { },
            Err(e) => {
                eprintln!("Error formatting stdin: {:?}", e);
                process::exit(1);
            },
        };
    } else {
        let mut failed = false;
        for file in &args.files { match es!({
            if args.write { eprintln!("Formatting {}", &file.to_string_lossy()); }
            let out = process(&config, &String::from_utf8(fs::read(file)?)?)?;
            if args.write { fs::write(&file, out.as_bytes())?; } else { print!("{}", out); }
            Ok(())
        }) { Ok(_) => { }, Err(e) => {
            eprintln!("Error formatting {:?}: {:?}", file, e);
            failed = true;
        } }; }
        if failed { process::exit(1); }
    }
}
