use anyhow::{anyhow, Context, Result};
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
        #[arg(long, help = "If any node is split, split all parents too")] root_splits: bool,
        #[arg(long, help = "Split any node with comments")] split_comments: bool,
    }

    let args = Args::parse();

    fn process(config: &FormatConfig, source: &str) -> Result<String> {
        let res = format_str(source, config)?;
        if !res.lost_comments.is_empty() {
            return Err(anyhow!("The following comments were missed during formatting: {:?}", res.lost_comments));
        }
        syn::parse_str::<File>(
            &res.rendered,
        ).context("Rendered document couldn't be re-parsed in verification step")?;
        Ok(res.rendered)
    }

    let config =
        FormatConfig {
            max_width: args.line_length,
            root_splits: args.root_splits,
            split_comments: args.split_comments,
        };
    if args.files.is_empty() { match es!({
        if args.write { return Err(anyhow!("Can't update file when source is passed via stdin (no path specified)")); }
        let mut source = Vec::new();
        std::io::stdin().read_to_end(&mut source)?;
        let out = process(&config, &String::from_utf8(source)?)?;
        print!("{}", out);
        Ok(())
    }) { Ok(_) => { }, Err(e) => {
        eprintln!("Error formatting stdin: {:?}", e);
        process::exit(1);
    } }; } else {
        let mut failed = false;
        for file in &args.files { match es!({
            let out = process(&config, &String::from_utf8(fs::read(file)?)?)?;
            if args.write {
                eprintln!("Formatted {}", &file.to_string_lossy());
                fs::write(&file, out.as_bytes())?;
            } else { print!("{}", out); }
            Ok(())
        }) { Ok(_) => { }, Err(e) => {
            eprintln!("Error formatting {:?}: {:?}", file, e);
            failed = true;
        } }; }
        if failed { process::exit(1); }
    }
}
