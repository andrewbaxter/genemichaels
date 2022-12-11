use std::{fs, path::PathBuf, process};

use clap::Parser;
use formatty::{es, format_str};

fn main() {
    #[derive(Parser)]
    struct Args {
        file: PathBuf,
        #[arg(default_value_t = 80)]
        line_length: usize,
    }
    let args = Args::parse();
    match es!({
        let source = String::from_utf8(fs::read(&args.file)?)?;
        let rendered = format_str(&source, args.line_length)?;
        print!("{}", rendered);

        Ok(())
    }) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error formatting {}: {:?}", args.file.to_string_lossy(), e);
            process::exit(1);
        }
    };
}
