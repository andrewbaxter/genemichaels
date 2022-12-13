use std::{fs, path::PathBuf, process};

use clap::Parser;
use formatty::{es, format_str, FormatConfig};

fn main() {
    #[derive(Parser)]
    struct Args {
        file: PathBuf,
        #[arg(default_value_t = 120)]
        line_length: usize,
        #[arg(long)]
        root_splits: bool,
        #[arg(long)]
        split_comments: bool,
    }
    let args = Args::parse();
    match es!({
        let source = String::from_utf8(fs::read(&args.file)?)?;
        let rendered = format_str(
            &source,
            &FormatConfig {
                max_width: args.line_length,
                root_splits: args.root_splits,
                split_comments: args.split_comments,
            },
        )?;
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
