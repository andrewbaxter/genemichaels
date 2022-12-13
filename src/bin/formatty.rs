use anyhow::{anyhow, Context};
use clap::Parser;
use formatty::{es, format_str, FormatConfig};
use std::{fs, io::Read, path::PathBuf, process};
use syn::File;

// TODO
// readme: install/usage with vs code, how it works,

fn main() {
    #[derive(Parser)]
    struct Args {
        file: Option<PathBuf>,
        #[arg(short, long, default_value_t = 120)]
        line_length: usize,
        #[arg(
            short,
            long,
            help = "Modify the file in place rather than print to stdout"
        )]
        write: bool,
        #[arg(long, help = "If any node is split, split all parents too")]
        root_splits: bool,
        #[arg(long, help = "Split any node with comments")]
        split_comments: bool,
    }
    let args = Args::parse();
    match es!({
        let source = match &args.file {
            Some(f) => fs::read(f)?,
            None => {
                if args.write {
                    return Err(anyhow!(
                        "Can't update file when source is passed via stdin (no path specified)"
                    ));
                }
                let mut bytes = Vec::new();
                std::io::stdin().read_to_end(&mut bytes)?;
                bytes
            }
        };
        let source = String::from_utf8(source)?;
        let res = format_str(
            &source,
            &FormatConfig {
                max_width: args.line_length,
                root_splits: args.root_splits,
                split_comments: args.split_comments,
            },
        )?;

        if !res.lost_comments.is_empty() {
            return Err(anyhow!(
                "The following comments were missed during formatting: {:?}",
                res.lost_comments
            ));
        }

        syn::parse_str::<File>(&res.rendered)
            .context("Rendered document couldn't be re-parsed in verification step")?;

        if args.write {
            fs::write(args.file.as_ref().unwrap(), res.rendered.as_bytes())?;
        } else {
            print!("{}", res.rendered);
        }

        Ok(())
    }) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error formatting {:?}: {:?}", args.file, e);
            process::exit(1);
        }
    };
}
