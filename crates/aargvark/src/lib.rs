#![doc = include_str!("../readme.md")]

pub use aargvark_proc_macros::Aargvark;

/// Types related to producing help text.
pub mod help;

/// Base types - return types, errors, etc.
pub mod base;

/// The main trait for parsing arguments.
pub mod traits;

/// Default implementations and helper traits.
pub mod traits_impls;

use {
    base::{
        Error,
        VarkFailure,
        VarkState,
        R,
    },
    help::VarkRetHelp,
    traits::AargvarkTrait,
};

/// Result of varking when no errors occurred. Either results in parsed value or
/// the parsing was interrupted because help was requested.
pub enum VarkRet<T> {
    Ok(T),
    Help(VarkRetHelp),
}

/// Parse the explicitly passed in arguments - don't read application globals. The
/// `command` is only used in help and error text. This abstracts the parsing away
/// from command-line usage so it can be used in other contexts.
pub fn vark_explicit<T: AargvarkTrait>(command: Option<String>, args: Vec<String>) -> Result<VarkRet<T>, Error> {
    let mut state = VarkState::new(false, command, args);
    match T::vark(&mut state) {
        R::Err => {
            return Err(Error {
                command: state.command,
                args: state.args,
                detail: state.errors,
            });
        },
        R::Help(builder) => {
            return Ok(VarkRet::Help(VarkRetHelp {
                command: state.command,
                args: state.args,
                consumed_args: state.i,
                builder: builder,
            }));
        },
        R::Ok(v) => {
            if state.i != state.args.len() {
                return Err(Error {
                    command: state.command,
                    detail: vec![VarkFailure {
                        arg_offset: state.i,
                        error: format!(
                            "Error parsing command line arguments: final arguments are unrecognized\n{:?}",
                            &state.args[state.i..]
                        ),
                    }],
                    args: state.args,
                });
            }
            return Ok(VarkRet::Ok(v));
        },
    }
}

#[derive(PartialEq, Eq)]
pub enum CompleteCursorPosition {
    /// Cursor is at the start of a new argument
    Empty,
    /// Cursor is at the end of a partially-written argument
    Partial,
}

/// Generate completions for the provided argument list.
///
/// The result is a list of completion options, where each option is a list of
/// unquoted command line arguments. When output to a shell, the arguments should
/// be quoted and joined by spaces as appropriate for the shell.
pub fn vark_complete<
    T: AargvarkTrait,
>(cursor: CompleteCursorPosition, command: Option<String>, args: Vec<String>) -> Vec<Vec<String>> {
    let mut args = args;
    if args.is_empty() || cursor == CompleteCursorPosition::Empty {
        args.push("".to_string());
    }
    let mut state = VarkState::new(true, command, args);
    T::vark(&mut state);
    return (state.last_completer.take().unwrap())();
}

/// Parse the command line arguments into the specified type. If parsing fails,
/// prints an error to stderr and exits with code 1. See `vark_explicit` if you'd
/// like more control (input, error handling, etc.)
pub fn vark<T: AargvarkTrait>() -> T {
    let mut args = std::env::args();
    let command = args.next();
    let mut args = args.collect::<Vec<String>>();
    if let Some(complete) = std::env::var_os("AARGVARK_COMPLETE") {
        let cursor = match complete.as_encoded_bytes() {
            b"empty" => CompleteCursorPosition::Empty,
            b"partial" => CompleteCursorPosition::Partial,
            _ => {
                eprintln!("Invalid value for AARGVARK_COMPLETE");
                std::process::exit(1);
            },
        };

        // Skip first argument, in bash the line comes with the invoked program name
        args.remove(0);
        let res = vark_complete::<T>(cursor, command, args);
        println!(
            "{}",
            res
                .into_iter()
                .map(
                    |args| args
                        .iter()
                        .map(|a| shell_escape::escape(std::borrow::Cow::Borrowed(&a)))
                        .collect::<Vec<_>>()
                        .join(" "),
                )
                .collect::<Vec<_>>()
                .join("\n")
        );
        std::process::exit(0);
    } else {
        match vark_explicit::<T>(command, args) {
            Ok(v) => match v {
                VarkRet::Ok(v) => return v,
                VarkRet::Help(h) => {
                    println!("{}", h.render());
                    std::process::exit(0);
                },
            },
            Err(e) => {
                eprintln!("{:?}", e);
                std::process::exit(1);
            },
        }
    }
}
