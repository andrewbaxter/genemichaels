#![doc = include_str!("../readme.md")]

use {
    base::{
        Error,
        ErrorDetail,
        VarkState,
        R,
    },
    help::VarkRetHelp,
    traits_impls::AargvarkTrait,
};
pub use aargvark_proc_macros::Aargvark;

/// Types related to producing help text.
pub mod help;

/// Base types - return types, errors, etc.
pub mod base;

/// Traits and helper traits for parsing arguments and default implementations.
pub mod traits_impls;

/// Result of varking when no errors occurred. Either results in parsed value or
/// the parsing was interrupted because help was requested.
pub enum VarkRet<T> {
    Ok(T),
    Help(VarkRetHelp),
}

/// Parse the explicitly passed in arguments - don't read application globals. The
/// `command` is only used in help and error text.
pub fn vark_explicit<T: AargvarkTrait>(command: Option<String>, args: Vec<String>) -> Result<VarkRet<T>, Error> {
    let mut state = VarkState::new(command, args);
    match T::vark(&mut state) {
        R::EOF => {
            return Err(Error {
                command: state.command,
                args: state.args,
                detail: ErrorDetail::TooLittle,
            });
        },
        R::Err => {
            return Err(Error {
                command: state.command,
                args: state.args,
                detail: ErrorDetail::Incorrect(state.errors),
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
                    args: state.args,
                    detail: ErrorDetail::TooMuch(state.i),
                });
            }
            return Ok(VarkRet::Ok(v));
        },
    }
}

/// Parse the command line arguments into the specified type. If parsing fails,
/// prints an error to stderr and exits with code 1. See `vark_explicit` if you'd
/// like more control (input, error handling, etc.)
pub fn vark<T: AargvarkTrait>() -> T {
    let mut args = std::env::args();
    let command = args.next();
    match vark_explicit(command, args.collect::<Vec<String>>()) {
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
