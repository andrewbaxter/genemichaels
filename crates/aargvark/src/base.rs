use {
    crate::help::{
        HelpPartialProduction,
        HelpState,
    },
    unicode_width::UnicodeWidthStr,
};

pub struct VarkFailure {
    pub arg_offset: usize,
    pub error: String,
}

/// Return type enum (like `Result`) during parsing.
pub enum R<T> {
    /// Ran out of arguments before parsing successfully completed.
    ///
    /// If autocomplete is enabled, this should contain autocomplete choices for where
    /// the input ran out and no further parsing should be attempted (i.e. choices
    /// should be propagated directly upward).
    EOF(Vec<String>),
    /// Parsing failed due to incorrect arguments.
    Err,
    /// Encountered `-h` or `--help` and aborted.
    Help(Box<dyn FnOnce(&mut HelpState) -> HelpPartialProduction>),
    /// Successfully parsed value.
    Ok(T),
}

/// Possible results of peeking the output.
pub enum PeekR<'a> {
    /// There's another argument
    Ok(&'a str),
    /// No more arguments remain
    None,
    /// The next argument is `-h` or `--help` - caller should return `R::Help`.
    Help,
}

#[doc(hidden)]
pub struct VarkState {
    pub(crate) autocomplete: bool,
    pub(crate) command: Option<String>,
    pub(crate) args: Vec<String>,
    pub(crate) i: usize,
    pub(crate) errors: Vec<VarkFailure>,
}

impl VarkState {
    pub fn new(autocomplete: bool, command: Option<String>, args: Vec<String>) -> Self {
        return Self {
            autocomplete: autocomplete,
            command: command,
            args: args,
            i: 0,
            errors: vec![],
        };
    }
}

impl VarkState {
    /// In autocomplete mode - if true, then EOF should contain a list of possible next
    /// entries (or empty if it's impossible to determine or there's nothing left to
    /// type)
    pub fn autocomplete<'a>(&'a self) -> bool {
        return self.autocomplete;
    }

    /// Return the next argument without consuming it.
    pub fn peek<'a>(&'a self) -> PeekR<'a> {
        if self.i >= self.args.len() {
            return PeekR::None;
        }
        let v = &self.args[self.i];
        if v == "-h" || v == "--help" {
            return PeekR::Help;
        }
        return PeekR::Ok(v);
    }

    /// The argument the current argument pointer is pointing to.
    pub fn position(&self) -> usize {
        return self.i;
    }

    /// Reset the agument pointer to an earlier argument (i.e. after consuming N
    /// arguments but finding the required final argument missing).
    pub fn rewind(&mut self, i: usize) {
        self.i = i;
    }

    /// Move the argument pointer to the next argument (ex: after inspecting it using
    /// `peek`).
    pub fn consume(&mut self) {
        self.i += 1;
    }

    /// Produce a "parse successful" return value.
    pub fn r_ok<T>(&self, v: T) -> R<T> {
        return R::Ok(v);
    }

    /// Produce a "parse failed" return value (includes which argument was being
    /// inspected when the failure occured).
    pub fn r_err<T>(&mut self, text: String) -> R<T> {
        self.errors.push(VarkFailure {
            arg_offset: self.i,
            error: text,
        });
        return R::Err;
    }
}

pub enum ErrorDetail {
    /// The parser needed more command line arguments.
    TooLittle,
    /// Fully parsed command but additional unconsumed arguments follow (offset of
    /// first unrecognized argument)
    TooMuch(usize),
    /// Aargvark considers multiple possible parses. This is a list of considered
    /// parses, in order of when they were ruled out.
    Incorrect(Vec<VarkFailure>),
}

/// Returned by `vark_explicit`. `command` is whatever is passed as `command` to
/// `vark_explicit`, the first of argv if using `vark`. `args` is the remaining
/// arguments.
pub struct Error {
    pub command: Option<String>,
    pub args: Vec<String>,
    pub detail: ErrorDetail,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.detail {
            ErrorDetail::TooLittle => {
                return "Missing arguments, use --help for more info".fmt(f);
            },
            ErrorDetail::TooMuch(first) => {
                return format_args!(
                    "Error parsing command line arguments: final arguments are unrecognized\n{:?}",
                    &self.args[*first..]
                ).fmt(f);
            },
            ErrorDetail::Incorrect(failures) => {
                let mut display_args = vec![];
                let mut offset_offset = 0;
                if let Some(c) = &self.command {
                    display_args.push(c.clone());
                    offset_offset = 1;
                }
                display_args.extend(self.args.iter().map(|a| format!("{:?}", a)));
                let mut display_arg_offsets = vec![];
                {
                    let mut offset = 0;
                    for d in &display_args {
                        display_arg_offsets.push(offset);
                        offset += d.width() + 1;
                    }
                    display_arg_offsets.push(offset);
                }
                let mut display_args = display_args.join(" ");
                display_args.push_str(" <END>");
                let mut text = "Error parsing arguments.\n".to_string();
                for e in failures.iter().rev() {
                    text.push_str("\n");
                    text.push_str(&format!(" * {}\n", e.error));
                    text.push_str("   ");
                    text.push_str(&display_args);
                    text.push_str("\n");
                    text.push_str("   ");
                    text.push_str(
                        &" ".repeat(
                            display_arg_offsets.get(e.arg_offset + offset_offset).cloned().unwrap_or(0usize),
                        ),
                    );
                    text.push_str("^\n");
                }
                return text.fmt(f);
            },
        }
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return std::fmt::Display::fmt(self, f);
    }
}

impl std::error::Error for Error { }
