use {
    crate::{
        help::{
            HelpPartialProduction,
            HelpState,
        },
        traits::AargvarkCompleter,
    },
    unicode_width::UnicodeWidthStr,
};

pub struct VarkFailure {
    pub arg_offset: usize,
    pub error: String,
}

/// Return type enum (like `Result`) during parsing.
pub enum R<T> {
    /// Parsing failed due to incorrect arguments.
    Err,
    /// Encountered `-h` or `--help` and aborted.
    Help(Box<dyn FnOnce(&mut HelpState) -> HelpPartialProduction>),
    /// Successfully parsed value.
    Ok(T),
}

/// Possible results of peeking the output.
#[derive(PartialEq, Eq)]
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
    pub(crate) provide_completions: bool,
    pub(crate) command: Option<String>,
    pub(crate) args: Vec<String>,
    pub(crate) i: usize,
    pub(crate) last_completer: Option<AargvarkCompleter>,
    pub(crate) errors: Vec<VarkFailure>,
}

impl VarkState {
    pub fn new(provide_completions: bool, command: Option<String>, args: Vec<String>) -> Self {
        return Self {
            provide_completions: provide_completions,
            command: command,
            args: args,
            i: 0,
            last_completer: None,
            errors: vec![],
        };
    }
}

impl VarkState {
    /// In autocomplete mode - if true, then EOF should contain a list of possible next
    /// entries (or empty if it's impossible to determine or there's nothing left to
    /// type)
    pub fn provide_completions<'a>(&'a self) -> bool {
        return self.provide_completions;
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

    /// Produce a "parse successful" return value. If completer is present, it'll
    /// replace the current completer.
    pub fn r_ok<T>(&mut self, v: T, completer: Option<AargvarkCompleter>) -> R<T> {
        if let Some(completer) = completer {
            self.last_completer = Some(completer);
        }
        return R::Ok(v);
    }

    /// Produce a "parse failed" return value (includes which argument was being
    /// inspected when the failure occured).
    pub fn r_err<T>(&mut self, text: String, completer: Option<AargvarkCompleter>) -> R<T> {
        if let Some(completer) = completer {
            self.last_completer = Some(completer);
        }
        self.errors.push(VarkFailure {
            arg_offset: self.i,
            error: text,
        });
        return R::Err;
    }

    /// Produce an "argument missing" return value. This doesn't clear/replace the
    /// completer (important during completion for completing on partial but valid
    /// arguments).
    pub fn r_err_missing<T>(&mut self) -> R<T> {
        return self.r_err("Missing argument(s), use --help for more info".to_string(), None);
    }
}

/// Returned by `vark_explicit`. `command` is whatever is passed as `command` to
/// `vark_explicit`, the first of argv if using `vark`. `args` is the remaining
/// arguments.
pub struct Error {
    pub command: Option<String>,
    pub args: Vec<String>,
    pub detail: Vec<VarkFailure>,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
        for e in self.detail.iter() {
            text.push_str("\n");
            text.push_str(&format!(" * {}\n", e.error));
            text.push_str("   ");
            text.push_str(&display_args);
            text.push_str("\n");
            text.push_str("   ");
            text.push_str(
                &" ".repeat(display_arg_offsets.get(e.arg_offset + offset_offset).cloned().unwrap_or(0usize)),
            );
            text.push_str("^\n");
        }
        return text.fmt(f);
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return std::fmt::Display::fmt(self, f);
    }
}

impl std::error::Error for Error { }
