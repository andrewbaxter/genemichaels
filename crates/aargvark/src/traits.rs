use crate::{
    base::{
        R,
        VarkState,
    },
    help::{
        HelpPattern,
        HelpState,
    },
};

/// Anything that implements this trait can be parsed and used as a field in other
/// parsable enums/structs.
pub trait AargvarkTrait: Sized {
    /// Called when `-h` is specified.
    fn build_help_pattern(state: &mut HelpState) -> HelpPattern;

    /// Called when this argument is reached. Should parse data until no more data can
    /// be parsed.
    fn vark(state: &mut VarkState) -> R<Self>;
}

/// This method provides completions when `vark()` is invoked in the completion
/// mode.
///
/// This should produce a list of completion options, where each completion is a
/// list of command line arguments. The shell will replace the last partial
/// argument and add additional elements from the selected option.
///
/// The arguments will be automatically quoted and concatenated with spaces as
/// necessary for the output shell.
///
/// In most cases, each element will be a single-element vec (i.e. for file
/// completions, each element would be a vec containing one filename).
pub type AargvarkCompleter = Box<dyn Fn() -> Vec<Vec<String>>>;

/// This is a completer that returns no options, for use in types with no sensible
/// completion functionality.
pub fn empty_completer() -> AargvarkCompleter {
    return Box::new(|| vec![]);
}
