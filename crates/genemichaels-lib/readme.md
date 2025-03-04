This is the library-version of `genemichaels`, useful for generating code, testing proc macro output (deterministically formatting for string comparisons), the uses are various!

# Usage

Do `cargo add genemichaels`

There are three main functions:

- `genemichaels::format_str` - formats a string (full rust source file, doesn't support snippets at the moment).
- `genemichaels::format_ast` - formats AST element (implements `genemichaels::Formattable`, most `syn::*` structs do). Comments need to be passed in separately, if you have any.
- `genemichaels::extract_comments` - takes a string of source code and extracts comments, mapping each comment to the start of a syntax element

If you want to format a `TokenStream`, parse it into an AST with `syn::parse2::<syn::File>(token_stream)` then call `format_ast`.

The format functions also return lost comments - comments not formatted/added to the formatted source after processing. In an ideal world this wouldn't exist, but right now comments are added on a case by case basis and not all source tokens support comments.
