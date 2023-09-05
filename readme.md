# Gene Michaels

Status: **Alpha**. I've been using it for months without issue, and I've tested against various code bases and doesn't blow them up. Right now post-formatting pre-writing it re-parses and confirms all comments are consumed as safety checks. Also files over 500kb may take all your memory and invoke the OOM killer.

- formats everything
- doesn't not format some things
- this is a haiku

Named after Gene Michaels.

Everything includes macros and comments. Dog fooded in this repo.

### Differences to Rustfmt

- This formats all macros, Rustfmt only formats macros under certain conditions
- This is fully deterministic, Rustfmt keeps certain stylistic choices like
- Rustfmt has [several](https://github.com/rust-lang/rustfmt/issues/3863) [restrictions](https://github.com/rust-lang/rustfmt/issues/2896) in what it formats normally, this always formats everything (if it doesn't it's a bug)
- This also reformats comments per Markdown rules

# Usage

Run `cargo install genemichaels`.

Running `genemichaels` will by default format all files in the current package (looking at `Cargo.toml` in the current directory). You can also pass in a list of filenames to format.

## VS Code

If you're using VS Code, add the setting:

```
  "rust-analyzer.rustfmt.overrideCommand": [
    "${userHome}/.cargo/bin/genemichaels", "--stdin"
  ]
```

to use it with reckless abandon.

## Configuration

The config contains parameters that tweak the formatting output, suitable for establishing a convention for a project. Things that don't affect the output (thread count, verbosity, etc) are command line arguments instead.

The configuration file is json, but it will strip lines starting with `//` first if you want to add comments.

Here's the config file. All values shown are defaults and the keys can be omitted if the default works for you.

```jsonc
{
  // Ideal maximum line width. If there's an unbreakable element the line won't be split.
  "max_width": 120,
  // When breaking a child element, also break all parent elements.
  "root_splits": false,
  // Break a `()` or `{}` if it has greater than this number of children.  Set to `null` to
  // disable breaking due to high child counts.
  "split_brace_threshold": 1,
  // Break a `#[]` on a separate line before the element it's associated with.
  "split_attributes": true,
  // Put the `where` clause on a new line.
  "split_where": true,
  // Maximum relative line length for comments (past the comment indentation level). Can be
  // `null` to disable relative wrapping.  If disabled, still wraps at `max_width`.
  "comment_width": 80,
  // If reformatting comments results in an error, abort formatting the document.
  "comment_errors_fatal": false,
  // If there are blank lines beyond what genemichaels would add itself, keep up to this number
  // of them.
  "keep_max_blank_lines": 0
}
```

## Disabling formatting for specific comments

Since comments are assumed to be markdown they will be formatted per markdown rules. To disable this for certain comments, start the comment with `//.` like

```
//. fn main() {
//.    println!("hi");
//. }
```

## Disabling formatting for specific files

To skip specific files, in the first 5 lines of the source add a comment containing `nogenemichaels`, ex:

```rust
// nogenemichaels
...
```

# Programmatic usage

Do `cargo add genemichaels`

There are three main functions:

- `genemichaels::format_str` - formats a string (full rust source file, doesn't support snippets at the moment).
- `genemichaels::format_ast` - formats AST element (implements `genemichaels::Formattable`, most `syn::*` structs do). Comments need to be passed in separately, if you have any.
- `genemichaels::extract_comments` - takes a string of source code and extracts comments, mapping each comment to the start of a syntax element

If you want to format a `TokenStream`, parse it into an AST with `syn::parse2::<syn::File>(token_stream)` then call `format_ast`.

The format functions also return lost comments - comments not formatted/added to the formatted source after processing. In an ideal world this wouldn't exist, but right now comments are added on a case by case basis and not all source tokens support comments.

# How it works

At a very high level:

- The syntax tree is converted into "split groups" which have segments

  For instance, a split group for `match {}` might have segments `match` `{` `<break>` and `}`. These segments are interleaved (other group's segments may come between `{` and `}`) in a single line, to start.

  If the split group is triggered to split, `<break>` and everything after it on that line are moved to a new line after the line they were on.

- The split groups form a tree, with each node having children
- The segments form a linear list (well, list of lists), as they'd appear in the source code

Then the algorithm basically wraps nodes until all lines are less than the max line width.

## Multi-threading

By default Gene Michaels formats multiple files on all available cores, but this uses proportionally more memory. If you have a project with particularly large files you can restrict to a smaller number of cores in the configuration.

## Comments

Comments deserve a special mention since they're handled out of band.

`syn` doesn't parse comments (except sometimes) so all the comments are extracted at the start of processing. Generally comments are associated with the next syntax element, except for end of line `//` comments which get associated with the first syntax element on the current line.

When building split groups, if the current syntax element has a token with a line/column matching an extracted comment, the comment is added to the split group.

### Verbatim comments

Gene Michaels supports an extra comment type, `//.` which signals a verbatim comment, which isn't processed. Use these for commenting out source code.

## Macros

Macros are formatted with a couple tricks:

1. If it parses as rust code (either an expression or statement list), it's formatted normally.
2. If it doesn't, it's split by `;` and `,` since those are usually separators, then the above is tried for each chunk.
3. Otherwise each token in the macro is concatenated with spaces (with a couple other per-case tweaks)

## Q&A

See [this Reddit post](https://www.reddit.com/r/rust/comments/zo54gj/gene_michaels_alternative_rust_code_formatter/) for many questions and answers.
