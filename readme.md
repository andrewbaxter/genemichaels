# Gene Michaels

Status: **Alpha**. Tested against various code bases and doesn't blow them up, but there could still be some missed things. Right now post-formatting pre-writing it re-parses and confirms all comments are consumed as safety checks. Also files over 500kb may take all your memory and invoke the OOM killer.

- formats everything
- doesn't not format some things
- this is a haiku

Named after Gene Michaels.

Everything includes macros and comments. Dog fooded in this repo.

### Differences to Rustfmt

* This formats all macros, Rustfmt only formats macros under certain conditions
* This is fully deterministic, Rustfmt keeps certain stylistic choices like
* Rustfmt has [several](https://github.com/rust-lang/rustfmt/issues/3863) [restrictions](https://github.com/rust-lang/rustfmt/issues/2896) in what it formats normally, this always formats everything (if it doesn't it's a bug)
* This also reformats comments per Markdown rules

# Usage

Run `cargo install genemichaels`

If you're using VS Code, add the setting:

```
  "rust-analyzer.rustfmt.overrideCommand": [
    "${userHome}/.cargo/bin/genemichaels"
  ]
```

to use it with reckless abandon.

# Programmatic usage

Do `cargo add genemichaels`

There are three main functions:

- `genemichaels::format_str` - formats a string (full rust source file, doesn't support snippets at the moment).
- `genemichaels::format_ast` - formats AST element (implements `genemichaels::Formattable`, most `syn::*` structs do). Comments need to be passed in separately, if you have any.
- `genemichaels::extract_comments` - takes a string of source code and extracts comments, mapping each comment to the start of a syntax element

The format functions also return lost comments - comments not formatted/added to the formatted source after processing. In an ideal world this wouldn't exist, but right now comments are added on a case by case basis and not all source tokens support comments.

# How it works

At a very high level:

- The syntax tree is converted into "split groups" which have segments

  For instance, a split group for `match {}` might have segments `match` `{` `<break>` and `}`. These segments are interleaved (other group's segments may come between `{` and `}`) in a single line, to start.

  If the split group is triggered to split, `<break>` and everything after it on that line are moved to a new line after the line they were on.

- The split groups form a tree, with each node having children
- The segments form a linear list (well, list of lists), as they'd appear in the source code

Then the algorithm basically wraps nodes until all lines are less than the max line width.

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
