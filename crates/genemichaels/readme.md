# Gene Michaels

Status: **Delta** (the one after gamma). I've been using it for years without major issue, and I've tested against various code bases and doesn't blow them up. I think other people might use it too! Right now post-formatting pre-writing it re-parses and confirms all comments are consumed as safety checks. Also files over 500kb may take all your memory and invoke the OOM killer.

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

Gene Michaels can be configured with a configuration file named `.genemichaels.json` in the current directory or any parent directory, or named `genemichaels.json` in your user configuration directory (ex: `~/.config/genemichaels.json`). If for some reason it doesn't find your config file you can double check it by running `genemichaels` with `strace`.

The configuration file is json, but it will strip lines starting with `//` first if you want to add comments.

Here is the default config - all values shown are defaults and can be omitted.

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
  // Genemichaels will replace line breaks with it's own deterministic line breaks.  You can
  // use this to keep extra line breaks (1 will keep up to 1 extra line break) during comment
  // extraction. This is unused during formatting.
  "keep_max_blank_lines": 0,
  // How much to indent at each split block. When using each split block is indented by one tab
  // but this value is used as the tab width for width/wrapping calculations.
  "indent_spaces": 4,
  // `"tabs"` or `"spaces"`. Write indents using spaces or tabs.
  "indent_unit": "spaces",
  // `//` (plain line-comments) won't be treated implicitly as markdown. In this case you can
  // use `//?` for explicitly markdown-formatted line-comments (these comments will work
  // regardless of the setting)
  "explicit_markdown_comments": false
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

# How it works

At a very high level:

- The syntax tree is converted into a linear list of segments, where each segment is a member of exactly one "split group". A split group has a single boolean switch of state: split or not split.

  For instance, a split group for `match {}` might have segments `match` `{` `<break>` and `}`. These segments are interleaved with other groups' segments, for instance other segments may come between `{` and `}`.

  All split groups start in the not split state.

  When a split group split state is toggled, `<break>` and everything after it on that line are moved to a new line after the line they were on.

The algorithm basically wraps nodes until all lines are less than the max line width.

This was a simplified explanation; there are a few other factors:

- Alignments
- Segments that change depending on whether their group is split or not (i.e. the `<break>` above which only breaks the line when the group is split, vs unconditional breaks)

## Multi-threading

By default Gene Michaels formats multiple files on all available cores, but this uses proportionally more memory. If you have a project with particularly large files you can restrict to a smaller number of cores in the configuration.

## Comments

Comments deserve a special mention since they're handled out of band.

`syn` doesn't parse comments (except sometimes) so all the comments are extracted at the start of processing. Generally comments are associated with the next syntax element, except for end of line `//` comments which get associated with the first syntax element on the current line.

When building split groups, if the current syntax element has a token with a line/column matching an extracted comment, the comment is added to the split group.

## Macros

Macros are formatted with a couple tricks:

1. If it parses as rust code (either an expression or statement list), it's formatted normally.
2. If it doesn't, it's split by `;` and `,` since those are usually separators, then the above is tried for each chunk.
3. Otherwise each token in the macro is concatenated with spaces (with a couple other per-case tweaks)

Formatting end user use of macros is prioritized over formatting `macro_rules`, since macros are used more than they're defined. Most macros look like normalish Rust syntax so many of the normal formatting rules can be used.

## Q&A

See [this Reddit post](https://www.reddit.com/r/rust/comments/zo54gj/gene_michaels_alternative_rust_code_formatter/) for many questions and answers.

For other questions or bug reports, etc. please use issues and/or discussions here.
