- formats everything
- doesn't not format some things
- this is a haiku

Status: Alpha. There's some risk of corrupting source code (mostly when dealing with rare syntax elements, comments in odd places). Some corruption will be caught and you'll be alerted with no changes to the file. But commit your code before running if possible.

Named after Gene Michaels.

# Usage

Run `cargo install genemichaels`

If you're using vs code, add the setting:

```
  "rust-analyzer.rustfmt.overrideCommand": [
    "${userHome}/.cargo/bin/genemichaels"
  ]
```

to use it with reckless abandon.

# How it works

At a very high level:

- The syntax tree is converted into "split groups" which have segments.
- The split groups form a tree, with each node having children.
- The segments form a linear list: they are collected in parallel to the tree in a single "line" list (within a list of lines, initially with just that one line).
- While any line goes past the desired length, split groups are split.

For instance, a split group for `match {}` might have segments `match` `{` `newline-if-split` and `}`. These segments are interleaved (other group's segments may come between `{` and `}`) in a single line, to start.

If the split group is triggered to split, `newline-if-split` and everything after it on that line are moved to a new line after the line they were on.

### Comments

Comments deserve a special mention since they're handled out of band.

syn` doesn't parse comments (except sometimes) so all the comments are extracted at the start of processing.

When building split groups, if the current syntax element has a token with a line/column matching an extracted comment, the comment is added to the split group.

Any unused comments cause an error to be raised.

### Verification

At the moment, the formatted code is run through the parser again. This doesn't catch everything (like some omitted elements), but prevents some totally broken output.
