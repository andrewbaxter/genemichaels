Finds formatter bugs by formatting a corpus of Rust code and checking invariants that must hold for any valid input:

- Formatting doesn't panic, error, crash, or hang
- No comments are reported as lost
- The output is valid Rust
- The output has the same tokens as the input, ignoring intentional normalizations outside of macro bodies (`vec!` counts as normal code): trailing commas/`+`, commas after block match arms, `dyn` on bare trait objects, `...` to `..=`, empty `<>` and bounds. With import or declaration normalization enabled (which merge and reorder items) only the set of names and literals is compared.
- The output has the same comment words as the input (comments are reflowed so only words are compared)
- Injected comments stay near where they were: they may only move forward past separators (`,`, `+`, `|`), and backward to the start of their statement (or line, for line-end comments) without leaving their block or crossing a statement boundary
- No lines in the output end with whitespace (if none in the input do)
- Formatting the output again doesn't change it

In addition to each file as-is, it checks variants with comments (`/* injected */`, `// injected`, `/// injected`, etc.) inserted at random token boundaries, which finds comment-placement bugs.

Failures are bucketed by kind and a coarse signature, and the smallest few cases in each bucket are automatically minimized.

# Usage

Get a corpus, for example the rustc UI tests (clone once to somewhere persistent - runs only read it):

```
git clone --depth 1 --filter=blob:none --sparse https://github.com/rust-lang/rust ~/.cache/genemichaels-fuzz/rust
cd ~/.cache/genemichaels-fuzz/rust && git sparse-checkout set tests/ui tests/pretty
```

or your local crate cache at `~/.cargo/registry/src`.

Run it (flags must come before the corpus paths):

```
cargo run --release -p genemichaels-fuzz -- run --inject 10 OUT_DIR path/to/corpus...
```

`OUT_DIR/summary.md` lists the buckets. Each bucket directory has `cases.txt` (every failing file and injection), and for the smallest cases `case-N.rs` (the failing input), `case-N.json` (failure details), and `case-N.min.rs`/`case-N.min.json` (the minimized input, and its failure including the formatter output).

Injection points are chosen deterministically per file; use `--seed N` to test different ones.

To check or minimize individual files:

```
cargo run --release -p genemichaels-fuzz -- check --minimize file.rs
```

Use `--config` with a `.genemichaels.json` to test non-default configurations. `configs/` has some that exercise different code paths (narrow widths, splitting disabled, tabs and explicit markdown comments, import/declaration normalization).
