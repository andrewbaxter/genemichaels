Self-similar derive-based command line argument parsing, in the same genre as Clap-derive. It supports

- Command line parsing
- Help

This attempts to support parsing arbitrarily complex command line arguments. Like with Serde, you can combine structs, vecs, enums in any way you want. Just because you can doesn't mean you should.

```
$ # This is an example help output, sans light ansi styling
$ spagh set -h
Usage: spagh set > IDENTITY DATA

    IDENTITY: BACKED-IDENTITY-ARG       Identity to publish as
    DATA: <PATH> | -                    Data to publish.  Must be json in the structure `{KEY: {"ttl": MINUTES, "value": DATA}, ...}`

BACKED-IDENTITY-ARG: local | card

    An identity with its associated secret.

    local <PATH>                        A file containing a generated key
    card card                           PC/SC card with ED25519 key

card: PCSC-ID PIN

    PCSC-ID: <STRING>                   Card to register, using id per pcscd (not identity id)
    PIN: <STRING>                       Card pin

$
```

# Why or why not

Why this and not Clap?

- This parses more complex data types, like vectors of sub-structures, or enums
- It's more consistent
- It has a super-simple interface (just `#[derive(Aargvark)]`)

Why not this?

- Some command line parsing conventions were discarded in order to simplify and maintain self-similarity. A lot of command line conventions are inconsistent or break down as you nest things, after all.
- Quirky CLI parsing generally isn't supported: Some tricks (like `-v` `-vv` `-vvv`) break patterns and probably won't ever be implemented. (Other things just haven't been implemented yet due to lack of time)
- Alpha

# Conventions and usage

To add it to your project, run

```sh
cargo add aargvark
```

To parse command line arguments

1. Define the data type you want to parse them into, like

   ```rust
   #[derive(Aargvark)]
   struct MyArgs {
     velociraptor: String,
     deadly: bool,
     color_pattern: Option<ColorPattern>,
   }
   ```

2. Vark it
   ```
   let args = aargvark::vark::<MyArgs>();
   ```

Optional fields in structs become optional (`--long`) arguments. If you want a `bool` long option that's enabled if the flag is specified (i.e. doesn't take a value), use `Option<()>`.

You can derive structs, enums, and tuples, and there are implementations for `Vec`, `HashSet`, most `Ip` and `SocketAddr` types, and `PathBuf` provided.

Some additional wrappers are provided for automatically loading (and parsing) files:

- `AargvarkFile<T>`
- `AargvarkJson<T>` requires feature `serde_json`
- `AargvarkYaml<T>` requires feature `serde_yaml`

To parse your own types, implement `AargvarkTrait`, or if your type takes a single string argument you can implement `AargvarkFromStr`.

# Advanced usage

- Vecs

  Vec elements are space separated. The way vec parsing works is it attempts to parse as many elements as possible. When parsing one element fails, it rewinds to after it parsed the last successful element and proceeds from the next field after the vec.

- Prevent recursion in help

  Add `#[vark(break)]` to a type, field, or variant to prevent recursing into any of the children. This is useful for subcommand enums - attach this to the enum and it will list the arguments but not the arguments' arguments (unless you do `-h` after specifying one on the command line).

- Rename enum variants and option keys

  Add `#[vark(name="x")]` to a field.

- Change placeholder (id) string

  Add `#[vark(id="x")]` to a field.
