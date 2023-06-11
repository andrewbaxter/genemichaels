Self-similar derive-based argument parsing, similar to Clap-derive.

This attempts to implement a simple, self-similar structure for parsing arbitrarily complex command line arguments. Like with Serde, you can combine structs, vecs, enums in any way you want.

# Why or why not

Why this and not Clap?

- This parse more complex data types, like vectors of sub-structures, or enums
- It's more consistent
- It has a super-simple interface (just `#[derive(Aargvark)]`)

Why not this?

- Some command line parsing conventions were discarded in order to simplify and maintain self-similarity. A lot of command line conventions are inconsistent or break down as you nest things.
- There's less customizability. Some things (like `-v` `-vv` `-vvv`) break patterns and probably won't ever be implemented. Other things just haven't been implemented yet due to lack of time.
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
   let args = MyArgs::vark();
   ```

You can derive structs, enums, and tuples, as well as `Vec`, `HashSet`, most `Ip` and `SocketAddr` types, and `PathBuf`.

There are also custom structs for reading files:

- `VarkFile<T>`
- `VarkJson<T>`
- `VarkYaml<T>`

For JSON and Yaml you must enable the respective features.
