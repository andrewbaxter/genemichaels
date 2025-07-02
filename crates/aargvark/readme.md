<table align="right" margin="1em"><tr>
<td><a href="https://crates.io/crates/aargvark"><img alt="crates.io" src="https://img.shields.io/crates/v/aargvark"></a></td>
<td><a href="https://docs.rs/aargvark"><img alt="docs.rs" src="https://img.shields.io/docsrs/aargvark"></td></a>
</tr></table>

Simple, consistent, and flexible derive-based command line argument parsing, in the same genre as Clap-derive. It currently supports

- Command line parsing
- Help
- Shell completion

Minimizing keypressing, bespoke CLI behaviors, and optimizing for powerusers aren't goals.

Aargvark supports parsing arbitrarily complex command line arguments: like with Serde, you can combine structs, vecs, enums in any way you want. Just because you can doesn't mean you should.

A definition like:

```rust no_run
use aargvark::Aargvark;
use std::path::PathBuf;

#[derive(Aargvark)]
struct RestartArgs {
    message: String,
}

#[derive(Aargvark)]
struct StopArgs {
    message: String,
    force: bool,
}

#[derive(Aargvark)]
#[vark(break_help)]
enum Command {
    Start,
    Restart(RestartArgs),
    Stop(StopArgs),
}

#[derive(Aargvark)]
struct Args {
    debug: Option<()>,
    config: Option<PathBuf>,
    server_name: String,
    command: Command,
}

let args = aargvark::vark::<Args>();
```

Produces this output:

```text
$ ; (The real thing is colored too)
$ ultrathon -h
Usage: ultrathon SERVER-NAME COMMAND [ ...FLAGS]

    This is an example of a command, with explanations taken from docstrings.

    SERVER-NAME: <STRING>               Name of server in config to run command on
    COMMAND: COMMAND
    [--debug]                           Enable verbose log output
    [--config <PATH>]                   Path to servers config JSON

COMMAND: start | restart | stop

    start ...
    restart ...
    stop ...

```

```text
$ ultrathon bootleg-server stop -h
Usage: ultrathon bootleg-server stop MESSAGE FORCE

    Stop all tasks, then stop the server.

    MESSAGE: <STRING>                   Message to send users before stopping.
    [--force]                           Force the server to stop even if there are pending tasks.

```

# Why or why not

Why this and not Clap?

- It has a super-simple interface (just `#[derive(Aargvark)]` on any enum/structure)
- This parses more complex data types, like vectors of sub-structures or enums
- It's more consistent

Why not this?

- It's newer, with fewer features and limited community ecosystem, extensions
- Some command line parsing conventions were discarded in order to simplify and maintain self-similarity. A lot of command line conventions are inconsistent or break down as you nest things, after all.
- Quirky CLI parsing generally isn't supported.

# Conventions and usage

To add it to your project, run

```text
cargo add aargvark
```

To parse command line arguments

```rust no_run
use aargvark::{Aargvark, vark};

// 1. Define the data type you want to parse them into, like

#[derive(Aargvark)]
struct ColorPattern; // ...

/// This command changes the color of a velociraptor, deadly
/// or not. This text is shown in help output.
#[derive(Aargvark)]
struct MyArgs {
  /// Field documentation, also included in help output.
  velociraptor: String,
  #[vark(flag = "-d", flag = "--deadly")]
  deadly: bool,
  color_pattern: Option<ColorPattern>,
}

// 2. Vark it

let args = vark::<MyArgs>();
```

Non-optional fields become positional arguments unless you give them a flag with `#[vark(flag = "--flag")]`. Optional fields become optional (`--long`) arguments. If you want a `bool` flag that's enabled if the flag is specified (i.e. doesn't take a value), use `Option<()>`.

You can derive structs, enums, and tuples, and there are implementations for `Vec`, `HashSet`, `Map` with `FromString` keys and values as `K=V` arguments, most `Ip` and `SocketAddr` types, and `PathBuf` built in.

Some additional wrappers are provided for automatically loading (and parsing) files:

- `AargvarkFile<T>`
- `AargvarkJson<T>` requires feature `serde_json`
- `AargvarkYaml<T>` requires feature `serde_yaml`

To parse your own types, implement `AargvarkTrait`, or if your type takes a single string argument you can implement `AargvarkFromStr` which is slightly simpler.

# Advanced usage

## Sequences, plural fields, Vecs

For `Vec` and `HashSet`, etc.

Sequence elements are space separated (not a single argument with commas).

The way sequence parsing works is it attempts to parse as many elements as possible. When parsing one element fails, it rewinds to after it parsed the last successful element and proceeds from the next field after the sequence.

If you have something like `Vec<String>` it will consume all arguments to the end of the command line. If this is not intentional, to avoid this you can use something like `Vec<NonFlag>` (`NonFlag` only accepts strings that don't start with `-`) or specify some other type that is unambiguous, or move the `Vec` argument to the end of your command line.

## Force using flags, replace flags, and add additional flags

Add ex: `#[vark(flag="--target-machine", flag="-tm")]` to a _field_.

If the field was positional, this will change it to be non-positional and require a flag (`--target-machine`, `-tm`).

If the field was optional, this will replace the default flag.

## Rename enum variant keys

Add ex: `#[vark(name="my-variant")]` to a _variant_.

This changes the command line tag used to select a variant.

## Prevent recursion in help

Add `#[vark(break_help)]` to a _type_, _field_, or _variant_ to prevent recursing into any of the children when displaying help.

This is useful for subcommand enums - attach this to the enum and it will list the variants and their tags but not the variants' arguments (the per-variant/struct help will be displayed if the user specifies `-h` _after_ the variant tag or struct flag - placing the parser in the context of that type).

## Change the help placeholder string

Add `#[vark(placeholder="TARGET-MACHINE")]` to a _type_, _field_, or _variant_.

This is the capitalized text (like XYZ) after an option that basically means "see XYZ section for more details"

## Shell completions

### Using

The `vark()` method will output completions when:

- The environment variable `VARK_COMPLETE` is set with the value `empty` or `partial`

  `empty` means a new (empty) argument is being started, and `partial` means that the last argument hasn't been completed yet. The reason this must be passed in externally is because the space at the end will be stripped by the time the program is executed.

- The command line with one discarded initial argument is provided

  The discarded initial argument allows you to pass `COMP_LINE` in bash verbatim (which contains the program argument). This is probaly easier than parsing, removing the argument, then re-quoting and splatting in bash.

  If your shell doesn't include the program name, you can just add `""` or something to pad the arguments.

One completion is printed per line on stdout. This is the output format `bash` expects.

The completions are always relative to the end of the command line - internal completion isn't supported and you'll get line-end completions if you attempt it which could be confusing.

In order to use these autocompletions, you need to configure your shell to invoke it.

For bash, something like:

```shell
_my_program () {
  # Only invoke if the cursor is at the end of the line
  if [[ $COMP_POINT == ${#COMP_LINE} ]]; then

    # Determine if a new argument is being started, or a partially written argument
    # is being finished
    local vark_complete_type
    if [[ "$COMP_LINE" == *" " ]]; then
        vark_complete_type=empty
    else
        vark_complete_type=partial
    fi

    # Call the program to generate and output completion options
    AARGVARK_COMPLETE=$vark_complete_type my_program $COMP_LINE
  fi
}
complete -C _my_program my_program
```

### Custom completions

Any type implementing `AargvarkTrait` has the option of replacing the latest completer when calling `state.r_ok` or `state.r_err`, which will be used when parsing completes if that's still the latest completer.

When implementing `AargvarkFromStr` you can additionally define the function `build_completer` to provide the same.

When the completer is invoked, it should produce a list of completion options. Each element is also a list, of command line options. Aargvark will quote the list to turn it into a shell-safe command line upon output.
