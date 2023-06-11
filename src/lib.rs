use std::{
    env::args,
    process::exit,
    net::{
        SocketAddr,
        SocketAddrV4,
        SocketAddrV6,
        IpAddr,
        Ipv4Addr,
        Ipv6Addr,
    },
    path::PathBuf,
    io::{
        stdin,
        Read,
    },
    fs,
    collections::HashSet,
    hash::Hash,
    ffi::{
        OsString,
    },
};
pub use aargvark_proc_macros;
pub use anyhow::{
    Context,
    Error,
    Result,
};
use comfy_table::Cell;
pub use once_cell::sync::Lazy;

struct VarkErr {
    i: usize,
    breadcrumbs: Vec<String>,
    err: String,
}

#[doc(hidden)]
pub enum R<T> {
    EOF,
    Err,
    Ok(T),
    Help,
}

#[doc(hidden)]
pub enum PeekR<'a> {
    Ok(&'a str),
    None,
    Help,
}

#[doc(hidden)]
pub struct VarkState {
    args: Vec<String>,
    i: usize,
    pub breadcrumbs: Vec<String>,
    errors: Vec<VarkErr>,
    pub simple_enum_root: bool,
}

#[doc(hidden)]
pub fn join_strs(sep: &str, v: &[&str]) -> String {
    v.join(sep)
}

#[doc(hidden)]
pub fn generate_help_section_usage_prefix(state: &VarkState) -> (String, HashSet<String>) {
    let mut text = "Usage:".to_string();
    for s in &state.breadcrumbs {
        text.push_str(" ");
        text.push_str(&s);
    }
    return (text, HashSet::new());
}

#[doc(hidden)]
pub fn generate_help_section_suffix(
    docstr: &str,
    placeholders: Vec<&str>,
    placeholders_detail: Vec<(String, &str)>,
    joiner: &str,
) -> String {
    let mut out = String::new();
    for p in placeholders {
        out.push_str(joiner);
        out.push_str(p);
    }
    out.push_str("\n\n");
    if !docstr.is_empty() {
        out.push_str(docstr);
        out.push_str("\n\n");
    }
    let mut table = comfy_table::Table::new();
    table.load_preset(comfy_table::presets::NOTHING);
    table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);
    for (placeholder, docstr) in placeholders_detail {
        table.add_row(vec![comfy_table::Cell::new(placeholder), Cell::new(docstr)]);
    }
    table.set_constraints(vec![comfy_table::ColumnConstraint::UpperBoundary(comfy_table::Width::Percentage(60))]);
    out.push_str(&table.to_string());
    out.push_str("\n");
    out
}

impl VarkState {
    pub fn peek<'a>(&'a self) -> PeekR<'a> {
        if self.i >= self.args.len() {
            return PeekR::None;
        }
        let v = &self.args[self.i];
        if v == "-h" || v == "--help" {
            return PeekR::Help;
        }
        return PeekR::Ok(v);
    }

    pub fn position(&self) -> usize {
        return self.i;
    }

    pub fn rewind(&mut self, i: usize) {
        self.i = i;
    }

    pub fn consume(&mut self) {
        self.i += 1;
    }

    pub fn r_ok<T>(&self, v: T) -> R<T> {
        return R::Ok(v);
    }

    pub fn r_err<T>(&mut self, text: String) -> R<T> {
        self.errors.push(VarkErr {
            i: self.i,
            breadcrumbs: self.breadcrumbs.clone(),
            err: text,
        });
        return R::Err;
    }
}

/// Parse the explicitly passed in arguments. The `command` is only used in help
/// text.
pub fn vark_explicit<T: AargvarkTrait>(command: String, args: Vec<String>) -> T {
    let mut state = VarkState {
        args: args,
        i: 0,
        breadcrumbs: vec![command],
        errors: vec![],
        simple_enum_root: true,
    };
    match T::vark(&mut state) {
        R::EOF => {
            eprintln!("You must specify command line arguments, use --help for more info.");
            exit(1);
        },
        R::Err => {
            let display_args: Vec<String> = state.args.iter().map(|a| format!("{:?}", a)).collect();
            let mut display_arg_offsets = vec![];
            {
                let mut offset = 0;
                for d in &display_args {
                    display_arg_offsets.push(offset);
                    offset += d.chars().count() + 1;
                }
            }
            let display_args = display_args.join(" ");
            let mut text = "Error parsing command line arguments.\n".to_string();
            state.errors.reverse();
            for e in state.errors {
                text.push_str("\n");
                text.push_str(&format!(" * {}\n", e.err));
                text.push_str(&format!("while parsing {:?} at\n", e.breadcrumbs));
                text.push_str(&display_args);
                text.push_str("\n");
                text.push_str(&" ".repeat(*display_arg_offsets.get(e.i).unwrap()));
                text.push_str("^\n");
            }
            eprintln!("{}", text);
            exit(1);
        },
        R::Ok(v) => {
            if state.i != state.args.len() {
                eprintln!(
                    "Error parsing command line arguments: final arguments are unrecognized\n{:?}",
                    &state.args[state.i..]
                );
                exit(1);
            }
            return v;
        },
        R::Help => {
            let (mut text, mut seen_sections) = generate_help_section_usage_prefix(&state);
            T::generate_help_section_suffix(&mut text, &mut seen_sections);
            eprintln!("{}", text);
            exit(0);
        },
    }
}

/// Parse the command line arguments into the specified type.
pub fn vark<T: AargvarkTrait>() -> T {
    let mut args = args();
    let command = args.next().unwrap_or("unknown!".to_string());
    return vark_explicit(command, args.collect::<Vec<String>>());
}

/// Anything that implements this trait can be parsed and used as a field in other
/// parsable enums/structs.
pub trait AargvarkTrait: Sized {
    fn vark(state: &mut VarkState) -> R<Self>;
    fn generate_help_placeholder() -> String;
    fn generate_help_section(text: &mut String, seen_sections: &mut HashSet<String>);
    fn generate_help_section_suffix(text: &mut String, seen_sections: &mut HashSet<String>);
}

/// A helper enum, providing a simpler interface for types that can be parsed from
/// a single primitive string.
pub trait AargvarkFromStr: Sized {
    fn from_str(s: &str) -> Result<Self, String>;
    fn generate_help_placeholder() -> String;
}

impl<T: AargvarkFromStr> AargvarkTrait for T {
    fn vark(state: &mut VarkState) -> R<Self> {
        let s = match state.peek() {
            PeekR::None => return R::EOF,
            PeekR::Help => return R::Help,
            PeekR::Ok(s) => s,
        };
        match T::from_str(s) {
            Ok(v) => {
                state.consume();
                return state.r_ok(v);
            },
            Err(e) => return state.r_err(e),
        }
    }

    fn generate_help_placeholder() -> String {
        T::generate_help_placeholder()
    }

    fn generate_help_section(_text: &mut String, _seen_sections: &mut HashSet<String>) { }

    fn generate_help_section_suffix(_text: &mut String, _seen_sections: &mut HashSet<String>) { }
}

macro_rules! auto_from_str{
    ($t: ty) => {
        impl AargvarkFromStr for $t {
            fn from_str(s: &str) -> Result<Self, String> {
                <Self as std::str::FromStr>::from_str(s).map_err(|e| e.to_string())
            }

            fn generate_help_placeholder() -> String {
                format!(
                    "<{}>",
                    convert_case::Casing::to_case(&std::any::type_name::<Self>(), convert_case::Case::UpperKebab)
                )
            }
        }
    };
}

auto_from_str!(String);

auto_from_str!(OsString);

auto_from_str!(SocketAddr);

auto_from_str!(SocketAddrV4);

auto_from_str!(SocketAddrV6);

auto_from_str!(IpAddr);

auto_from_str!(Ipv4Addr);

auto_from_str!(Ipv6Addr);

auto_from_str!(PathBuf);

impl AargvarkTrait for bool {
    fn vark(state: &mut VarkState) -> R<Self> {
        return state.r_ok(true);
    }

    fn generate_help_placeholder() -> String {
        return "<BOOL>".to_string();
    }

    fn generate_help_section(_text: &mut String, _seen_sections: &mut HashSet<String>) { }

    fn generate_help_section_suffix(_text: &mut String, _seen_sections: &mut HashSet<String>) { }
}

/// This parses a path (or - for stdin) passed on the command line into bytes.
pub struct AargvarkFile(Vec<u8>);

impl AargvarkFromStr for AargvarkFile {
    fn from_str(s: &str) -> Result<Self, String> {
        if s == "-" {
            let mut out = vec![];
            match stdin().read_to_end(&mut out) {
                Ok(_) => return Ok(Self(out)),
                Err(e) => return Err(format!("Error reading stdin: {}", e)),
            };
        } else {
            match fs::read(s) {
                Ok(v) => return Ok(Self(v)),
                Err(e) => return Err(format!("Error reading {}: {}", s, e)),
            };
        }
    }

    fn generate_help_placeholder() -> String {
        format!("<PATH|->")
    }
}

/// This parses a path (or - for stdin) passed on the command line as json into the
/// specified type.
#[cfg(serde_json)]
pub struct AargvarkJson<T>(T);

#[cfg(serde_json)]
impl<T: Deserialize> AargvarkFromStr for AargvarkJson<T> {
    fn from_str(s: &str) -> Result<Self, String> {
        let b = AargvarkFile::from_str(s)?;
        match serde_json::from_str(b) {
            Ok(v) => return Ok(Self(v)),
            Err(e) => return Err(e.to_string()),
        };
    }

    fn generate_help_placeholder() -> String {
        format!("<PATH|->")
    }
}

/// This parses a path (or - for stdin) passed on the command line as yaml into the
/// specified type.
#[cfg(serde_yaml)]
pub struct AargvarkYaml<T>(T);

#[cfg(serde_yaml)]
impl<T: Deserialize> AargvarkFromStr for AargvarkYaml<T> {
    fn from_str(s: &str) -> Result<Self, String> {
        let b = AargvarkFile::from_str(s)?;
        match serde_yaml::from_str(b) {
            Ok(v) => return Ok(Self(v)),
            Err(e) => return Err(e.to_string()),
        };
    }

    fn generate_help_placeholder() -> String {
        format!("<PATH|->")
    }
}

#[doc(hidden)]
pub fn vark_from_iter<T: AargvarkTrait, C: FromIterator<T>>(state: &mut VarkState) -> R<C> {
    state.simple_enum_root = false;
    let mut out = vec![];
    let mut rewind_to = state.position();
    let mut i = 0usize;
    loop {
        i += 1;
        state.breadcrumbs.push(format!("[{}]", i));
        let r = T::vark(state);
        state.breadcrumbs.pop();
        match r {
            R::Help => {
                return R::Help;
            },
            R::Ok(v) => {
                out.push(v);
                rewind_to = state.position();
            },
            R::Err | R::EOF => {
                state.rewind(rewind_to);
                return state.r_ok(C::from_iter(out.into_iter()));
            },
        };
    }
}

impl<T: AargvarkTrait> AargvarkTrait for Vec<T> {
    fn vark(state: &mut VarkState) -> R<Self> {
        return vark_from_iter(state);
    }

    fn generate_help_placeholder() -> String {
        format!("{}[ ...]", T::generate_help_placeholder())
    }

    fn generate_help_section(text: &mut String, seen_sections: &mut HashSet<String>) {
        Self::generate_help_section_suffix(text, seen_sections);
    }

    fn generate_help_section_suffix(text: &mut String, seen_sections: &mut HashSet<String>) {
        T::generate_help_section(text, seen_sections);
    }
}

impl<T: AargvarkTrait + Eq + Hash> AargvarkTrait for HashSet<T> {
    fn vark(state: &mut VarkState) -> R<Self> {
        return vark_from_iter(state);
    }

    fn generate_help_placeholder() -> String {
        format!("{}[ ...]", T::generate_help_placeholder())
    }

    fn generate_help_section(text: &mut String, seen_sections: &mut HashSet<String>) {
        Self::generate_help_section_suffix(text, seen_sections);
    }

    fn generate_help_section_suffix(text: &mut String, seen_sections: &mut HashSet<String>) {
        T::generate_help_section(text, seen_sections);
    }
}
