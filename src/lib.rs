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
pub use aargvark_proc_macros::Aargvark;
use comfy_table::Cell;

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

pub fn style_lit(l: &str) -> String {
    console::Style::new().bold().apply_to(l).to_string()
}

pub fn style_name(l: &str) -> String {
    l.to_string()
}

#[doc(hidden)]
pub fn generate_help_section_usage_prefix(state: &VarkState) -> (String, HashSet<String>) {
    let mut text = "Usage: ".to_string();
    for (i, s) in state.breadcrumbs.iter().enumerate() {
        if i > 0 {
            text.push_str(" ");
        }
        text.push_str(&style_lit(s));
    }
    return (text, HashSet::new());
}

#[doc(hidden)]
pub fn generate_help_section_suffix(
    docstr: &str,
    placeholders: Vec<&str>,
    placeholders_detail: Vec<(&str, &str)>,
    joiner: &str,
) -> String {
    let mut out = String::new();
    out.push_str(" ");
    for (i, p) in placeholders.iter().enumerate() {
        if i > 0 {
            out.push_str(joiner);
        }
        out.push_str(p);
    }
    out.push_str("\n\n");
    if !docstr.is_empty() {
        out.push_str(docstr);
        out.push_str("\n");
    }
    let mut table = comfy_table::Table::new();
    table.load_preset(comfy_table::presets::NOTHING);
    table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);
    for (placeholder, docstr) in placeholders_detail {
        table.add_row(vec![comfy_table::Cell::new(placeholder), Cell::new(docstr)]);
    }
    table.set_constraints(vec![comfy_table::ColumnConstraint::Boundaries {
        lower: comfy_table::Width::Percentage(20),
        upper: comfy_table::Width::Percentage(60),
    }]);
    out.push_str(&table.to_string());
    out.push_str("\n\n");
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
                text.push_str(&format!("  while parsing {:?} at\n", e.breadcrumbs));
                text.push_str("   ");
                text.push_str(&display_args);
                text.push_str("\n");
                text.push_str("   ");
                text.push_str(
                    &" ".repeat(
                        display_arg_offsets
                            .get(e.i)
                            .cloned()
                            .or_else(|| display_arg_offsets.last().cloned())
                            .unwrap_or(0usize),
                    ),
                );
                text.push_str("^\n");
            }
            eprintln!("{}\n", text);
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
            eprintln!("{}\n", text.trim());
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
    ($placeholder: literal, $t: ty) => {
        impl AargvarkFromStr for $t {
            fn from_str(s: &str) -> Result<Self, String> {
                <Self as std::str::FromStr>::from_str(s).map_err(|e| e.to_string())
            }

            fn generate_help_placeholder() -> String {
                format!("<{}>", style_lit($placeholder))
            }
        }
    };
}

auto_from_str!("STRING", String);

auto_from_str!("INT", u8);

auto_from_str!("INT", u16);

auto_from_str!("INT", u32);

auto_from_str!("INT", u64);

auto_from_str!("INT", i8);

auto_from_str!("INT", i16);

auto_from_str!("INT", i32);

auto_from_str!("INT", i64);

auto_from_str!("INT", f32);

auto_from_str!("NUM", f64);

auto_from_str!("STRING", OsString);

auto_from_str!("SOCKET", SocketAddr);

auto_from_str!("SOCKETV4", SocketAddrV4);

auto_from_str!("SOCKETV6", SocketAddrV6);

auto_from_str!("IP", IpAddr);

auto_from_str!("IPV4", Ipv4Addr);

auto_from_str!("IPV6", Ipv6Addr);

auto_from_str!("PATH", PathBuf);

#[cfg(feature = "http_types")]
auto_from_str!("URI", http::Uri);

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
        format!("<{}>|{}", style_lit("PATH"), style_lit("-"))
    }
}

/// This parses a path (or - for stdin) passed on the command line as json into the
/// specified type.
#[cfg(feature = "serde_json")]
pub struct AargvarkJson<T>(pub T);

#[cfg(feature = "serde_json")]
impl<T: for<'a> serde::Deserialize<'a>> AargvarkFromStr for AargvarkJson<T> {
    fn from_str(s: &str) -> Result<Self, String> {
        let b = AargvarkFile::from_str(s)?;
        match serde_json::from_slice(&b.0) {
            Ok(v) => return Ok(Self(v)),
            Err(e) => return Err(e.to_string()),
        };
    }

    fn generate_help_placeholder() -> String {
        format!("<{}>|{}", style_lit("PATH"), style_lit("-"))
    }
}

#[cfg(feature = "serde_json")]
impl<T: Clone> Clone for AargvarkJson<T> {
    fn clone(&self) -> Self {
        AargvarkJson(self.0.clone())
    }
}

/// This parses a path (or - for stdin) passed on the command line as yaml into the
/// specified type.
#[cfg(feature = "serde_yaml")]
pub struct AargvarkYaml<T>(pub T);

#[cfg(feature = "serde_yaml")]
impl<T: for<'a> serde::Deserialize<'a>> AargvarkFromStr for AargvarkYaml<T> {
    fn from_str(s: &str) -> Result<Self, String> {
        let b = AargvarkFile::from_str(s)?;
        match serde_yaml::from_slice(&b.0) {
            Ok(v) => return Ok(Self(v)),
            Err(e) => return Err(e.to_string()),
        };
    }

    fn generate_help_placeholder() -> String {
        format!("<{}>|{}", style_lit("PATH"), style_lit("-"))
    }
}

#[cfg(feature = "serde_yaml")]
impl<T: Clone> Clone for AargvarkYaml<T> {
    fn clone(&self) -> Self {
        AargvarkYaml(self.0.clone())
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
