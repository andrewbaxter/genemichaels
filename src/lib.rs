#![doc= include_str !("../readme.md")]

use std::{
    any::TypeId,
    cell::RefCell,
    collections::{
        HashMap,
        HashSet,
    },
    env::args,
    ffi::{
        OsString,
    },
    fs,
    hash::Hash,
    io::{
        stdin,
        Read,
    },
    net::{
        SocketAddr,
        SocketAddrV4,
        SocketAddrV6,
        IpAddr,
        Ipv4Addr,
        Ipv6Addr,
    },
    path::PathBuf,
    process::exit,
    rc::Rc,
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
                display_arg_offsets.push(offset);
            }
            let mut display_args = display_args.join(" ");
            display_args.push_str(" <END>");
            let mut text = "Error parsing command line arguments.\n".to_string();
            state.errors.reverse();
            for e in state.errors {
                text.push_str("\n");
                text.push_str(&format!(" * {}\n", e.err));
                text.push_str(&format!("   while processing command {:?} at\n", e.breadcrumbs));
                text.push_str("   ");
                text.push_str(&display_args);
                text.push_str("\n");
                text.push_str("   ");
                text.push_str(&" ".repeat(display_arg_offsets.get(e.i).cloned().unwrap_or(0usize)));
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
    fn build_help_pattern(state: &mut HelpState) -> HelpPattern;
}

/// A helper enum, providing a simpler interface for types that can be parsed from
/// a single primitive string.
pub trait AargvarkFromStr: Sized {
    fn from_str(s: &str) -> Result<Self, String>;
    fn build_help_pattern(state: &mut HelpState) -> HelpPattern;
}

impl<T: AargvarkFromStr> AargvarkTrait for T {
    fn vark(state: &mut VarkState) -> R<Self> {
        let s = match state.peek() {
            PeekR::None => return R::EOF,
            PeekR::Help => {
                show_help_and_exit(state, |state| {
                    return HelpPartialProduction {
                        description: "".to_string(),
                        content: HelpPartialContent::Pattern(<Self as AargvarkTrait>::build_help_pattern(state)),
                    };
                });
            },
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

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return <T as AargvarkFromStr>::build_help_pattern(state);
    }
}

macro_rules! auto_from_str{
    ($placeholder: literal, $t: ty) => {
        impl AargvarkFromStr for $t {
            fn from_str(s: &str) -> Result<Self, String> {
                return <Self as std::str::FromStr>::from_str(s).map_err(|e| e.to_string());
            }

            fn build_help_pattern(_state: &mut HelpState) -> HelpPattern {
                return HelpPattern(vec![HelpPatternElement:: Type($placeholder.to_string())]);
            }
        }
    };
}

auto_from_str!("STRING", String);

auto_from_str!("INT", u8);

auto_from_str!("INT", u16);

auto_from_str!("INT", u32);

auto_from_str!("INT", u64);

auto_from_str!("INT", usize);

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

impl AargvarkFromStr for bool {
    fn from_str(s: &str) -> Result<Self, String> {
        return <Self as std::str::FromStr>::from_str(s).map_err(|e| e.to_string());
    }

    fn build_help_pattern(_state: &mut HelpState) -> HelpPattern {
        return HelpPattern(
            vec![
                HelpPatternElement::Variant(
                    vec![
                        HelpPattern(vec![HelpPatternElement::Literal("true".to_string())]),
                        HelpPattern(vec![HelpPatternElement::Literal("false".to_string())])
                    ],
                )
            ],
        );
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Source {
    Stdin,
    File(PathBuf),
}

/// This parses a path (or - for stdin) passed on the command line into bytes.
pub struct AargvarkFile {
    pub value: Vec<u8>,
    pub source: Source,
}

impl AargvarkFromStr for AargvarkFile {
    fn from_str(s: &str) -> Result<Self, String> {
        if s == "-" {
            let mut out = vec![];
            match stdin().read_to_end(&mut out) {
                Ok(_) => return Ok(Self {
                    value: out,
                    source: Source::Stdin,
                }),
                Err(e) => return Err(format!("Error reading stdin: {}", e)),
            };
        } else {
            match fs::read(s) {
                Ok(v) => return Ok(Self {
                    value: v,
                    source: Source::File(PathBuf::from(s)),
                }),
                Err(e) => return Err(format!("Error reading {}: {}", s, e)),
            };
        }
    }

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return HelpPattern(
            vec![
                HelpPatternElement::Variant(
                    vec![
                        <PathBuf as AargvarkTrait>::build_help_pattern(state),
                        HelpPattern(vec![HelpPatternElement::Literal("-".to_string())])
                    ],
                )
            ],
        );
    }
}

/// This parses a path (or - for stdin) passed on the command line as json into the
/// specified type.
#[cfg(feature = "serde_json")]
pub struct AargvarkJson<T> {
    pub value: T,
    pub source: Source,
}

#[cfg(feature = "serde_json")]
impl<T: for<'a> serde::Deserialize<'a>> AargvarkFromStr for AargvarkJson<T> {
    fn from_str(s: &str) -> Result<Self, String> {
        let b = AargvarkFile::from_str(s)?;
        match serde_json::from_slice(&b.value) {
            Ok(v) => return Ok(Self {
                value: v,
                source: b.source,
            }),
            Err(e) => return Err(e.to_string()),
        };
    }

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return HelpPattern(
            vec![
                HelpPatternElement::Variant(
                    vec![
                        <PathBuf as AargvarkTrait>::build_help_pattern(state),
                        HelpPattern(vec![HelpPatternElement::Literal("-".to_string())])
                    ],
                )
            ],
        );
    }
}

#[cfg(feature = "serde_json")]
impl<T: Clone> Clone for AargvarkJson<T> {
    fn clone(&self) -> Self {
        return AargvarkJson {
            value: self.value.clone(),
            source: self.source.clone(),
        };
    }
}

/// This parses a path (or - for stdin) passed on the command line as yaml into the
/// specified type.
#[cfg(feature = "serde_yaml")]
pub struct AargvarkYaml<T> {
    pub value: T,
    pub source: Source,
}

#[cfg(feature = "serde_yaml")]
impl<T: for<'a> serde::Deserialize<'a>> AargvarkFromStr for AargvarkYaml<T> {
    fn from_str(s: &str) -> Result<Self, String> {
        let b = AargvarkFile::from_str(s)?;
        match serde_yaml::from_slice(&b.value) {
            Ok(v) => return Ok(Self {
                value: v,
                source: b.source,
            }),
            Err(e) => return Err(e.to_string()),
        };
    }

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return HelpPattern(
            vec![
                HelpPatternElement::Variant(
                    vec![
                        <PathBuf as AargvarkTrait>::build_help_pattern(state),
                        HelpPattern(vec![HelpPatternElement::Literal("-".to_string())])
                    ],
                )
            ],
        );
    }
}

#[cfg(feature = "serde_yaml")]
impl<T: Clone> Clone for AargvarkYaml<T> {
    fn clone(&self) -> Self {
        return AargvarkYaml {
            value: self.value.clone(),
            source: self.source.clone(),
        };
    }
}

#[doc(hidden)]
pub fn vark_from_iter<T: AargvarkTrait, C: FromIterator<T>>(state: &mut VarkState) -> R<C> {
    let mut out = vec![];
    let mut rewind_to = state.position();
    let mut i = 0usize;
    loop {
        i += 1;
        state.breadcrumbs.push(format!("[{}]", i));
        let r = T::vark(state);
        state.breadcrumbs.pop();
        match r {
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

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return HelpPattern(vec![HelpPatternElement::Array(T::build_help_pattern(state))]);
    }
}

impl<T: AargvarkTrait + Eq + Hash> AargvarkTrait for HashSet<T> {
    fn vark(state: &mut VarkState) -> R<Self> {
        return vark_from_iter(state);
    }

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return HelpPattern(vec![HelpPatternElement::Array(T::build_help_pattern(state))]);
    }
}

fn style_usage(s: impl AsRef<str>) -> String {
    return s.as_ref().to_string();
}

fn style_description(s: impl AsRef<str>) -> String {
    return s.as_ref().to_string();
}

fn style_id(s: impl AsRef<str>) -> String {
    return console::Style::new().blue().dim().apply_to(s.as_ref()).to_string();
}

fn style_type(s: impl AsRef<str>) -> String {
    return console::Style::new().magenta().apply_to(s.as_ref()).to_string();
}

fn style_logical(s: impl AsRef<str>) -> String {
    return console::Style::new().dim().apply_to(s.as_ref()).to_string();
}

fn style_literal(s: impl AsRef<str>) -> String {
    return console::Style::new().bold().apply_to(s.as_ref()).to_string();
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub struct HelpProductionKey {
    type_id: TypeId,
    variant: usize,
}

struct HelpProduction {
    id: String,
    description: String,
    content: HelpProductionType,
}

pub enum HelpPartialContent {
    Pattern(HelpPattern),
    Production(HelpProductionType),
}

impl HelpPartialContent {
    pub fn struct_(fields: Vec<HelpField>, optional_fields: Vec<HelpOptionalField>) -> Self {
        return HelpPartialContent::Production(
            HelpProductionType::Struct(Rc::new(RefCell::new(HelpProductionTypeStruct {
                fields: fields,
                optional_fields: optional_fields,
            }))),
        );
    }

    pub fn enum_(variants: Vec<HelpVariant>) -> Self {
        return HelpPartialContent::Production(HelpProductionType::Enum(Rc::new(RefCell::new(variants))));
    }
}

pub struct HelpPartialProduction {
    pub description: String,
    pub content: HelpPartialContent,
}

pub enum HelpProductionType {
    Struct(Rc<RefCell<HelpProductionTypeStruct>>),
    Enum(Rc<RefCell<Vec<HelpVariant>>>),
}

pub struct HelpProductionTypeStruct {
    pub fields: Vec<HelpField>,
    pub optional_fields: Vec<HelpOptionalField>,
}

pub struct HelpField {
    pub id: String,
    pub pattern: HelpPattern,
    pub description: String,
}

pub struct HelpOptionalField {
    pub literal: String,
    pub pattern: HelpPattern,
    pub description: String,
}

pub struct HelpVariant {
    pub literal: String,
    pub pattern: HelpPattern,
    pub description: String,
}

#[derive(Clone)]
pub struct HelpPattern(pub Vec<HelpPatternElement>);

impl HelpPattern {
    fn render(&self, stack: &mut Vec<(HelpProductionKey, Rc<HelpProduction>)>, state: &HelpState) -> String {
        let mut out = String::new();
        for (i, e) in self.0.iter().enumerate() {
            if i > 0 {
                out.push_str(" ");
            }
            out.push_str(&e.render(stack, state));
        }
        return out;
    }
}

#[derive(Clone)]
pub enum HelpPatternElement {
    Literal(String),
    Type(String),
    Reference(HelpProductionKey),
    Option(HelpPattern),
    Array(HelpPattern),
    Variant(Vec<HelpPattern>),
}

impl HelpPatternElement {
    fn render(&self, stack: &mut Vec<(HelpProductionKey, Rc<HelpProduction>)>, state: &HelpState) -> String {
        match self {
            HelpPatternElement::Literal(l) => return style_literal(l),
            HelpPatternElement::Type(i) => return style_type(format!("<{}>", i)),
            HelpPatternElement::Reference(i) => {
                let production = state.productions.get(i).unwrap();
                stack.push((*i, production.clone()));
                return style_id(production.id.as_str())
            },
            HelpPatternElement::Option(i) => return format!(
                "{}{}{}",
                style_logical("["),
                i.render(stack, state),
                style_logical("]")
            ),
            HelpPatternElement::Array(i) => return format!("{}{}", i.render(stack, state), style_logical("[ ...]")),
            HelpPatternElement::Variant(i) => return i
                .iter()
                .map(|x| x.render(stack, state))
                .collect::<Vec<_>>()
                .join(&style_logical(" | ")),
        }
    }
}

pub struct HelpState {
    // Write during building
    name_counter: HashMap<String, usize>,
    // Write during building, read during rendering
    productions: HashMap<HelpProductionKey, Rc<HelpProduction>>,
}

impl HelpState {
    fn add(
        &mut self,
        type_id: TypeId,
        type_id_variant: usize,
        id: impl ToString,
        description: impl ToString,
        content: HelpProductionType,
    ) -> HelpProductionKey {
        let mut id = id.to_string();
        let count = *self.name_counter.entry(id.clone()).and_modify(|x| *x += 1).or_insert(1);
        if count > 1 {
            id = format!("{} ({})", id, count);
        }
        let key = HelpProductionKey {
            type_id: type_id,
            variant: type_id_variant,
        };
        self.productions.insert(key, Rc::new(HelpProduction {
            id: id,
            description: description.to_string(),
            content: content,
        }));
        return key;
    }

    pub fn add_struct(
        &mut self,
        type_id: TypeId,
        type_id_variant: usize,
        id: impl ToString,
        description: impl ToString,
    ) -> (HelpProductionKey, Rc<RefCell<HelpProductionTypeStruct>>) {
        let out = Rc::new(RefCell::new(HelpProductionTypeStruct {
            fields: vec![],
            optional_fields: vec![],
        }));
        let key = self.add(type_id, type_id_variant, id, description, HelpProductionType::Struct(out.clone()));
        return (key, out);
    }

    pub fn add_enum(
        &mut self,
        type_id: TypeId,
        type_id_variant: usize,
        id: impl ToString,
        description: impl ToString,
    ) -> (HelpProductionKey, Rc<RefCell<Vec<HelpVariant>>>) {
        let out = Rc::new(RefCell::new(vec![]));
        let key = self.add(type_id, type_id_variant, id, description, HelpProductionType::Enum(out.clone()));
        return (key, out);
    }
}

pub fn show_help_and_exit<
    F: FnOnce(&mut HelpState) -> HelpPartialProduction,
>(state: &VarkState, build_root: F) -> ! {
    fn format_desc(out: &mut String, desc: &str) {
        if !desc.is_empty() {
            out.push_str(
                &style_description(
                    textwrap::wrap(
                        desc,
                        &textwrap::Options::with_termwidth().initial_indent("    ").subsequent_indent("    "),
                    ).join("\n"),
                ),
            );
            out.push_str("\n\n");
        }
    }

    fn format_pattern(out: &mut String, content: &HelpProductionType) {
        match content {
            HelpProductionType::Struct(struct_) => {
                let struct_ = struct_.borrow();
                for f in &struct_.fields {
                    out.push_str(" ");
                    out.push_str(&style_id(&f.id));
                }
                if !struct_.optional_fields.is_empty() {
                    out.push_str(" ");
                    out.push_str(&style_logical("[ ...OPT]"));
                }
            },
            HelpProductionType::Enum(fields) => {
                for (i, f) in fields.borrow().iter().enumerate() {
                    if i > 0 {
                        out.push_str(" |");
                    }
                    out.push_str(" ");
                    out.push_str(&style_literal(&f.literal));
                }
            },
        }
    }

    fn format_content(
        out: &mut String,
        stack: &mut Vec<(HelpProductionKey, Rc<HelpProduction>)>,
        help_state: &HelpState,
        content: &HelpProductionType,
    ) {
        let mut table = comfy_table::Table::new();
        table.load_preset(comfy_table::presets::NOTHING);
        table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);
        match content {
            HelpProductionType::Struct(struct_) => {
                let struct_ = struct_.borrow();
                for f in &struct_.fields {
                    table.add_row(
                        vec![
                            comfy_table::Cell::new(
                                format!("   {}: {}", style_id(&f.id), f.pattern.render(stack, help_state)),
                            ),
                            Cell::new(style_description(&f.description))
                        ],
                    );
                }
                for f in &struct_.optional_fields {
                    let mut elems = vec![HelpPatternElement::Literal(f.literal.clone())];
                    elems.extend(f.pattern.0.clone());
                    table.add_row(
                        vec![
                            comfy_table::Cell::new(
                                format!(
                                    "   {}",
                                    HelpPatternElement::Option(HelpPattern(elems)).render(stack, help_state)
                                ),
                            ),
                            Cell::new(style_description(&f.description))
                        ],
                    );
                }
            },
            HelpProductionType::Enum(fields) => {
                for f in &*fields.borrow() {
                    table.add_row(
                        vec![
                            comfy_table::Cell::new(
                                format!("   {} {}", style_literal(&f.literal), f.pattern.render(stack, help_state)),
                            ),
                            Cell::new(style_description(&f.description))
                        ],
                    );
                }
            },
        }
        table.set_constraints(vec![comfy_table::ColumnConstraint::Boundaries {
            lower: comfy_table::Width::Percentage(20),
            upper: comfy_table::Width::Percentage(60),
        }]);
        out.push_str(&table.to_string());
        out.push_str("\n\n");
    }

    let mut help_state = HelpState {
        name_counter: HashMap::new(),
        productions: HashMap::new(),
    };
    let mut stack = Vec::<(HelpProductionKey, Rc<HelpProduction>)>::new();
    let mut seen_productions = HashSet::<HelpProductionKey>::new();
    let partial = build_root(&mut help_state);

    // Write initial partial production
    let mut out = style_usage("Usage:");
    for s in &state.breadcrumbs {
        out.push_str(" ");
        out.push_str(&style_literal(s));
    }
    out.push_str(" >");
    let mut temp_stack = vec![];
    match &partial.content {
        HelpPartialContent::Pattern(p) => {
            if !p.0.is_empty() {
                out.push_str(" ");
                out.push_str(&p.render(&mut temp_stack, &help_state));
            }
        },
        HelpPartialContent::Production(content) => {
            format_pattern(&mut out, content);
        },
    }
    out.push_str("\n\n");
    format_desc(&mut out, &partial.description);
    match &partial.content {
        HelpPartialContent::Pattern(_) => {
            out.push_str("\n\n");
        },
        HelpPartialContent::Production(content) => {
            format_content(&mut out, &mut temp_stack, &mut help_state, content);
        },
    }
    temp_stack.reverse();
    stack.extend(temp_stack);

    // Recurse productions
    while let Some((key, top)) = stack.pop() {
        if !seen_productions.insert(key) {
            continue;
        }
        out.push_str(&style_id(&top.id));
        out.push_str(":");
        format_pattern(&mut out, &top.content);
        out.push_str("\n\n");
        format_desc(&mut out, &top.description);
        let mut temp_stack = vec![];
        format_content(&mut out, &mut temp_stack, &mut help_state, &top.content);
        temp_stack.reverse();
        stack.extend(temp_stack);
    }
    print!("{}", out);
    exit(0);
}
