use {
    crate::{
        base::{
            PeekR,
            VarkState,
            R,
        },
        help::{
            HelpPartialContent,
            HelpPartialProduction,
            HelpPattern,
            HelpPatternElement,
            HelpState,
        },
    },
    std::{
        collections::{
            HashMap,
            HashSet,
        },
        ffi::OsString,
        io::Read,
        net::{
            IpAddr,
            Ipv4Addr,
            Ipv6Addr,
            SocketAddr,
            SocketAddrV4,
            SocketAddrV6,
        },
        ops::Deref,
        path::PathBuf,
    },
};

/// Anything that implements this trait can be parsed and used as a field in other
/// parsable enums/structs.
pub trait AargvarkTrait: Sized {
    /// Called when this argument is reached. Should parse data until no more data can
    /// be parsed.
    fn vark(state: &mut VarkState) -> R<Self>;

    /// Called when `-h` is specified.
    fn build_help_pattern(state: &mut HelpState) -> HelpPattern;

    fn complete() -> Vec<String> {
        return vec![];
    }
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
            PeekR::None => return R::EOF(vec![]),
            PeekR::Help => return R::Help(Box::new(|state| {
                return HelpPartialProduction {
                    description: "".to_string(),
                    content: HelpPartialContent::Pattern(<Self as AargvarkTrait>::build_help_pattern(state)),
                };
            })),
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
            match std::io::stdin().read_to_end(&mut out) {
                Ok(_) => return Ok(Self {
                    value: out,
                    source: Source::Stdin,
                }),
                Err(e) => return Err(format!("Error reading stdin: {}", e)),
            };
        } else {
            match std::fs::read(s) {
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
        match serde_path_to_error::deserialize(&mut serde_json::Deserializer::from_slice(&b.value)) {
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
        match serde_path_to_error::deserialize(serde_yaml::Deserializer::from_slice(&b.value)) {
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
    loop {
        let r = T::vark(state);
        match r {
            R::Ok(v) => {
                out.push(v);
                rewind_to = state.position();
            },
            R::Help(b) => return R::Help(b),
            R::EOF(v) => {
                if state.autocomplete {
                    return R::EOF(v);
                } else {
                    state.rewind(rewind_to);
                    return state.r_ok(C::from_iter(out.into_iter()));
                }
            },
            R::Err => {
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

impl<T: AargvarkTrait + Eq + std::hash::Hash> AargvarkTrait for HashSet<T> {
    fn vark(state: &mut VarkState) -> R<Self> {
        return vark_from_iter(state);
    }

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return HelpPattern(vec![HelpPatternElement::Array(T::build_help_pattern(state))]);
    }
}

/// A key-value argument type (single argument) that takes the format `K=V` where
/// both `K` and `V` need to be string-parsable types. The argument is split at the
/// first unescaped `=` - additional `=` in the key can be escaped with `\`.
///
/// This is used for the `HashMap` implementation which takes a series of arguments
/// like `a=a b=b c=123`.
pub struct AargvarkKV<K, V> {
    pub key: K,
    pub value: V,
}

impl<K: AargvarkFromStr, V: AargvarkFromStr> AargvarkTrait for AargvarkKV<K, V> {
    fn vark(state: &mut VarkState) -> R<Self> {
        let res = String::vark(state);
        let res = match res {
            R::EOF(v) => return R::EOF(v),
            R::Err => return R::Err,
            R::Help(b) => return R::Help(b),
            R::Ok(r) => r,
        };
        let mut res = res.chars().into_iter();
        let mut k = vec![];
        let mut escape = false;
        for c in &mut res {
            if escape {
                k.push(c);
                escape = false;
            } else {
                if c == '\\' {
                    escape = true;
                } else if c == '=' {
                    break;
                } else {
                    k.push(c);
                }
            }
        }
        let key = match K::from_str(&k.into_iter().collect::<String>()) {
            Ok(r) => r,
            Err(e) => return state.r_err(format!("Error parsing map key: {}", e)),
        };
        let value = match V::from_str(&res.collect::<String>()) {
            Ok(r) => r,
            Err(e) => return state.r_err(format!("Error parsing map value: {}", e)),
        };
        return state.r_ok(AargvarkKV {
            key: key,
            value: value,
        });
    }

    fn build_help_pattern(_state: &mut HelpState) -> HelpPattern {
        return HelpPattern(vec![HelpPatternElement::Literal("K=V".to_string())]);
    }
}

impl<K: AargvarkFromStr + Eq + std::hash::Hash, V: AargvarkFromStr> AargvarkTrait for HashMap<K, V> {
    fn vark(state: &mut VarkState) -> R<Self> {
        let res = match <Vec<AargvarkKV<K, V>>>::vark(state) {
            R::EOF(v) => return R::EOF(v),
            R::Err => return R::Err,
            R::Help(b) => return R::Help(b),
            R::Ok(r) => r,
        };
        return state.r_ok(res.into_iter().map(|kv| (kv.key, kv.value)).collect());
    }

    fn build_help_pattern(state: &mut HelpState) -> HelpPattern {
        return <Vec<AargvarkKV<K, V>>>::build_help_pattern(state);
    }
}

/// A simple wrapper around `String` that only accepts strings that don't start
/// with `-`. Having an argument of `Vec<String>` will cause it to consume all
/// remaining arguments (since all of them are valid strings), while `Vec<NotFlag>`
/// will only consume until the next flag (or the end of the arguments).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotFlag(pub String);

impl AargvarkFromStr for NotFlag {
    fn from_str(s: &str) -> Result<Self, String> {
        if s.starts_with("-") {
            return Err("Argument looks like a flag (starts with -)".to_string());
        }
        return Ok(NotFlag(s.to_string()));
    }

    fn build_help_pattern(_state: &mut HelpState) -> HelpPattern {
        return HelpPattern(vec![HelpPatternElement::Type("STRING".to_string())]);
    }
}

impl ToString for NotFlag {
    fn to_string(&self) -> String {
        return self.0.clone();
    }
}

impl Deref for NotFlag {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        return &self.0;
    }
}

impl Into<String> for NotFlag {
    fn into(self) -> String {
        return self.0;
    }
}

impl AsRef<str> for NotFlag {
    fn as_ref(&self) -> &str {
        return self.0.as_str();
    }
}
