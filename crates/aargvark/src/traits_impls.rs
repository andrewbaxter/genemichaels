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
        traits::{
            empty_completer,
            AargvarkCompleter,
            AargvarkTrait,
        },
    },
    std::{
        collections::{
            HashMap,
            HashSet,
        },
        env::current_dir,
        ffi::OsString,
        fs::read_dir,
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
        path::{
            Path,
            PathBuf,
        },
    },
};

/// A helper enum, providing a simpler interface for types that can be parsed from
/// a single primitive string.
///
/// Note that `from_str` is not called if there is no argument - this means if you
/// need to do completer-specific setup regardless of whether a partial argument
/// was present or not, this must be done in the `Default` method of the completer.
pub trait AargvarkFromStr: Sized {
    fn from_str(s: &str) -> Result<Self, String>;
    fn build_help_pattern(state: &mut HelpState) -> HelpPattern;

    fn build_completer(_arg: &str) -> AargvarkCompleter {
        return empty_completer();
    }
}

impl<T: AargvarkFromStr> AargvarkTrait for T {
    fn vark(state: &mut VarkState) -> R<Self> {
        let s = match state.peek() {
            PeekR::None => return state.r_err_missing(),
            PeekR::Help => return R::Help(Box::new(|state| {
                return HelpPartialProduction {
                    description: "".to_string(),
                    content: HelpPartialContent::Pattern(<Self as AargvarkTrait>::build_help_pattern(state)),
                };
            })),
            PeekR::Ok(s) => s,
        };
        let completer = T::build_completer(s);
        match T::from_str(s) {
            Ok(v) => {
                state.consume();
                return state.r_ok(v, Some(completer));
            },
            Err(e) => return state.r_err(e, Some(completer)),
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

fn pathbuf_completer(arg: &str) -> AargvarkCompleter {
    let arg = arg.to_string();
    return Box::new(move || {
        fn list(p: &Path) -> Option<Vec<String>> {
            let d;
            match read_dir(&p) {
                Ok(d0) => {
                    d = d0;
                },
                Err(_) => {
                    return None;
                },
            }
            let mut out = vec![];
            for e in d {
                let Ok(e) = e else {
                    continue;
                };
                let Some(n) = e.file_name().to_str().map(|x| x.to_string()) else {
                    continue;
                };
                out.push(n);
            }
            return Some(out);
        }

        let path = PathBuf::from(&arg);
        let Ok(current_dir) = current_dir() else {
            return vec![];
        };

        // If dir, list that dir
        {
            let prefix;
            let use_path;
            if arg == "" {
                use_path = &current_dir;
                prefix = format!("");
            } else if arg.ends_with("/") {
                use_path = &path;
                prefix = arg.to_string();
            } else {
                use_path = &path;
                prefix = format!("{}/", arg);
            }
            if let Some(o) = list(use_path) {
                return o.into_iter().filter_map(|x| Some(vec![format!("{}{}", prefix, x)])).collect();
            }
        }

        // Not a dir, so must be a partial filename
        {
            let parent = path.parent().unwrap().to_path_buf();
            let prefix;
            let use_path;
            if parent.as_os_str().as_encoded_bytes() == b"" {
                use_path = &current_dir;
                prefix = format!("");
            } else {
                use_path = &parent;
                prefix = format!("{}/", parent.to_str().unwrap());
            }
            if let Some(o) = list(use_path) {
                let filter_prefix = path.file_name().unwrap().to_str().unwrap();
                let mut out = vec![];
                for e in o {
                    if !e.starts_with(filter_prefix) {
                        continue;
                    }
                    out.push(vec![format!("{}{}", prefix, e)]);
                }
                return out;
            }
        }

        // Bad path, give up
        return vec![];
    });
}

impl AargvarkFromStr for PathBuf {
    fn from_str(s: &str) -> Result<Self, String> {
        return <Self as std::str::FromStr>::from_str(s).map_err(|e| e.to_string());
    }

    fn build_help_pattern(_state: &mut HelpState) -> HelpPattern {
        return HelpPattern(vec![HelpPatternElement::Type("PATH".to_string())]);
    }

    fn build_completer(arg: &str) -> AargvarkCompleter {
        return pathbuf_completer(arg);
    }
}

#[cfg(feature = "http_types")]
auto_from_str!("URI", http::Uri);

const BOOL_LITERALS: &[&str] = &["true", "false"];

impl AargvarkFromStr for bool {
    fn from_str(s: &str) -> Result<Self, String> {
        return <Self as std::str::FromStr>::from_str(s).map_err(|e| e.to_string());
    }

    fn build_help_pattern(_state: &mut HelpState) -> HelpPattern {
        return HelpPattern(
            vec![
                HelpPatternElement::Variant(
                    BOOL_LITERALS
                        .iter()
                        .map(|l| HelpPattern(vec![HelpPatternElement::Literal(l.to_string())]))
                        .collect(),
                )
            ],
        );
    }

    fn build_completer(arg: &str) -> AargvarkCompleter {
        let arg = arg.to_string();
        return Box::new(move || {
            let mut out = vec![];
            for l in BOOL_LITERALS {
                if l.starts_with(&arg) {
                    out.push(vec![l.to_string()]);
                }
            }
            return out;
        });
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

    fn build_completer(arg: &str) -> AargvarkCompleter {
        return pathbuf_completer(arg);
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

    fn build_completer(arg: &str) -> AargvarkCompleter {
        return pathbuf_completer(arg);
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

    fn build_completer(arg: &str) -> AargvarkCompleter {
        return pathbuf_completer(arg);
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

/// This parses a path (or - for stdin) passed on the command line as toml into the
/// specified type.
#[cfg(feature = "serde_toml")]
pub struct AargvarkToml<T> {
    pub value: T,
    pub source: Source,
}

#[cfg(feature = "serde_toml")]
impl<T: for<'a> serde::Deserialize<'a>> AargvarkFromStr for AargvarkToml<T> {
    fn from_str(s: &str) -> Result<Self, String> {
        let b = AargvarkFile::from_str(s)?;
        match serde_path_to_error::deserialize(
            toml::Deserializer::new(
                &String::from_utf8(b.value).map_err(|e| format!("TOML document at {} isn't valid UTF-8: {}", s, e))?,
            ),
        ) {
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

    fn build_completer(arg: &str) -> AargvarkCompleter {
        return pathbuf_completer(arg);
    }
}

#[cfg(feature = "serde_toml")]
impl<T: Clone> Clone for AargvarkToml<T> {
    fn clone(&self) -> Self {
        return AargvarkToml {
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
        if state.provide_completions() && !out.is_empty() && state.peek() == PeekR::None {
            // Short circuit before overwriting previous state if at EOF - all types must
            // consume at least one argument
            return R::Err;
        }
        match T::vark(state) {
            R::Ok(v) => {
                out.push(v);
                rewind_to = state.position();
            },
            R::Help(b) => return R::Help(b),
            R::Err => {
                state.rewind(rewind_to);
                return state.r_ok(C::from_iter(out.into_iter()), None);
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
            R::Err => return R::Err,
            R::Help(b) => return R::Help(b),
            R::Ok(r) => r,
        };
        let mut res = res.chars().into_iter();
        let mut k = vec![];
        let mut escape = false;
        let mut at_value = false;
        for c in &mut res {
            if escape {
                k.push(c);
                escape = false;
            } else {
                if c == '\\' {
                    escape = true;
                } else if c == '=' {
                    at_value = true;
                    break;
                } else {
                    k.push(c);
                }
            }
        }
        let k = k.into_iter().collect::<String>();
        let key = match K::from_str(&k) {
            Ok(r) => r,
            Err(e) => return state.r_err(format!("Error parsing map key: {}", e), Some(K::build_completer(&k))),
        };
        if !at_value {
            return state.r_err(format!("Missing value in K=V argument"), Some(K::build_completer(&k)));
        }
        let v = res.collect::<String>();
        let value = match V::from_str(&v) {
            Ok(r) => r,
            Err(e) => return state.r_err(format!("Error parsing map value: {}", e), Some(V::build_completer(&v))),
        };
        return state.r_ok(AargvarkKV {
            key: key,
            value: value,
        }, Some(V::build_completer(&v)));
    }

    fn build_help_pattern(_state: &mut HelpState) -> HelpPattern {
        return HelpPattern(vec![HelpPatternElement::Literal("K=V".to_string())]);
    }
}

impl<K: AargvarkFromStr + Eq + std::hash::Hash, V: AargvarkFromStr> AargvarkTrait for HashMap<K, V> {
    fn vark(state: &mut VarkState) -> R<Self> {
        let res = match <Vec<AargvarkKV<K, V>>>::vark(state) {
            R::Err => return R::Err,
            R::Help(b) => return R::Help(b),
            R::Ok(r) => r,
        };
        return state.r_ok(res.into_iter().map(|kv| (kv.key, kv.value)).collect(), None);
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
