#![cfg(test)]

use genemichaels::{
    format_str,
    FormatConfig,
};

fn rt(text: &str) {
    let after = format_str(text, &FormatConfig::default()).unwrap().rendered;
    assert!(text == &after, "Roundtrip changes\n\nBefore:\n{}\n\nAfter:\n{}", text, &after);
}

#[test]
fn rt_macro1() {
    rt(r#"macro_rules! err(($l: expr, $($args: tt) *) => {
    log!($l, slog::Level::Error, "", $($args) *)
};);
"#);
}

#[test]
fn rt_comment_footnote1() {
    rt(
        r#"//! If that's what you have and it works, you don't need this crate at all — you
//! can just use [`CARGO_BIN_EXE_<name>`][cargo-env].
//!
//!   [cargo-env]: https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-crates
pub struct Foo;

impl Foo {
    pub fn do_stuff() {
        fn get_cargo_env_or_panic(key: &str) -> std::ffi::OsString {
            std::env::var_os(key).unwrap_or_else(||
                panic!("The environment variable '{key}' is not set, is this running under a 'cargo test' command?")
            )
        }

        get_cargo_env_or_panic("");
    }
}"#,
    );
}
