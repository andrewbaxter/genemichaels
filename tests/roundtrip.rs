#![cfg(test)]

use genemichaels::{
    format_str,
    FormatConfig,
};

fn rt(text: &str) {
    assert_eq!(text, &format_str(text, &FormatConfig::default()).unwrap().rendered);
}

#[test]
fn rt_field() {
    rt(r#"fn main() {
    let _x = MyStruct { abc };
}
"#)
}

#[test]
fn rt_macro1() {
    rt(r#"macro_rules! err(($l: expr, $($args: tt) *) => {
    log!($l, slog::Level::Error, "", $($args) *)
};);
"#);
}

#[test]
fn rt_macro2() {
    rt(
        r#"struct Foo {
    yes: bool,
    other: i32,
}

fn g(f: Foo) {
    assert!(matches!(f, Foo { yes: yes, .. } if yes))
}
"#,
    );
}

#[test]
fn rt_comments_numbered_list1() {
    rt(r#"// 1. list item one
//
// 2. list item 2
//
// 3. list item 3
static _x: i32 = 4i32;
"#)
}
