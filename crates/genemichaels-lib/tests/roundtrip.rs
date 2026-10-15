#![cfg(test)]

use {
    genemichaels_lib::{
        DeclarationNormalizationMode,
        ExternalFormatterConfig,
        FormatConfig,
        format_str,
    },
    std::collections::BTreeMap,
};

fn rt_keep(text: &str) {
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 1,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
}

fn rt(text: &str) {
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 0,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
}

#[test]
fn rt_comment_before_if_brace1() {
    rt(r#"fn main() {
    if true
    // hi
    { }
}
"#)
}

#[test]
fn rt_comment_before_label1() {
    rt(r#"fn main() {
    // hello
    'x: for a in b { }
}
"#);
}

#[test]
fn rt_comment_before_semi1() {
    rt(r#"fn main() {
    let x = 44
    // comment
    ;
}
"#)
}

#[test]
fn rt_comment_x() {
    // https://github.com/andrewbaxter/genemichaels/issues/89
    rt(r#"fn get_valid_selection(get_actual_edit_transaction: impl Fn(
    // current
    &Selection,
    // next
    &Selection,
) -> anyhow::Result<EditTransaction>) -> anyhow::Result<Either<Selection, EditTransaction>> {
    todo!()
}
"#);
}

#[test]
fn rt_comments_blank_keep1() {
    rt_keep(r#"
fn main() { }
"#);
}

#[test]
fn rt_comments_blank_keep_sof_max1() {
    let res = format_str(r#"


fn main() { }
"#, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 1,
        ..Default::default()
    }).unwrap();
    assert_eq!(res.rendered, r#"
fn main() { }
"#);
}

#[test]
fn rt_comments_blank_keep2() {
    rt_keep(r#"// a

// b
fn main() {
    // c

    x;

    // d
    y;
}

fn b() { }
"#);
}

#[test]
fn rt_comments_blank_nokeep1() {
    let res = format_str(r#"


fn main() { }
"#, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 0,
        ..Default::default()
    }).unwrap();
    assert_eq!(res.rendered, r#"fn main() { }
"#);
}

#[test]
fn rt_comments_directive_verbatim() {
    rt(r#"fn main() {
    //# this is verbatim
}
"#);
}

#[test]
fn rt_comments_end() {
    rt(r#"const X: i32 = 7;
// This is where the file ends.
"#)
}

#[test]
fn rt_comments_generic_type1() {
    rt(r#"struct X<
    // hi
    F: FnOnce(i32) -> i32,
>(F);
"#);
}

#[test]
fn rt_comments_macro_comment() {
    rt(r#"x!{
    a,
    b
    //. .
    ,
}
"#);
}

#[test]
fn rt_comments_macro_final_comma() {
    rt(r#"x!{
    a,
    b,
}
"#);
}

#[test]
fn rt_comments_macro_final_comma2() {
    rt(r#"x!(1, 2,);
"#);
}

#[test]
fn rt_comments_md_long_h1() {
    rt(r#"// # This is a very very very unusually very very long comment that should wrap even longer and longer
"#);
}

#[test]
fn rt_comments_md_long_h2() {
    rt(r#"// ## This is a very very very unusually very very long comment that should wrap even longer and longer
"#);
}

#[test]
fn rt_comments_md_long_p() {
    rt(r#"// This is a very very very unusually very very long comment that should wrap even
// longer and longer but make sure it wraps the same way every time
//
// This should be a separate paragraph
"#);
}

#[test]
fn rt_comments_mixed1() {
    rt(r#"// a
/// b
struct S;
"#);
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

#[test]
fn rt_comments_numbered_list2() {
    rt(r#"// 3. list item one
//
// 4. list item 2
//
// 5. list item 3
static _x: i32 = 4i32;
"#)
}

#[test]
fn rt_comments_sig_return() {
    rt(r#"fn main()
// test
-> () { }
"#);
}

#[test]
fn rt_comments_try1() {
    rt(r#"fn main() {
    x()
    // failed
    ?;
}
"#);
}

#[test]
fn rt_comments_unbreakable_links1() {
    rt(r#"//! This is a very long line that will get wrapped right around check_store
//! [`check_store()`].
//!
//! [`check_store()`]: a::b::c
fn main() { }
"#);
}

#[test]
fn rt_comments_verbatim1() {
    rt(r#"fn main() {
    //. this is verbatim
}
"#);
}

#[test]
fn rt_const_ref() {
    rt(r#"fn main() {
    (*&raw const X).x(text);
}
"#);
}

#[test]
fn rt_dontskip_modattrs() {
    rt(r#"#![allow(
    clippy::too_many_arguments,
    clippy::field_reassign_with_default,
    clippy::never_loop,
    clippy::derive_hash_xor_eq
)]

fn main() { }
"#);
}

#[test]
fn rt_empty_parens1() {
    rt(r#"fn main() {
    call_123456789_123456789_12346789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789();
}
"#);
}

#[test]
fn rt_extern_c_static1() {
    rt(r#"extern "C" {
    static X: Y;
}
"#);
}

#[test]
fn rt_extern_c_type1() {
    rt(r#"fn main() {
    let language_fn: Symbol<unsafe extern "C" fn() -> Language> = x;
}
"#);
}

#[test]
fn rt_external_formatter_comment_code_block() {
    rt_fake(r#"/// ```fake
/// aaaaa
/// ```
fn f() { }
"#);
}

fn rt_fake(text: &str) {
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        external_formatters: BTreeMap::from([("fake".to_string(), ExternalFormatterConfig {
            commandline: vec!["sed".to_string(), "s/./a/g".to_string()],
            adjust_indent: false,
        })]),
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
}

#[test]
fn rt_field1() {
    rt(r#"fn main() {
    let _x = MyStruct { abc };
}
"#)
}

#[test]
fn rt_field2() {
    rt(r#"fn main() {
    match x {
        MyStruct { abc } => { },
    }
}
"#)
}

#[test]
fn rt_fn1() {
    rt(r#"fn main<T>()
where
    T: Fn() { }
"#);
}

#[test]
fn rt_genem_file_skip() {
    rt(r#"//# genemichaels-file-skip
fn main() {
    let x=1;
}
"#);
}

#[test]
fn rt_generic_nested_constraint() {
    rt(r#"trait X: Y<Error: Debug> { }
"#);
}

#[test]
fn rt_import_normalization_combine_single_use_group_doc_comment() {
    let text = r#"//! Doc comment line one
//!
//! Doc comment line two
use {
    a::A,
    b::B,
};

fn main() { }
"#;
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 0,
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
}

#[test]
fn rt_import_normalization_split_doc_comment() {
    let text = r#"//! This is a module doc comment
use std::collections::BTreeSet;
use std::collections::HashMap;
"#;
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 0,
        import_normalization: genemichaels_lib::ImportNormalizationMode::Split,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
}

#[test]
fn rt_inner_doc_only() {
    rt(r#"//! Only a doc
"#);
}

#[test]
fn rt_inner_doc_scopes() {
    rt(r#"//! File doc
#![allow(x)]

mod m {
    //! Mod doc
    use z;
}

mod empty {
    //! Only a doc
}

fn f() {
    //! Fn doc
    let x = 1;
}

impl X {
    //! Impl doc
    fn h() { }
}

trait T {
    //! Trait doc
}

extern "C" {
    //! Extern doc
}

m!{
    //! Macro doc
    struct Q;
}

m!(
    //! Macro paren doc
    a
);
"#);
}

#[test]
fn rt_labeled_block() {
    rt(r#"fn main() {
    'label: { }
}
"#);
}

#[test]
fn rt_macro1() {
    rt(r#"macro_rules! err(($l: expr, $($args: tt) *) => {
    log!($l, slog::Level::Error, "", $($args) *)
});
"#);
}

#[test]
fn rt_macro2() {
    rt(r#"fn g(f: Foo) {
    assert!(matches!(f, Foo { yes: yes, .. } if yes))
}
"#);
}

#[test]
fn rt_macro_blockcomma() {
    rt(r#"fn main() {
    mac!({
        "a": {
        },
        "b": x
    });
}
"#)
}

#[test]
fn rt_macro_star_equal() {
    rt(r#"x!(a *= b);
"#);
}

#[test]
fn rt_macro_star_equal_gt() {
    rt(r#"x!(a * => b);
"#);
}

#[test]
fn rt_match_at1() {
    rt(r#"fn main() {
    match age() {
        n @ 13 ..= 19 => (),
    }
}
"#);
}

#[test]
fn rt_match_at2() {
    rt(r#"fn main() {
    match age() {
        Some(n @ 42) => (),
    }
}
"#);
}

#[test]
fn rt_match_at3() {
    rt(r#"fn main() {
    match x {
        v @ (A::B | A::A) => (),
    }
}
"#);
}

#[test]
fn rt_match_attr_indent1() {
    rt(r#"fn main() {
    match x() {
        #[something]
        X => { },
    }
}
"#);
}

#[test]
fn rt_pat_field1() {
    rt(r#"fn main() {
    match x {
        Expr::MethodCall(ExprMethodCall { args, receiver: func, .. }) => { },
    }
}
"#)
}

#[test]
fn rt_rustfmt_skip_all() {
    rt(r#"#![rustfmt::skip]
           fn main() {
    struct SomeStrangeIndentation {
 abcd: i32,
                              def: String, k: Option<
               (
                  )>}
}
"#);
}

#[test]
fn rt_rustfmt_skip_all_with_comments() {
    rt(r#"#![rustfmt::skip]
           fn main() {
    struct SomeStrangeIndentation {
 abcd: i32,
     // comment 1
                              def: String, k: Option<
               (
                  )>}
}
"#);
}

#[test]
fn rt_rustfmt_skip_subtree() {
    rt(r#"fn main() {
    #[rustfmt::skip]
    struct SomeStrangeIndentation {
 abcd: i32,
                              def: String, k: Option<
               (
                  )>}
}
"#);
}

#[test]
fn rt_rustfmt_skip_subtree_comment() {
    rt(r#"fn main() {
    #[rustfmt::skip]
    // Start comment
    struct SomeStrangeIndentation {
// Some comment
 abcd: i32,
                              def: String, k: Option<
               (
                  )>}
    // End line end comment
}
"#);
}

#[test]
fn rt_rustfmt_skip_subtree_comment_array() {
    rt(r#"fn main() {
    #[rustfmt::skip]
    let src_attribs = [
        // EGL_WIDTH
        0x3057, src.width() as i32,
        // EGL_HEIGHT
        0x3056, src.height() as i32,
        //EGL_DMA_BUF_PLANE0_FD_EXT
        0x3271, srcfd,
        // EGL_DMA_BUF_PLANE0_OFFSET_EXT
        0x3273, 0,
        // EGL_DMA_BUF_PLANE0_PITCH_EXT
        0x3274, src.stride() as i32,
        // EGL_LINUX_DRM_FOURCC_EXT
        // DRM_FORMAT_XRGB8888
        0x3203, 0x34325258,
        egl::EGL_NONE,
     ];
}
"#);
}

#[test]
fn rt_self_type() {
    rt(r#"impl Something {
    fn something(self: i32) { }
}
"#);
}

#[test]
fn rt_skip_norm_enum() {
    rtc(r#"//# genemichaels-norm-skip
enum Bar {
    Zed,
    Alpha,
    Mid,
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn rt_skip_norm_struct() {
    rtc(r#"//# genemichaels-norm-skip
struct Foo {
    z: i32,
    a: u8,
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn rt_skip_norm_struct2() {
    rtc(r#"#[derive(Aargvark)]
//# genemichaels-norm-skip
struct DerivePgpSignCommand {
    /// Path of key (in ascii-armor format) to sign with
    key: AargvarkSpecificPath,
    /// Data to sign.
    data: AargvarkFile,
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn rt_skip_norm_struct3() {
    rtc(r#"//# genemichaels-norm-skip
#[derive(Aargvark)]
struct DerivePgpSignCommand {
    /// Path of key (in ascii-armor format) to sign with
    key: AargvarkSpecificPath,
    /// Data to sign.
    data: AargvarkFile,
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn rt_skip_shebang() {
    rt(r#"#!/bin/bash
fn main() { }
"#);
}

#[test]
fn rt_static_mut1() {
    rt(r#"static mut X: Y = Y(7);
"#);
}

#[test]
fn rt_struct1() {
    rt(r#"fn main() {
    x(MyStruct {
        y: z,
        ..Default::default()
    })
}
"#)
}

#[test]
fn rt_struct_generic1() {
    rt(r#"struct DefaultTupleStruct<A, B, C>(
    A,
    #[serde(default)]
    B,
    #[serde(default = "MyDefault::my_default")]
    C,
)
where
    C: MyDefault;
"#);
}

fn rt_tabs(text: &str) {
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 0,
        indent_spaces: 4,
        indent_unit: genemichaels_lib::IndentUnit::Tabs,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
}

#[test]
fn rt_trait1() {
    rt(r#"pub trait MyTrait<T, D>: Sized
where
    T: MyTrait2,
    D: MyTrait3 {
    fn is_empty(&self) -> bool;
    #[allow(clippy::needless_lifetimes)]
    fn another_method<'a>(&'a self) -> ReturnValue<T, D>;
}
"#);
}

#[test]
fn rt_trait2() {
    rt(r#"pub trait X {
    type Z;
}
"#)
}

#[test]
fn rt_trait_associated_type_where1() {
    rt(r#"trait ABC {
    type Iterator<'a>: Iterator<Item = &'a T>
    where
        T: 'a,
        Self: 'a;
}
"#);
}

#[test]
fn rt_trait_impl_associated_type_where1() {
    rt(r#"impl ABC for T {
    type Assoc = usize
    where
        T: Trait;
}
"#);
}

#[test]
fn rt_trait_impl_default_fn() {
    rt(r#"impl ABC for T {
    default fn deserialize_or_zeroed() { }
}
"#);
}

#[test]
fn rt_try_try1() {
    rt(r#"fn main() {
    x().hello()??.await;
}
"#);
}

#[test]
fn rt_tuple_unit1() {
    rt(r#"fn main() {
    x((7,));
}
"#);
}

#[test]
fn rt_tuple_unit2() {
    rt(r#"fn main(x: (i32,)) { }
"#);
}

#[test]
fn rt_tuple_unit3() {
    rt(r#"fn main((x,): (i32,)) { }
"#);
}

#[test]
fn rt_type_assignment_where1() {
    rt(r#"type CoreBitFields<const C: usize>
where
    [u8; (C + 7) / 8]: Sized = [u8; (C + 7) / 8];
"#);
}

#[test]
fn rt_unsafe_extern_c_() {
    rt(r#"unsafe extern "C" { }
"#);
}

fn rtc(text: &str, config: &FormatConfig) {
    let res = format_str(text, config).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
}

#[test]
fn rttabs_struct_generic1() {
    rt_tabs(r#"struct DefaultTupleStruct<A, B, C>(
	A,
	#[serde(default)]
	B,
	#[serde(default = "MyDefault::my_default")]
	C,
)
where
	C: MyDefault;
"#);
}

#[test]
fn rt_pub_extern_crate1() {
    rt(r#"pub extern crate core;
"#);
}

#[test]
fn rt_unsafe_mod1() {
    rt(r#"unsafe mod n;
"#);
}

#[test]
fn rt_variadic_named1() {
    rt(r#"extern "C" {
    fn foo(x: i32, va1: ...);
}

unsafe extern "C" fn v(n: usize, mut args: ...) { }

type F = unsafe extern "C" fn(i32, rest: ...);
"#);
}

#[test]
fn rt_closure_binder_const1() {
    rt(r#"fn main() {
    for<'a> async move || -> () { };
    const || { };
}
"#);
}

#[test]
fn rt_qself_struct1() {
    rt(r#"fn main() {
    <Foo as A>::Assoc {};
    let <T as Trait>::Assoc { x } = y;
    let <T as Trait>::Assoc(a) = y;
}
"#);
}

#[test]
fn rt_extern_type_generics1() {
    rt(r#"extern "C" {
    type A<T>
    where
        T: 'static;
}
"#);
}

#[test]
fn rt_pat_rest_attrs1() {
    rt(r#"fn main() {
    let S {
        a,
        #[cfg(x)]
        ..
    } = s;
}
"#);
}

#[test]
fn rt_match_inner_attrs1() {
    rt(r#"fn main() {
    match f {
        #![deny(x)]
        Match => { },
    }
}
"#);
}

#[test]
fn rt_doc_comment_after_brace_stays_with_item1() {
    let res = format_str(r#"fn a() {
}/// Docs for b
fn b() { }
"#, &FormatConfig::default()).unwrap();
    assert!(res.lost_comments.is_empty());
    pretty_assertions::assert_str_eq!(r#"fn a() { }

/// Docs for b
fn b() { }
"#, res.rendered);
}

#[test]
fn rt_lone_inner_attr1() {
    rt(r#"#![deny]
"#);
}

#[test]
fn rt_rustfmt_skip_file_verbatim1() {
    rt(r#"//! Docs
// hi
#![rustfmt::skip]
fn   x() {}
"#);
}

#[test]
fn rt_macro_comment_before_close1() {
    rt(r#"fn main() {
    foo!(
        // c
    );
}

#[foo(
    // c
)]
fn x() { }
"#);
}

#[test]
fn rt_turbofish_comment1() {
    rt(r#"fn main() {
    x.foo::<
        // d
        A,
    >();
    x = foo
        // e
        ::<u32>;
}
"#);
}

#[test]
fn rt_unsafe_loop_comment_before_brace1() {
    rt(r#"fn main() {
    unsafe
    // c
    {
        x;
    }
    loop
    // d
    { }
}
"#);
}

#[test]
fn rt_empty_comment1() {
    rt(r#"//
fn x() {
    //
    a;
}
"#);
}

#[test]
fn rt_comments_around_inner_doc_merged1() {
    let res = format_str(r#"// a
//! Docs
// b
#[x]
fn f() { }
"#, &FormatConfig::default()).unwrap();
    assert!(res.lost_comments.is_empty());
    pretty_assertions::assert_str_eq!(r#"//! Docs
// a b
#[x]
fn f() { }
"#, res.rendered);
    rt(&res.rendered);
}

#[test]
fn rt_triple_star_block_comment_not_doc1() {
    let res = format_str(r#"fn main() {
    /*****! certainly not I */
}
"#, &FormatConfig::default()).unwrap();
    pretty_assertions::assert_str_eq!(r#"fn main() {
    // ! certainly not I
}
"#, res.rendered);
}

#[test]
fn rt_shebang_bare1() {
    rt(r#"#!
fn main() { }
"#);
}

#[test]
fn rt_shebang_like_inner_attr1() {
    let res = format_str(r#"#!//bin/bash
[allow(x)]
fn main() {}
"#, &FormatConfig::default()).unwrap();
    pretty_assertions::assert_str_eq!(r#"// bin/bash
#![allow(x)]

fn main() { }
"#, res.rendered);
}

#[test]
fn rt_markdown_reference_labels1() {
    rt(r#"// See [MCP 295], [x][], and [Foo Bar][Baz Qux].
//
// [MCP 295]: https://github.com/rust-lang/compiler-team/issues/295
//
// [Baz Qux]: https://x.y
//
// [x]: https://z
fn x() { }
"#);
}

#[test]
fn rt_markdown_unstable_kept_verbatim1() {
    rt(r#"// ~? ERROR unknown start of token: `
// ~? ERROR expected identifier, found `%`
fn x() { }
"#);
}

#[test]
fn rt_trait_alias_where1() {
    rt(r#"trait MkSvc<T> = Svc<T>
where
    <Self as Svc<T>>::Res: Svc<T>;
"#);
}

#[test]
fn rt_open_range_float1() {
    rt(r#"fn main() {
    _ = 0. ..;
}
"#);
}

#[test]
fn rt_where_trailing_comma_comment1() {
    let res = format_str(r#"fn foo<F>()
where
    F: 'static /* c */,
{}
"#, &FormatConfig::default()).unwrap();
    pretty_assertions::assert_str_eq!(r#"fn foo<F>()
where
    F: 'static
// c
{ }
"#, res.rendered);
    rt(&res.rendered);
}

#[test]
fn rt_rustfmt_skip_inner_line_comment_not_duplicated1() {
    rt(r#"#[rustfmt::skip]
fn main(// c
) {}
"#);
}

#[test]
fn rt_float_dot_method1() {
    rt(r#"fn main() {
    _ = 0. .to_string();
    _ = .. ..;
}
"#);
}

#[test]
fn rt_comment_before_comma1() {
    let res = format_str(r#"fn main() {
    f(a /* c */, b);
    let [x /* e */, y] = z;
}
"#, &FormatConfig::default()).unwrap();
    pretty_assertions::assert_str_eq!(r#"fn main() {
    f(
        a,
        // c
        b,
    );
    let [
        x,
        // e
        y,
    ] = z;
}
"#, res.rendered);
    rt(&res.rendered);
}

#[test]
fn rt_comment_before_trailing_comma1() {
    let res = format_str(r#"fn main() {
    S { a: 1, b: 2 /* c */, };
}
"#, &FormatConfig::default()).unwrap();
    rt(&res.rendered);
    assert!(res.rendered.contains("// c"));
}

#[test]
fn rt_lifetime_param_comment1() {
    rt(r#"fn foo(_: impl for<
    /// doc
    'a,
> FnOnce() -> &'a u32) { }
"#);
}

#[test]
fn rt_dyn_comment1() {
    let res = format_str(r#"fn f(x: Box</* c */ dyn A + 'static>) {}
"#, &FormatConfig::default()).unwrap();
    assert!(res.lost_comments.is_empty());
    rt(&res.rendered);
}

#[test]
fn rt_long_else_if_chain_fast1() {
    std::thread::Builder::new().stack_size(256 * 1024 * 1024).spawn(|| {
        let mut text = String::from("fn f(v: u32) -> u32 {\n    if v == 0 {\n        0\n");
        for i in 1 .. 2000 {
            text.push_str(&format!("    }} else if v == {} {{\n        {}\n", i, i));
        }
        text.push_str("    } else {\n        0\n    }\n}\n");
        let start = std::time::Instant::now();
        rt(&text);
        assert!(start.elapsed() < std::time::Duration::from_secs(10), "Took {:?}", start.elapsed());
    }).unwrap().join().unwrap();
}

#[test]
fn rt_rustfmt_skip_match_arm_comma1() {
    rt(r#"fn main() {
    match 1 {
        #[rustfmt::skip]
        _ => {},
        #[rustfmt::skip]
        _ =>   1,
    }
}
"#);
}

#[test]
fn rt_trait_fn_inner_attr1() {
    rt(r#"trait T {
    fn f() {
        #![allow(x)]

        a
    }
}
"#);
}

#[test]
fn rt_macro_lossy_parse_kept1() {
    rt(r#"c_enum!{
    enum thread_state {
        pub B_THREAD_RUNNING = 1,
    }
}

fn f() {
    assert_eq!(match c {
        0...3 => 1,
        _ => 2
    }, 1);
}
"#);
}

#[test]
fn rt_macro_trailing_separators_kept1() {
    rt(r#"fn f() {
    quote!{
        fn serialize<S>() -> u8
        where
            S: Serializer, { }

        fn g() -> impl Sized + { }
    };
}
"#);
}

#[test]
fn rt_markdown_code_fence_info1() {
    rt(r#"//! ```rust, no_run
//! x
//! ```
fn x() { }
"#);
}

#[test]
fn rt_markdown_autolinks1() {
    rt(r#"// * Henry <hdevalence@hdevalence.ca> and <https://x.y/z>
fn x() { }
"#);
}

#[test]
fn rt_markdown_multiline_label1() {
    let res = format_str(r#"/// [ISAKMP/IKE
/// amplification]: https://example.com/x
fn rng_seed() { }
"#, &FormatConfig::default()).unwrap();
    pretty_assertions::assert_str_eq!(r#"/// [ISAKMP/IKE amplification]: https://example.com/x
fn rng_seed() { }
"#, res.rendered);
}

#[test]
fn rt_markdown_parser_panic1() {
    let res = format_str(r#"extern  {
    // GStaticMutex
    //=========================================================================
    //=========================================================================
    // GStaticPrivate
    //=========================================================================
}
"#, &FormatConfig::default()).unwrap();
    assert!(res.lost_comments.is_empty());
    rt(&res.rendered);
}

#[test]
fn rt_macro_nested_receiver_fast1() {
    std::thread::Builder::new().stack_size(256 * 1024 * 1024).spawn(|| {
        let mut text = String::from("fn x() {\n    quote!{\n        fn foo(\n");
        for _ in 0 .. 21 {
            text.push_str("self: foo<{ fn foo(\n");
        }
        for _ in 0 .. 21 {
            text.push_str(")}>\n");
        }
        text.push_str(") {}\n    };\n}\n");
        let start = std::time::Instant::now();
        format_str(&text, &FormatConfig::default()).unwrap();
        assert!(start.elapsed() < std::time::Duration::from_secs(10), "Took {:?}", start.elapsed());
    }).unwrap().join().unwrap();
}

#[test]
fn rt_markdown_content_change_kept_verbatim1() {
    rt(r#"//! [![github]](https://github.com/x)&ensp;[![crates-io]](https://crates.io/x)
enum Precedence {
    // + -
}
"#);
}

#[test]
fn rt_rustfmt_skip_mod_inner_attr1() {
    rt(r#"#[rustfmt::skip]
mod _rt {
    #![allow]
    fn   x() {}
}
"#);
}

#[test]
fn rt_macro_no_dyn_added1() {
    rt(r#"should_parse!({
    type A = Box<Foo +>;
});
"#);
}

#[test]
fn rt_macro_doc_comment_turbofish1() {
    rt(r#"fn main() {
    println!(size_of
        /// injected
        ::<Foo>)
}
"#);
}

#[test]
fn rt_long_line_array_fast1() {
    let mut text = String::from("pub const T: [u16; 8000] = [\n");
    for i in 0 .. 8000 {
        text.push_str(&format!("    0x{:04x},\n", i));
    }
    text.push_str("];\n");
    let one_line = text.replace(",\n    ", ", ");
    let start = std::time::Instant::now();
    let res = format_str(&one_line, &FormatConfig::default()).unwrap();
    assert!(start.elapsed() < std::time::Duration::from_secs(10), "Took {:?}", start.elapsed());
    assert!(res.rendered.contains("    0x1f3f,\n"));
    rt(&res.rendered);
}

#[test]
fn rt_markdown_hard_break1() {
    let res = format_str("/// a b  \n/// c d\ntype X = u8;\n", &FormatConfig::default()).unwrap();
    pretty_assertions::assert_str_eq!("/// a b c d\ntype X = u8;\n", res.rendered);
}

#[test]
fn rt_markdown_multiline_title1() {
    let res =
        format_str(
            "// > [first]: http://example.com \"\n// > second\"\nfn f() { }\n",
            &FormatConfig::default(),
        ).unwrap();
    rt(&res.rendered);
}

#[test]
fn rt_macro_nested_tuple_field1() {
    rt(r#"fn f() {
    assert_eq!(&value.0.0)
}
"#);
}

#[test]
fn rt_fn_pointer_arg_comments1() {
    rt(r#"type F = fn(
    *const DisplayInfo,
    // c
    *mut *mut ImageOutput,
) -> Status;
type G = Option<fn(
    a: u32,
    /// doc
    param4: u32,
) -> i32>;
"#);
}

#[test]
fn rt_assoc_constraint_generics1() {
    rt(r#"fn f<T: Trait<Gat<'a>: Bound>>() { }
"#);
}

#[test]
fn rt_anonymous_param1() {
    rt(r#"trait T {
    fn f(Box<u8>);
}
"#);
}

#[test]
fn rt_tuple_index_text1() {
    rt(r#"fn main() {
    x.1_000;
    S { 0x1: 1 };
}
"#);
}

#[test]
fn rt_int_literal_tuple_index1() {
    rt(r#"fn main() {
    0 .0;
}
"#);
}

#[test]
fn rt_macro_int_literal_dot1() {
    rt(r#"fn main() {
    m!(1 .; 1. .);
}
"#);
}

#[test]
fn rt_variadic_comment1() {
    rt(r#"unsafe extern "C" {
    fn test(
        _: i32,
        // a
        ap: ...
    ) { }
}
"#);
}

#[test]
fn rt_if_let_guard_eq_comment1() {
    rt(r#"fn foo() {
    match () {
        () if let Foo {}
            // a
            = Foo => { },
    }
}
"#);
}

#[test]
fn rt_or_pattern_after_comment1() {
    rt(r#"fn main() {
    match x {
        A => b,
        // c
        D | E => { },
    }
}
"#);
}

#[test]
fn rt_macro_bang_comment1() {
    rt(r#"fn main() {
    m
    // a
    !();
}
"#);
}
