#![cfg(test)]

use {
    genemichaels_lib::{
        format_str,
        FormatConfig,
    },
};

fn rt(text: &str) {
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        keep_max_blank_lines: 0,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    pretty_assertions::assert_str_eq!(text, res.rendered);
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
    rt(
        r#"struct DefaultTupleStruct<A, B, C>(
    A,
    #[serde(default)]
    B,
    #[serde(default = "MyDefault::my_default")]
    C,
)
where
    C: MyDefault;
"#,
    );
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
fn rt_type_assignment_where1() {
    rt(r#"type CoreBitFields<const C: usize>
where
    [u8; (C + 7) / 8]: Sized = [u8; (C + 7) / 8];
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
fn rt_pat_field1() {
    rt(
        r#"fn main() {
    match x {
        Expr::MethodCall(ExprMethodCall { args, receiver: func, .. }) => { },
    }
}
"#,
    )
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
fn rt_comments_end() {
    rt(r#"const X: i32 = 7;
// This is where the file ends.
"#)
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
fn rt_comments_unbreakable_links1() {
    rt(
        r#"//! This is a very long line that will get wrapped right around check_store
//! [`check_store()`].
//!
//! [`check_store()`]: a::b::c
fn main() { }
"#,
    );
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
fn rt_comments_verbatim1() {
    rt(r#"fn main() {
    //. this is verbatim
}
"#);
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
fn rt_generic_nested_constraint() {
    rt(r#"trait X: Y<Error: Debug> { }
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

/// The start of the file is a special case since the first line is already on a
/// new line.  So for elements following others, a "blank line" is a new line from
/// the previous element, then another new line to the start of the next. But for
/// SOF a blank line is just one new line.
///
/// This should probably be handled explicitly, but for now a single function that
/// assumes it's extracting comments from non-SOF will generally be correct and
/// simpler, I don't think SOF blank lines are that important...
#[test]
fn rt_comments_blank_keep1() {
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
fn rt_try_try1() {
    rt(r#"fn main() {
    x().hello()??.await;
}
"#);
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
fn rt_comment_before_semi1() {
    rt(r#"fn main() {
    let x = 44
    // comment
    ;
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
fn rt_comment_x() {
    // https://github.com/andrewbaxter/genemichaels/issues/89
    rt(
        r#"fn get_valid_selection(get_actual_edit_transaction: impl Fn(
    // current
    &Selection,
    // next
    &Selection,
) -> anyhow::Result<EditTransaction>) -> anyhow::Result<Either<Selection, EditTransaction>> {
    todo!()
}
"#,
    );
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
fn rt_trait1() {
    rt(
        r#"pub trait MyTrait<T, D>: Sized
where
    T: MyTrait2,
    D: MyTrait3 {
    fn is_empty(&self) -> bool;
    #[allow(clippy::needless_lifetimes)]
    fn another_method<'a>(&'a self) -> ReturnValue<T, D>;
}
"#,
    );
}

#[test]
fn rt_trait2() {
    rt(r#"pub trait X {
    type Z;
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
fn rt_empty_parens1() {
    rt(
        r#"fn main() {
    call_123456789_123456789_12346789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789();
}
"#,
    );
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
fn rt_unsafe_extern_c_() {
    rt(r#"unsafe extern "C" { }
"#);
}

#[test]
fn rt_static_mut1() {
    rt(r#"static mut X: Y = Y(7);
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
fn rt_self_type() {
    rt(r#"impl Something {
    fn something(self: i32) { }
}
"#);
}

#[test]
fn rt_skip_shebang() {
    rt(r#"#!/bin/bash
fn main() { }
"#);
}

#[test]
fn rt_dontskip_modattrs() {
    rt(
        r#"#![allow(
    clippy::too_many_arguments,
    clippy::field_reassign_with_default,
    clippy::never_loop,
    clippy::derive_hash_xor_eq
)]

fn main() { }
"#,
    );
}

#[test]
fn rt_rustfmt_skip_all() {
    rt(
        r#"#![rustfmt::skip]
           fn main() {
    struct SomeStrangeIndentation {
 abcd: i32,
                              def: String, k: Option<
               (
                  )>}
}
"#,
    );
}

#[test]
fn rt_rustfmt_skip_all_with_comments() {
    rt(
        r#"#![rustfmt::skip]
           fn main() {
    struct SomeStrangeIndentation {
 abcd: i32,
     // comment 1
                              def: String, k: Option<
               (
                  )>}
}
"#,
    );
}

#[test]
fn rt_rustfmt_skip_subtree() {
    rt(
        r#"fn main() {
    #[rustfmt::skip]
    struct SomeStrangeIndentation {
 abcd: i32,
                              def: String, k: Option<
               (
                  )>}
}
"#,
    );
}

#[test]
fn rt_rustfmt_skip_subtree_comment() {
    rt(
        r#"fn main() {
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
"#,
    );
}

#[test]
fn rt_rustfmt_skip_subtree_comment_array() {
    rt(
        r#"fn main() {
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
"#,
    );
}

#[test]
fn rttabs_struct_generic1() {
    rt_tabs(
        r#"struct DefaultTupleStruct<A, B, C>(
	A,
	#[serde(default)]
	B,
	#[serde(default = "MyDefault::my_default")]
	C,
)
where
	C: MyDefault;
"#,
    );
}

#[test]
fn rt_const_ref() {
    rt(r#"fn main() {
    (*&raw const X).x(text);
}
"#);
}
