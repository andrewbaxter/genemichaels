#![cfg(test)]

use genemichaels::{
    format_str,
    FormatConfig,
};

fn rt(text: &str) {
    let res = format_str(text, &FormatConfig {
        max_width: 120,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Comments remain: {:?}", res.lost_comments);
    assert!(text == res.rendered, "Formatted text changed:\n\nBefore:\n{}\n\nAfter:\n{}\n", text, res.rendered);
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
