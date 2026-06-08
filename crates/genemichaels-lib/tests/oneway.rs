use {
    genemichaels_lib::{
        DeclarationNormalizationCategory,
        DeclarationNormalizationMode,
        ExternalFormatterConfig,
        FormatConfig,
        format_str,
    },
    std::collections::BTreeMap,
};

fn ow(before: &str, want_after: &str) {
    owc(before, want_after, &FormatConfig {
        max_width: 120,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_auto() {
    owc(r#"fn my_func() { }

struct MyStruct { }

use std::fmt;

mod mymod;

trait MyTrait { }

const X: i32 = 1;
"#, r#"mod mymod;

use std::fmt;

const X: i32 = 1;

trait MyTrait { }

fn my_func() { }

struct MyStruct {}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::Auto,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_auto_comments_move_with_items() {
    owc(r#"// My function
fn my_func() { }

// My import
use std::fmt;

// My module
mod mymod;
"#, r#"// My module
mod mymod;

// My import
use std::fmt;

// My function
fn my_func() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::Auto,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_auto_comments_no_lost() {
    let res = format_str(r#"// My function
fn my_func() { }

// My import
use std::fmt;

// My module
mod mymod;
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::Auto,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Lost comments: {:?}", res.lost_comments);
}

#[test]
fn ow_declaration_normalization_auto_impl_follows_type() {
    owc(r#"impl MyStruct {
    fn method(&self) { }
}

struct MyStruct { }
"#, r#"struct MyStruct {}

impl MyStruct {
    fn method(&self) { }
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::Auto,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_by_category_custom_order() {
    owc(r#"fn my_func() { }

use std::fmt;

const X: i32 = 1;
"#, r#"const X: i32 = 1;

use std::fmt;

fn my_func() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByCategory(
            vec![
                DeclarationNormalizationCategory::Const,
                DeclarationNormalizationCategory::Use,
                DeclarationNormalizationCategory::Concrete,
            ],
        ),
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_by_name() {
    owc(r#"fn b() { }

fn a() { }

use std::fmt;
"#, r#"use std::fmt;

fn a() { }

fn b() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_by_name_case_insensitive() {
    owc(r#"fn Zebra() { }

fn apple() { }
"#, r#"fn apple() { }

fn Zebra() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_by_name_comments_move_with_items() {
    owc(r#"// Comment for b
fn b() { }

// Comment for a
fn a() { }
"#, r#"// Comment for a
fn a() { }

// Comment for b
fn b() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_by_name_comments_no_lost() {
    let res = format_str(r#"// Comment for b
fn b() { }

// Comment for a
fn a() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    }).unwrap();
    assert!(res.lost_comments.is_empty(), "Lost comments: {:?}", res.lost_comments);
}

#[test]
fn ow_declaration_normalization_by_name_doc_comments_move_with_items() {
    owc(r#"/// Documentation for b
fn b() { }

/// Documentation for a
fn a() { }
"#, r#"/// Documentation for a
fn a() { }

/// Documentation for b
fn b() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_by_name_multiline_comments() {
    owc(r#"// First line for z
// Second line for z
fn z() { }

// Comment for a
fn a() { }
"#, r#"// Comment for a
fn a() { }

// First line for z Second line for z
fn z() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_enum_by_name() {
    owc(r#"enum Foo {
    Zebra,
    Apple,
    Mango,
}
"#, r#"enum Foo {
    Apple,
    Mango,
    Zebra,
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_impl_auto() {
    owc(r#"struct Foo { }

impl Foo {
    fn my_func() { }

    type Bar = i32;

    const Z: i32 = 1;
}
"#, r#"struct Foo {}

impl Foo {
    const Z: i32 = 1;
    type Bar = i32;

    fn my_func() { }
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::Auto,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_impl_by_name() {
    owc(r#"struct Foo { }

impl Foo {
    fn z() { }

    fn a() { }
}
"#, r#"struct Foo {}

impl Foo {
    fn a() { }

    fn z() { }
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_none() {
    owc(r#"fn b() { }

fn a() { }
"#, r#"fn b() { }

fn a() { }
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::None,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_struct_named_fields_by_name() {
    owc(r#"struct Foo {
    z: i32,
    a: String,
    m: bool,
}
"#, r#"struct Foo {
    a: String,
    m: bool,
    z: i32,
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_struct_tuple_unchanged() {
    owc(r#"struct Foo(i32, String);
"#, r#"struct Foo(i32, String);
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_trait_auto() {
    owc(r#"trait Foo {
    fn my_func();

    type Bar;

    const Z: i32;
}
"#, r#"trait Foo {
    const Z: i32;
    type Bar;

    fn my_func();
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::Auto,
        ..Default::default()
    });
}

#[test]
fn ow_declaration_normalization_trait_by_name() {
    owc(r#"trait Foo {
    fn z();

    fn a();
}
"#, r#"trait Foo {
    fn a();
    fn z();
}
"#, &FormatConfig {
        declaration_normalization: DeclarationNormalizationMode::ByName,
        ..Default::default()
    });
}

#[test]
fn ow_directive_comment_external_formatter() {
    let mut external_formatters = BTreeMap::new();
    external_formatters.insert("sed".to_string(), ExternalFormatterConfig {
        commandline: vec!["sed".to_string(), "-e".to_string(), "s/hello/HELLO/".to_string()],
        adjust_indent: false,
    });
    owc(r##"fn main() {
    let x = 
        //# genemichaels-external: sed
        "hello";
}
"##, r##"fn main() {
    let x = 
        //# genemichaels-external: sed
        "HELLO";
}
"##, &FormatConfig {
        external_formatters,
        ..Default::default()
    });
}

#[test]
fn ow_dont_format_when_explicit_normal() {
    // Before/after newlines inconsequential
    owc(r#" 
//  remove extra spaces"#, r#" 
//  remove extra spaces
"#, &FormatConfig {
        max_width: 120,
        explicit_markdown_comments: true,
        ..Default::default()
    });
}

#[test]
fn ow_external_formatter_adjust_indent_off() {
    let mut external_formatters = BTreeMap::new();
    external_formatters.insert("cat".to_string(), ExternalFormatterConfig {
        commandline: vec!["cat".to_string()],
        adjust_indent: false,
    });
    owc(r##"fn main() {
    let x = 
        //# genemichaels-external: cat
        r#"
        line1
        line2
    "#;
}
"##, r##"fn main() {
    let x = 
        //# genemichaels-external: cat
        r#"
        line1
        line2
    "#;
}
"##, &FormatConfig {
        external_formatters,
        ..Default::default()
    });
}

#[test]
fn ow_external_formatter_adjust_indent_on() {
    let mut external_formatters = BTreeMap::new();
    external_formatters.insert("sed".to_string(), ExternalFormatterConfig {
        commandline: vec!["sed".to_string(), "-e".to_string(), "s/^\\s+//".to_string()],
        adjust_indent: true,
    });
    owc(r##"fn main() {
    let x = 
        //# genemichaels-external: sed
        r#"
        line1
        line2
    "#;
}
"##, r##"fn main() {
    let x = 
        //# genemichaels-external: sed
        r#"
           line1
           line2
           "#;
}
"##, &FormatConfig {
        external_formatters,
        ..Default::default()
    });
}

#[test]
fn ow_external_formatter_left_strip() {
    let mut external_formatters = std::collections::BTreeMap::new();
    external_formatters.insert("cat".to_string(), ExternalFormatterConfig {
        commandline: vec!["cat".to_string()],
        adjust_indent: true,
    });
    owc(r##"fn main() {
    let x = 
        //# genemichaels-external: cat
        r#"
          line1
        line2
    "#;
}
"##, r##"fn main() {
    let x = 
        //# genemichaels-external: cat
        r#"
             line1
           line2
           "#;
}
"##, &FormatConfig {
        external_formatters,
        ..Default::default()
    });
}

#[test]
fn ow_external_formatter_to_raw() {
    let mut external_formatters = BTreeMap::new();
    external_formatters.insert("cat".to_string(), ExternalFormatterConfig {
        commandline: vec!["cat".to_string()],
        adjust_indent: true,
    });
    owc(r#"fn main() {
    let x = 
        //# genemichaels-external: cat
        "line1
line2";
}
"#, r##"fn main() {
    let x = 
        //# genemichaels-external: cat
        r#"line1
           line2"#;
}
"##, &FormatConfig {
        external_formatters,
        ..Default::default()
    });
}

#[test]
fn ow_format_explicit_normal() {
    // Before/after newlines inconsequential
    ow(r#" 
//?  remove extra spaces"#, r#" 
//? remove extra spaces
"#);
}

#[test]
fn ow_format_rust_in_comment() {
    ow(r#"/// ```rust
/// fn  foo(  )  {
/// }
/// ```
fn main() {}
"#, r#"/// ```rust
/// fn foo() { }
/// ```
fn main() { }
"#);
}

#[test]
fn ow_genem_skip_no_attrs() {
    ow(r#"fn main() {
    //# genemichaels-skip
    struct SomeStrangeIndentation {
 abcd: i32,
                              def: String, k: Option<
               (
                  )>}
}
"#, r#"fn main() {
    //# genemichaels-skip
    struct SomeStrangeIndentation {
 abcd: i32,
                              def: String, k: Option<
               (
                  )>}
}
"#);
}

#[test]
fn ow_import_normalization_combine() {
    owc(r#"// Comment 1
use std::collections::BTreeMap;
// Comment 1
use std::collections::BTreeSet;
"#, r#"use {
    // Comment 1
    std::collections::BTreeMap,
    // Comment 1
    std::collections::BTreeSet,
};
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_attr_whitespace_loss() {
    owc(r#"// Comment A
#[cfg(feature = "x")]
use a;
// Comment A
#[cfg(feature = "x")]
use b;
"#, r#"// Comment A
#[cfg(feature = "x")]
use {
    a,
    b,
};
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_attr_whitespace_no_combine() {
    owc(r#"// Comment A
#[cfg(feature = "x")]
use a;
// Comment B
#[cfg(feature = "x")]
use b;
"#, r#"// Comment A
#[cfg(feature = "x")]
use a;

// Comment B
#[cfg(feature = "x")]
use b;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_mixed() {
    owc(r#"use std::collections::BTreeMap;
pub mod x { type A = i32; }
use x::A;
"#, r#"use std::collections::BTreeMap;

pub mod x {
    type A = i32;
}

use x::A;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_single_use_group() {
    owc(r#"use {
    crate::{
        a::B,
        c::D,
    },
    flowcontrol::shed,
    dumpster::unsync::Gc,
    v8::tc_scope,
    jswrap::JsWrap,
    core::mem,
    core::marker::Copy,
};
"#, r#"use {
    core::{
        marker::Copy,
        mem,
    },
    crate::{
        a::B,
        c::D,
    },
    dumpster::unsync::Gc,
    flowcontrol::shed,
    jswrap::JsWrap,
    v8::tc_scope,
};
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_sub_diff() {
    owc(r#"use std::
    // Needed for X
    collections::BTreeMap;
use std::collections::BTreeSet;
"#, r#"use std::{
    // Needed for X
    collections::BTreeMap,
    collections::BTreeSet,
};
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_sub_diff2() {
    owc(r#"use std::{
    // Needed for X
    collections::BTreeMap,
    collections::CTreeMap
};
use std::collections::BTreeSet;
"#, r#"use std::{
    // Needed for X
    collections::BTreeMap,
    collections::{
        BTreeSet,
        CTreeMap,
    },
};
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_sub_diff3() {
    owc(r#"use std::{
    // Needed for X
    collections::{
        m::BTreeMap,
        m::CTreeMap,
    },
    collections::CTreeMap
};
use std::collections::BTreeSet;
"#, r#"use std::{
    // Needed for X
    collections::m::{
        BTreeMap,
        CTreeMap,
    },
    collections::{
        BTreeSet,
        CTreeMap,
    },
};
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_sub_diff4() {
    owc(r#"
use a::{
    b::X,
    c,
    b::Y,
};
use d::Y;
use a::b::Z;
"#, r#"use {
    a::{
        b::{
            X,
            Y,
            Z,
        },
        c,
    },
    d::Y,
};
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_combine_vis_diff() {
    owc(r#"pub use a;
use b;
"#, r#"pub use a;
use b;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_none() {
    owc(r#"use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
"#, r#"use std::collections::{
    BTreeMap,
    BTreeSet,
};
use std::fmt::Debug;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::None,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_split() {
    owc(r#"// Comment 1
use std::collections::{BTreeSet, BTreeMap};
"#, r#"// Comment 1
use std::collections::BTreeMap;

// Comment 1
use std::collections::BTreeSet;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Split,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_split_attr_whitespace_loss() {
    owc(r#"// Leading comment
#[cfg(feature = "a")]
use {a, b};
"#, r#"// Leading comment
#[cfg(feature = "a")]
use a;
#[cfg(feature = "a")]
use b;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Split,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_split_comment() {
    owc(r#"use std::{
    // comment
    collections::{
        BTreeMap,
        CTreeMap,
    },
};
"#, r#"use std::
    // comment
    collections::BTreeMap;
use std::
    // comment
    collections::CTreeMap;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Split,
        ..Default::default()
    });
}

#[test]
fn ow_import_normalization_split_mismatched_cmp() {
    owc(r#"// Comment B
use b;
// Comment A
use a;
"#, r#"// Comment A
use a;

// Comment B
use b;
"#, &FormatConfig {
        import_normalization: genemichaels_lib::ImportNormalizationMode::Split,
        ..Default::default()
    });
}

#[test]
fn ow_raw_string_wrap() {
    owc(
        r##"fn main() {
    let x = r#"
        short
        this is a very long line that should cause the entire let statement to wrap because it exceeds the maximum line length
    "#;
}
"##,
        r##"fn main() {
    let x =
        r#"
        short
        this is a very long line that should cause the entire let statement to wrap because it exceeds the maximum line length
    "#;
}
"##,
        &FormatConfig {
            max_width: 80,
            ..Default::default()
        },
    );
}

#[test]
fn ow_remove_blanks1() {
    ow(r#"fn main() {

}
"#, r#"fn main() { }
"#);
}

#[test]
fn ow_remove_blanks2() {
    ow(r#"fn main() {
    let mut x = 7;

    x *= 2;
}
"#, r#"fn main() {
    let mut x = 7;
    x *= 2;
}
"#);
}

#[test]
fn ow_rustfmt_skip_end_line_end_comment_keep() {
    owc(r#"fn main() {
    #[rustfmt::skip]
    struct SomeStrangeIndentation {
       } // End line end comment
}
"#, r#"fn main() {
    #[rustfmt::skip]
    struct SomeStrangeIndentation {
       }
    // End line end comment

}
"#, &FormatConfig {
        max_width: 120,
        explicit_markdown_comments: true,
        ..Default::default()
    });
}

#[test]
fn ow_rustfmt_skip_start_comment_duplication() {
    owc(r#"fn main() {
    #[rustfmt::skip] // early comment
    struct SomeStrangeIndentation { }
}
"#, r#"fn main() {
    // early comment
    #[rustfmt::skip]
    // early comment
    struct SomeStrangeIndentation { }
}
"#, &FormatConfig {
        max_width: 120,
        explicit_markdown_comments: true,
        ..Default::default()
    });
}

#[test]
fn ow_vec_macro_trailing_comma() {
    ow(r#"fn main() {
    let x = vec![1, 2, 3,];
}
"#, r#"fn main() {
    let x = vec![1, 2, 3];
}
"#);
}

#[test]
fn ow_vec_macro_trailing_comma_split() {
    owc(r#"fn main() {
    let x = vec![1, 2, 3];
}
"#, r#"fn main() {
    let x =
        vec![
            1,
            2,
            3,
        ];
}
"#, &FormatConfig {
        max_width: 20,
        ..Default::default()
    });
}

fn owc(before: &str, want_after: &str, config: &FormatConfig) {
    let res = format_str(before, config).unwrap();
    assert!(
        want_after == res.rendered,
        "Formatted text changed:\n\nWant after:\n{}\n\nAfter:\n{}\n",
        want_after
            .split("\n")
            .enumerate()
            .map(|(line, text)| format!("{:>03} [{}]", line, text))
            .collect::<Vec<String>>()
            .join("\n"),
        res
            .rendered
            .split("\n")
            .enumerate()
            .map(|(line, text)| format!("{:>03} [{}]", line, text))
            .collect::<Vec<String>>()
            .join("\n")
    );
}

#[test]
fn rt_external_formatter_raw_string() {
    let res = format_str(r##"fn f() -> &'static str {
    let x = 
        //# genemichaels-external: fake
        r#"
       aaaaa
    "#;
}
"##, &FormatConfig {
        external_formatters: BTreeMap::from([("fake".to_string(), ExternalFormatterConfig {
            commandline: vec!["sed".to_string(), "s/a/b/g".to_string()],
            adjust_indent: false,
        })]),
        ..Default::default()
    }).unwrap();
    pretty_assertions::assert_str_eq!(r##"fn f() -> &'static str {
    let x = 
        //# genemichaels-external: fake
        r#"
       bbbbb
    "#;
}
"##, res.rendered);
}
