use {
    genemichaels_lib::{
        FormatConfig,
        ExternalFormatterConfig,
        format_str,
    },
    std::collections::BTreeMap,
};

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

fn ow(before: &str, want_after: &str) {
    owc(before, want_after, &FormatConfig {
        max_width: 120,
        ..Default::default()
    });
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
fn ow_format_explicit_normal() {
    // Before/after newlines inconsequential
    ow(r#" 
//?  remove extra spaces"#, r#" 
//? remove extra spaces
"#);
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
fn ow_external_formatter_adjust_indent_on() {
    let mut external_formatters = BTreeMap::new();
    external_formatters.insert("sed".to_string(), ExternalFormatterConfig {
        commandline: vec!["sed".to_string(), "-e".to_string(), "s/^\\s+//".to_string()],
        adjust_indent: true,
    });
    owc(r##"fn main() {
    let x = #[rustfmt::external("sed")]
    r#"
        line1
        line2
    "#;
}
"##, r##"fn main() {
    let x = #[rustfmt::external("sed")]
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
fn ow_external_formatter_adjust_indent_off() {
    let mut external_formatters = BTreeMap::new();
    external_formatters.insert("cat".to_string(), ExternalFormatterConfig {
        commandline: vec!["cat".to_string()],
        adjust_indent: false,
    });
    owc(r##"fn main() {
    let x = #[rustfmt::external("cat")]
    r#"
        line1
        line2
    "#;
}
"##, r##"fn main() {
    let x = #[rustfmt::external("cat")]
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
    let x = #[rustfmt::external("cat")]
    "line1
line2";
}
"#, r##"fn main() {
    let x = #[rustfmt::external("cat")]
    r#"line1
       line2"#;
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
    let x = #[rustfmt::external("cat")]
    r#"
          line1
        line2
    "#;
}
"##, r##"fn main() {
    let x = #[rustfmt::external("cat")]
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
fn rt_external_formatter_raw_string() {
    let res = format_str(r##"fn f() -> &'static str {
    #[rustfmt::external("fake")]
    r#"
       aaaaa
    "#
}
"##, &FormatConfig {
        external_formatters: BTreeMap::from([("fake".to_string(), ExternalFormatterConfig {
            commandline: vec!["sed".to_string(), "s/a/b/g".to_string()],
            adjust_indent: false,
        })]),
        ..Default::default()
    }).unwrap();
    pretty_assertions::assert_str_eq!(r##"fn f() -> &'static str {
    #[rustfmt::external("fake")]
    r#"
       bbbbb
    "#
}
"##, res.rendered);
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
fn ow_directive_comment_external_formatter() {
    let mut external_formatters = BTreeMap::new();
    external_formatters.insert("sed".to_string(), ExternalFormatterConfig {
        commandline: vec!["sed".to_string(), "-e".to_string(), "s/hello/HELLO/".to_string()],
        adjust_indent: false,
    });
    owc(r##"fn main() {
    let x = 
        //#[rustfmt::  external("sed")]
        "hello";
}
"##, r##"fn main() {
    let x = 
        //#[rustfmt::external("sed")]
        "HELLO";
}
"##, &FormatConfig {
        external_formatters,
        ..Default::default()
    });
}
