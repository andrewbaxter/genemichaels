use {
    genemichaels_lib::{
        FormatConfig,
        format_str,
    },
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
    owc(
        r#"fn main() {
    #[rustfmt::skip] // early comment
    struct SomeStrangeIndentation { }
}
"#,
        r#"fn main() {
    // early comment
    #[rustfmt::skip]
    // early comment
    struct SomeStrangeIndentation { }
}
"#,
        &FormatConfig {
            max_width: 120,
            explicit_markdown_comments: true,
            ..Default::default()
        },
    );
}

#[test]
fn ow_rustfmt_skip_end_line_end_comment_keep() {
    owc(
        r#"fn main() {
    #[rustfmt::skip]
    struct SomeStrangeIndentation {
       } // End line end comment
}
"#,
        r#"fn main() {
    #[rustfmt::skip]
    struct SomeStrangeIndentation {
       }
    // End line end comment

}
"#,
        &FormatConfig {
            max_width: 120,
            explicit_markdown_comments: true,
            ..Default::default()
        },
    );
}

#[test]
fn ow_import_normalization_none() {
    owc(
        r#"use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
"#,
        r#"use std::collections::{
    BTreeMap,
    BTreeSet,
};
use std::fmt::Debug;
"#,
        &FormatConfig {
            import_normalization: genemichaels_lib::ImportNormalizationMode::None,
            ..Default::default()
        },
    );
}

#[test]
fn ow_import_normalization_split() {
    owc(
        r#"// Comment 1
use std::collections::{BTreeSet, BTreeMap};
"#,
        r#"// Comment 1
use std::collections::BTreeMap;

// Comment 1
use std::collections::BTreeSet;
"#,
        &FormatConfig {
            import_normalization: genemichaels_lib::ImportNormalizationMode::Split,
            ..Default::default()
        },
    );
}

#[test]
fn ow_import_normalization_combine() {
    owc(
        r#"// Comment 1
use std::collections::BTreeMap;
// Comment 1
use std::collections::BTreeSet;
"#,
        r#"// Comment 1
use std::collections::{
    BTreeMap,
    BTreeSet,
};
"#,
        &FormatConfig {
            import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
            ..Default::default()
        },
    );
}

#[test]
fn ow_import_normalization_combine_sub_diff() {
    owc(
        r#"
use std::
    // Needed for X
    collections::BTreeMap;
use std::collections::BTreeSet;
"#,
        r#"
use std::
    // Needed for X
    collections::BTreeMap;
use std::collections::BTreeSet;
"#,
        &FormatConfig {
            import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
            ..Default::default()
        },
    );
}

#[test]
fn ow_import_normalization_combine_mixed() {
    owc(
        r#"
use std::collections::BTreeMap;
pub mod x { type A = i32; }
use x::A;
"#,
        r#"
use std::collections::BTreeMap;

pub mod x {
    type A = i32;
}

use x::A;
"#,
        &FormatConfig {
            import_normalization: genemichaels_lib::ImportNormalizationMode::Combine,
            ..Default::default()
        },
    );
}
