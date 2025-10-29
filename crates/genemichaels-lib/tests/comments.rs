use {
    genemichaels_lib::{
        Comment,
        extract_whitespaces,
        CommentMode,
        HashLineColumn,
        format_md,
        Whitespace,
    },
    proc_macro2::LineColumn,
};

fn extract_whitespaces_first(keep_max_blank_lines: usize, text: &str) -> Vec<Whitespace> {
    let (mut whitespaces, tokens) = extract_whitespaces(keep_max_blank_lines, text).unwrap();
    return whitespaces.remove(&HashLineColumn(tokens.into_iter().next().unwrap().span().start())).unwrap();
}

fn eq_whitespace_text(w: &Vec<Whitespace>) -> String {
    let mut text = String::new();
    for w in w {
        match &w.mode {
            genemichaels_lib::WhitespaceMode::BlankLines(l) => {
                text.push_str(&format!("blank {}\n", l));
            },
            genemichaels_lib::WhitespaceMode::Comment(c) => {
                text.push_str(&format!("comment {:?} [{:?}]\n", c.mode, c.lines));
            },
        }
    }
    return text;
}

fn eq_whitespaces(got: Vec<Whitespace>, expect: Vec<Whitespace>) {
    let got = eq_whitespace_text(&got);
    let expect = eq_whitespace_text(&expect);
    assert_eq!(got, expect, "Mismatch:\nGot:\n{:?}\n\nExpected:\n{:?}\n", got, expect);
}

fn t(keep_max_blank_lines: usize, text: &str, expect: Vec<Whitespace>) {
    eq_whitespaces(
        extract_whitespaces_first(keep_max_blank_lines, &format!("{}\nconst THING: i32 = 7;", text)),
        expect,
    );
}

fn t_end(keep_max_blank_lines: usize, text: &str, expect: Vec<Whitespace>) {
    let (mut comments, _) = extract_whitespaces(keep_max_blank_lines, text).unwrap();
    let got = comments.remove(&HashLineColumn(LineColumn {
        line: 0,
        column: 1,
    })).unwrap();
    eq_whitespaces(got, expect);
}

/// For dummy line/cols, unused in expected data
fn loc() -> LineColumn {
    return LineColumn {
        line: 0,
        column: 0,
    };
}

fn comment(mode: CommentMode, lines: &str) -> Whitespace {
    return Whitespace {
        loc: loc(),
        mode: genemichaels_lib::WhitespaceMode::Comment(Comment {
            mode: mode,
            lines: lines.to_string(),
            orig_start_offset: 0,
        }),
    };
}

fn blanks(count: usize) -> Whitespace {
    return Whitespace {
        loc: loc(),
        mode: genemichaels_lib::WhitespaceMode::BlankLines(count),
    };
}

#[test]
fn comments_line_end1() {
    t(
        0,
        "// comment 1\nconst THING: i32 = 7; // the thing\n",
        vec![comment(CommentMode::Normal, " comment 1\n the thing")],
    );
}

#[test]
fn comments_block_outer1() {
    t(0, "/** outer1 */", vec![comment(CommentMode::DocOuter, "outer1")]);
}

#[test]
fn comments_block_empty1() {
    t(0, "/**/", vec![comment(CommentMode::Normal, "")]);
}

#[test]
fn extract_end() {
    let (comments, _) = extract_whitespaces(0, "const THING: i32 = 7;\n// The end.").unwrap();
    assert!(!comments.is_empty());
}

#[test]
fn extract_end2() {
    let (comments, _) = extract_whitespaces(0, "const THING: i32 = 7;\n// The end.\n").unwrap();
    assert!(!comments.is_empty());
}

#[test]
fn extract_blank_before_text() {
    t(5, "\n\n//x", vec![blanks(1), comment(CommentMode::Normal, "x")]);
}

#[test]
fn extract_blank_before_end() {
    t_end(5, "\n\n//x", vec![blanks(1), comment(CommentMode::Normal, "x")]);
}

#[test]
fn extract_quad_slash() {
    t(0, "//// abcd", vec![comment(CommentMode::Normal, "// abcd")]);
}

#[test]
fn from_rt_blank_keep1() {
    let got = extract_whitespaces_first(1, r#"

fn main() {
}
"#);
    eq_whitespaces(got, vec![blanks(1)]);
}

#[test]
fn format_simple_wrapping1() {
    let mut res = String::new();
    format_md(&mut res, 10, None, "", "this is very long text").unwrap();
    assert_eq!(res, "this is\nvery long\ntext");
}

#[test]
fn format_split_link1() {
    let mut res = String::new();
    format_md(&mut res, 1000, None, "__", "![this is\na broken](https://example.com)").unwrap();
    assert_eq!(res, "__![this is a broken](https://example.com)");
}

#[test]
fn format_split_link2() {
    let mut res = String::new();
    format_md(&mut res, 1000, None, "__", "[abcdabcde
abcdabc](https://example.com)
").unwrap();
    assert_eq!(res, "__[abcdabcde abcdabc](https://example.com)");
}

#[test]
fn format_split_punct_cross_inline1() {
    let mut res = String::new();
    format_md(&mut res, 10, None, "__", "abcd `abc`.").unwrap();
    assert_eq!(res, "__abcd\n__`abc`.");
}

#[test]
fn format_rel_width1() {
    let mut res = String::new();
    format_md(&mut res, 0, Some(10), "// ", "a b c d e f").unwrap();
    assert_eq!(res, "// a b c d e\n// f");
}

#[test]
fn format_overflow_use_next_break1() {
    let mut res = String::new();
    format_md(&mut res, 0, Some(10), "// ", "`abcd abcd` a b c d").unwrap();
    assert_eq!(res, "// `abcd abcd`\n// a b c d");
}
