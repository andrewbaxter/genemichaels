use genemichaels::{
    Comment,
    extract_comments,
    CommentMode,
    HashLineColumn,
    format_md,
};
use proc_macro2::LineColumn;

fn t(text: &str, expect: Vec<Comment>) {
    let (comments, tokens) = extract_comments(&format!("{}\nconst THING: i32 = 7;", text)).unwrap();
    let got = comments.get(&HashLineColumn(tokens.into_iter().next().unwrap().span().start())).unwrap();
    assert_eq!(got.len(), expect.len(), "Count mismatch:\nGot:\n{:?}\n\nExpected:\n{:?}\n", got, expect);
    for (i, (g, e)) in got.iter().zip(expect).enumerate() {
        assert!(g.lines == e.lines, "Comment {} text mismatch:\nGot:\n{}\n\nExpected:\n{}\n", i, g.lines, e.lines);
        assert!(
            g.mode == e.mode,
            "Comment {} mode mismatch, got {:?} vs expected {:?} for:\n{}",
            i,
            g.mode,
            e.mode,
            e.lines
        );
    }
}

#[test]
fn comments_line_end1() {
    t("// comment 1\nconst THING: i32 = 7; // the thing\n", vec![Comment {
        loc: LineColumn {
            line: 0,
            column: 0,
        },
        mode: CommentMode::Normal,
        lines: " comment 1\n the thing".into(),
    }]);
}

#[test]
fn comments_block_outer1() {
    t("/** outer1 */", vec![Comment {
        loc: LineColumn {
            line: 0,
            column: 0,
        },
        mode: CommentMode::DocOuter,
        lines: "outer1".into(),
    }]);
}

#[test]
fn comments_block_empty1() {
    t("/**/", vec![Comment {
        loc: LineColumn {
            line: 0,
            column: 0,
        },
        mode: CommentMode::Normal,
        lines: "".into(),
    }]);
}

#[test]
fn extract_end() {
    let (comments, _) = extract_comments("const THING: i32 = 7;\n// The end.").unwrap();
    assert!(!comments.is_empty());
}

#[test]
fn extract_end2() {
    let (comments, _) = extract_comments("const THING: i32 = 7;\n// The end.\n").unwrap();
    assert!(!comments.is_empty());
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
