use genemichaels::{
    Comment,
    extract_comments,
    CommentMode,
    HashLineColumn,
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
