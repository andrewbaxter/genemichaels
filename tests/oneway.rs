use genemichaels::{
    FormatConfig,
    format_str,
};

fn ow(before: &str, want_after: &str) {
    let res = format_str(before, &FormatConfig {
        max_width: 120,
        ..Default::default()
    }).unwrap();
    assert!(
        want_after == res.rendered,
        "Formatted text changed:\n\nWant after:\n{}\n\nAfter:\n{}\n",
        want_after
            .lines()
            .enumerate()
            .map(|(line, text)| format!("{:>03} {}", line, text))
            .collect::<Vec<String>>()
            .join("\n"),
        res
            .rendered
            .lines()
            .enumerate()
            .map(|(line, text)| format!("{:>03} {}", line, text))
            .collect::<Vec<String>>()
            .join("\n")
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
