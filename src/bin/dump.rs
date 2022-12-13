use std::fs;
use proc_macro2::{TokenStream, LineColumn};
use quote::ToTokens;
use syn::{File};

fn main() {
    let f: File = syn::parse_str(&String::from_utf8(fs::read("src/lib.rs").unwrap()).unwrap()).unwrap();
    let mut previous = LineColumn {line: 1, column: 0};
    recurse(&mut previous, 0, f.to_token_stream());
}

fn p(previous: &mut LineColumn, depth: usize, d: impl std::fmt::Debug, s: LineColumn) {
    println!("{}{:?} - {}:{}", " ".repeat(depth * 4), d, s.line, s.column);
    if previous.line > s.line || (previous.line == s.line && previous.column > s.column) { panic!("backwards!"); }
    *previous = s;
}

fn recurse(previous: &mut LineColumn, depth: usize, ts: TokenStream) {
    for t in ts { match t { proc_macro2::TokenTree::Group(x) => {
        let (start, end) =
            match x.delimiter() {
                proc_macro2::Delimiter::Parenthesis => ("(", ")"),
                proc_macro2::Delimiter::Brace => ("{", "}"),
                proc_macro2::Delimiter::Bracket => ("[", "]"),
                proc_macro2::Delimiter::None => ("\"\"", "\"\""),
            };
        p(previous, depth, start, x.span_open().start());
        p(previous, depth, start, x.span_open().end());
        recurse(previous, depth + 1, x.stream());
        p(previous, depth, end, x.span_close().start());
        p(previous, depth, end, x.span_close().end());
    }, proc_macro2::TokenTree::Ident(x) => {
        p(previous, depth, &x, x.span().start());
        p(previous, depth, &x, x.span().end());
    }, proc_macro2::TokenTree::Punct(x) => {
        p(previous, depth, &x, x.span().start());
        p(previous, depth, &x, x.span().end());
    }, proc_macro2::TokenTree::Literal(x) => {
        p(previous, depth, &x, x.span().start());
        p(previous, depth, &x, x.span().end());
    } } }
}
