use std::{
    cell::RefCell,
    fmt::Write,
    rc::Rc,
};
use proc_macro2::{
    LineColumn,
    TokenStream,
};
use quote::{
    quote,
    ToTokens,
};
use syn::{
    punctuated::Punctuated,
    Attribute,
    Block,
    ExprCall,
    Macro,
};
use crate::{
    comments::HashLineColumn,
    new_sg,
    sg_type::build_path,
    Alignment,
    Formattable,
    FormattableStmt,
    MakeSegsState,
    SplitGroup,
    SplitGroupBuilder,
    MarginGroup,
    TrivialLineColMath,
    check_split_brace_threshold,
};

pub(crate) fn build_rev_pair(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: impl Formattable,
    right: impl Formattable,
) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut node = new_sg();
    node.child(base.make_segs(out, base_indent));
    node.seg(out, " ");
    node.child(right.make_segs(out, base_indent));
    let out = node.build();
    out.borrow_mut().children.reverse();
    out
}

pub(crate) fn append_binary(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    tok: &str,
    right: impl Formattable,
) {
    node.seg(out, tok);
    let indent = base_indent.indent();
    node.split(out, indent.clone(), true);
    node.seg_unsplit(out, " ");
    node.child(right.make_segs(out, &indent));
}

pub(crate) fn new_sg_binary(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    left: impl Formattable,
    tok: &str,
    right: impl Formattable,
) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut node = new_sg();
    node.child(left.make_segs(out, base_indent));
    append_binary(out, base_indent, &mut node, tok, right);
    node.build()
}

pub(crate) fn append_attr(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    bang: bool,
    attr: &Attribute,
) {
    append_comments(out, base_indent, sg, attr.pound_token.span.start());
    sg.child({
        let mut sg = new_sg();
        let indent = base_indent.indent();
        let mut prefix = String::new();
        prefix.write_str(if bang {
            "#!["
        } else {
            "#["
        }).unwrap();
        sg.seg(out, prefix);
        sg.child(build_path(out, &indent, &attr.path));
        append_macro_body(out, &indent, &mut sg, attr.tokens.clone());
        append_comments(out, &indent, &mut sg, attr.bracket_token.span.end().prev());
        sg.seg(out, "]");
        sg.build()
    });
}

pub(crate) fn append_statement_list_raw(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    attrs: Option<&Vec<Attribute>>,
    block: &Vec<impl FormattableStmt>,
) {
    if check_split_brace_threshold(out, block.len()) {
        sg.initial_split();
    }
    sg.seg_unsplit(out, " ");
    let mut previous_margin_group = crate::MarginGroup::None;
    let mut i = 0;
    for attr in attrs.unwrap_or(&vec![]) {
        match attr.style {
            syn::AttrStyle::Outer => {
                continue;
            },
            syn::AttrStyle::Inner(_) => { },
        };
        if i > 0 {
            sg.split_if(out, base_indent.clone(), out.split_attributes, false);
        }
        append_attr(out, base_indent, sg, true, attr);
        if !out.split_attributes {
            sg.seg_unsplit(out, " ");
        }
        previous_margin_group = MarginGroup::Attr;
        i += 1;
    }
    for el in block {
        let (new_margin_group, want_margin) = el.want_margin();
        if i > 0 {
            if previous_margin_group != new_margin_group || want_margin || has_comments(out, el) {
                sg.split(out, base_indent.clone(), true);
            }
            sg.split(out, base_indent.clone(), true);
        }
        sg.child((&el).make_segs(out, &base_indent));
        sg.seg_unsplit(out, " ");
        previous_margin_group = new_margin_group;
        i += 1;
    }
}

pub(crate) fn append_bracketed_statement_list(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    prefix: &'static str,
    attrs: Option<&Vec<Attribute>>,
    stmts: &Vec<impl FormattableStmt>,
    end: LineColumn,
) {
    sg.seg(out, prefix);
    let indent = base_indent.indent();
    sg.split(out, indent.clone(), true);
    append_statement_list_raw(out, &indent, sg, attrs, stmts);
    append_comments(out, &indent, sg, end);
    sg.split(out, base_indent.clone(), false);
    sg.seg(out, "}");
}

pub(crate) fn new_sg_block(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    start: LineColumn,
    prefix: &'static str,
    block: &Vec<impl FormattableStmt>,
    end: LineColumn,
) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut sg = new_sg();
    append_comments(out, base_indent, &mut sg, start);
    append_bracketed_statement_list(out, base_indent, &mut sg, prefix, None, block, end);
    sg.build()
}

pub(crate) fn append_inline_list_raw<E: Formattable, T: >(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    punct: &str,
    punct_is_suffix: bool,
    exprs: &Punctuated<E, T>,
) {
    for (i, pair) in exprs.pairs().enumerate() {
        if i > 0 {
            node.split(out, base_indent.clone(), true);
        }
        node.child(pair.value().make_segs(out, &base_indent));
        if i < exprs.len() - 1 {
            node.seg(out, punct);
            node.seg_unsplit(out, " ");
        } else {
            if punct_is_suffix {
                node.seg_split(out, punct);
            }
        }
    }
}

pub(crate) fn append_inline_list<E: Formattable, T: >(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    punct: &str,
    punct_is_suffix: bool,
    exprs: &Punctuated<E, T>,
) {
    let indent = base_indent.indent();
    sg.split(out, indent.clone(), true);
    append_inline_list_raw(out, &indent, sg, punct, punct_is_suffix, exprs);
}

pub(crate) fn append_comma_bracketed_list<E: Formattable, T: >(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    end: LineColumn,
    suffix: &str,
) {
    // node.add_comments(out, base_indent, prefix_start);
    sg.seg(out, prefix);
    let indent = base_indent.indent();
    sg.split(out, indent.clone(), true);
    append_inline_list_raw(out, &indent, sg, ",", true, exprs);
    append_comments(out, &indent, sg, end);
    sg.split(out, base_indent.clone(), false);
    sg.seg(out, suffix);
}

pub(crate) fn new_sg_comma_bracketed_list<E: Formattable, T: >(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: Option<impl Formattable>,
    start: LineColumn,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    end: LineColumn,
    suffix: &str,
) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut sg = new_sg();
    if let Some(base) = base {
        sg.child(base.make_segs(out, base_indent));
    }
    append_comments(out, base_indent, &mut sg, start);
    append_comma_bracketed_list(out, base_indent, &mut sg, prefix, exprs, end, suffix);
    sg.build()
}

pub(crate) fn new_sg_comma_bracketed_list_ext<E: Formattable, T: >(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: Option<impl Formattable>,
    start: LineColumn,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    extra: impl Formattable,
    suffix: &str,
) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut sg = new_sg();
    if let Some(base) = base {
        sg.child(base.make_segs(out, base_indent));
    }
    append_comments(out, base_indent, &mut sg, start);
    sg.seg(out, prefix);
    let indent = base_indent.indent();
    for pair in exprs.pairs() {
        sg.split(out, indent.clone(), true);
        sg.child((&pair.value()).make_segs(out, &indent));
        sg.seg(out, ",");
        sg.seg_unsplit(out, " ");
    }
    sg.split(out, indent.clone(), true);
    sg.child(extra.make_segs(out, base_indent));
    sg.seg_unsplit(out, " ");
    sg.split(out, base_indent.clone(), false);
    sg.seg(out, suffix);
    sg.build()
}

pub(crate) fn new_sg_outer_attrs(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    attrs: &Vec<Attribute>,
    child: impl Formattable,
) -> Rc<
    RefCell<SplitGroup>,
> {
    if attrs.is_empty() {
        return child.make_segs(out, base_indent);
    }
    let mut sg = new_sg();
    for attr in attrs {
        match attr.style {
            syn::AttrStyle::Outer => { },
            syn::AttrStyle::Inner(_) => {
                continue;
            },
        };
        append_attr(out, base_indent, &mut sg, false, attr);
        if !out.split_attributes {
            sg.seg_unsplit(out, " ");
        }
        sg.split_if(out, base_indent.clone(), out.split_attributes, false);
    }
    sg.child(child.make_segs(out, base_indent));
    sg.build()
}

pub(crate) fn new_sg_macro(out: &mut MakeSegsState, base_indent: &Alignment, mac: &Macro, semi: bool) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut sg = new_sg();
    sg.child(build_path(out, base_indent, &mac.path));
    sg.seg(out, "!");
    let indent = base_indent.indent();
    match mac.delimiter {
        syn::MacroDelimiter::Paren(x) => {
            sg.seg(out, "(");
            sg.split(out, indent.clone(), true);
            append_macro_body(out, &indent, &mut sg, mac.tokens.clone());
            append_comments(out, base_indent, &mut sg, x.span.end().prev());
            sg.split(out, base_indent.clone(), false);
            sg.seg(out, ")");
        },
        syn::MacroDelimiter::Brace(x) => {
            sg.seg(out, "{");
            sg.split(out, indent.clone(), true);
            append_macro_body(out, &indent, &mut sg, mac.tokens.clone());
            append_comments(out, base_indent, &mut sg, x.span.end().prev());
            sg.split(out, base_indent.clone(), false);
            sg.seg(out, "}");
        },
        syn::MacroDelimiter::Bracket(x) => {
            sg.seg(out, "[");
            sg.split(out, indent.clone(), true);
            append_macro_body(out, &indent, &mut sg, mac.tokens.clone());
            append_comments(out, base_indent, &mut sg, x.span.end().prev());
            sg.split(out, base_indent.clone(), false);
            sg.seg(out, "]");
        },
    }
    if semi {
        sg.seg(out, ";");
    }
    sg.build()
}

pub(crate) fn append_macro_body(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    tokens: TokenStream,
) {
    if let Ok(exprs) = syn::parse2::<ExprCall>(quote!{f(# tokens)}) {
        append_inline_list_raw(out, base_indent, sg, ",", false, &exprs.args);
    } else if let Ok(block) = syn::parse2::<Block>(quote!{{# tokens}}) {
        append_statement_list_raw(out, base_indent, sg, None, &block.stmts);
    } else {
        #[derive(PartialEq)] 
        enum ConsecMode {
            // Start, joining punct (.)
            StartJoin,
            // Idents, literals
            IdentLit,
            // Other punctuation
            Punct,
        }

        let mut substreams = vec![];
        let mut top = vec![];
        for t in tokens {
            match t {
                proc_macro2::TokenTree::Punct(p) if match p.as_char() {
                    ';' | ',' => true,
                    _ => false,
                } => {
                    substreams.push((top.split_off(0), Some(p)));
                },
                _ => {
                    top.push(t);
                },
            }
        }
        if !top.is_empty() {
            substreams.push((top, None));
        }
        let substreams_len = substreams.len();
        for (i, sub) in substreams.into_iter().enumerate() {
            if i > 0 {
                sg.split(out, base_indent.clone(), true);
            }
            let tokens = TokenStream::from_iter(sub.0);
            if let Ok(exprs) = syn::parse2::<ExprCall>(quote!{f(# tokens)}) {
                append_inline_list_raw(out, base_indent, sg, ",", false, &exprs.args);
            } else if let Ok(block) = syn::parse2::<Block>(quote!{{# tokens}}) {
                append_statement_list_raw(out, base_indent, sg, None, &block.stmts);
            } else {
                let mut mode = ConsecMode::StartJoin;
                for t in tokens {
                    match t {
                        proc_macro2::TokenTree::Group(g) => {
                            append_comments(out, base_indent, sg, g.span_open().start());
                            sg.child({
                                let mut sg = new_sg();
                                let indent = base_indent.indent();
                                match g.delimiter() {
                                    proc_macro2::Delimiter::Parenthesis => {
                                        sg.seg(out, "(");
                                        sg.split(out, indent.clone(), true);
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                        append_comments(out, &indent, &mut sg, g.span_close().start());
                                        sg.split(out, base_indent.clone(), false);
                                        sg.seg(out, ")");
                                    },
                                    proc_macro2::Delimiter::Brace => {
                                        match mode {
                                            ConsecMode::StartJoin => { },
                                            _ => {
                                                sg.seg(out, " ");
                                            },
                                        }
                                        sg.seg(out, "{");
                                        sg.split(out, indent.clone(), true);
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                        append_comments(out, &indent, &mut sg, g.span_close().start());
                                        sg.split(out, base_indent.clone(), false);
                                        sg.seg(out, "}");
                                    },
                                    proc_macro2::Delimiter::Bracket => {
                                        sg.seg(out, "[");
                                        sg.split(out, indent.clone(), true);
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                        append_comments(out, &indent, &mut sg, g.span_close().start());
                                        sg.split(out, base_indent.clone(), false);
                                        sg.seg(out, "]");
                                    },
                                    proc_macro2::Delimiter::None => {
                                        // TODO needs verification
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                    },
                                }
                                sg.build()
                            });
                            mode = ConsecMode::IdentLit;
                        },
                        proc_macro2::TokenTree::Ident(i) => {
                            match mode {
                                ConsecMode::StartJoin => { },
                                ConsecMode::IdentLit | ConsecMode::Punct => {
                                    sg.seg(out, " ");
                                },
                            }
                            append_comments(out, base_indent, sg, i.span().start());
                            sg.seg(out, &i.to_string());
                            mode = ConsecMode::IdentLit;
                        },
                        proc_macro2::TokenTree::Punct(p) => match p.as_char() {
                            '\'' => {
                                match mode {
                                    ConsecMode::StartJoin => { },
                                    ConsecMode::IdentLit | ConsecMode::Punct => {
                                        sg.seg(out, " ");
                                    },
                                }
                                append_comments(out, base_indent, sg, p.span().start());
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::StartJoin;
                            },
                            _ => {
                                match mode {
                                    ConsecMode::StartJoin => { },
                                    ConsecMode::IdentLit => {
                                        sg.seg(out, " ");
                                    },
                                    ConsecMode::Punct => { },
                                }
                                append_comments(out, base_indent, sg, p.span().start());
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::Punct;
                            },
                        },
                        proc_macro2::TokenTree::Literal(l) => {
                            match mode {
                                ConsecMode::StartJoin => { },
                                ConsecMode::IdentLit | ConsecMode::Punct => {
                                    sg.seg(out, " ");
                                },
                            }
                            append_comments(out, base_indent, sg, l.span().start());
                            sg.seg(out, &l.to_string());
                            mode = ConsecMode::IdentLit;
                        },
                    }
                }
            }
            if let Some(suf) = sub.1 {
                sg.seg(out, suf);
            }
            if i < substreams_len - 1 {
                sg.seg_unsplit(out, " ");
            }
        }
    }
}

pub(crate) fn append_comments(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    loc: LineColumn,
) {
    let comments = match out.comments.remove(&HashLineColumn(loc)) {
        Some(c) => c,
        None => return,
    };
    sg.add(out, Rc::new(RefCell::new(crate::Segment{
        node: sg.node.clone(),
        line: None,
        mode: crate::SegmentMode::All,
        content: crate::SegmentContent::Comment((base_indent.clone(), comments)),
    })));
}

pub(crate) fn has_comments(out: &mut MakeSegsState, t: impl ToTokens) -> bool {
    t
        .to_token_stream()
        .into_iter()
        .next()
        .map(|t| out.comments.contains_key(&HashLineColumn(t.span().start())))
        .unwrap_or(false)
}
