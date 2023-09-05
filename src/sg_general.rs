use std::{
    fmt::Write,
};
use proc_macro2::{
    LineColumn,
    TokenStream,
    TokenTree,
    Punct,
};
use quote::{
    quote,
    ToTokens,
};
use syn::{
    Attribute,
    Block,
    ExprCall,
    Macro,
    token::{
        Comma,
        Or,
        Add,
        Paren,
        Brace,
        Bracket,
    },
    MacroDelimiter,
    Expr,
    Item,
    Stmt,
};
use crate::{
    whitespace::HashLineColumn,
    new_sg,
    sg_type::build_path,
    Alignment,
    Formattable,
    FormattableStmt,
    MakeSegsState,
    SplitGroupBuilder,
    MarginGroup,
    TrivialLineColMath,
    check_split_brace_threshold,
    SplitGroupIdx,
    FormattablePunct,
    sg_general_lists::{
        append_inline_list_raw,
        InlineListSuffix,
    },
    Whitespace,
    WhitespaceMode,
};

pub(crate) fn build_rev_pair(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: impl Formattable,
    right: impl Formattable,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    sg.child(base.make_segs(out, base_indent));
    sg.seg(out, " ");
    sg.child(right.make_segs(out, base_indent));
    sg.reverse_children();
    sg.build(out)
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
    tok_loc: LineColumn,
    tok: &str,
    right: impl Formattable,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    sg.child(left.make_segs(out, base_indent));
    append_whitespace(out, base_indent, &mut sg, tok_loc);
    append_binary(out, base_indent, &mut sg, tok, right);
    sg.build(out)
}

pub(crate) fn append_attr(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    bang: bool,
    attr: &Attribute,
) {
    append_whitespace(out, base_indent, sg, attr.pound_token.span.start());
    sg.child({
        let mut sg = new_sg(out);
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
        append_whitespace(out, &indent, &mut sg, attr.bracket_token.span.end().prev());
        sg.seg(out, "]");
        sg.build(out)
    });
}

pub(crate) fn append_statement_list_raw(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    attrs: Option<&Vec<Attribute>>,
    block: &Vec<impl FormattableStmt>,
) {
    if check_split_brace_threshold(out, block.len()) ||
        block.iter().any(|s| has_comments(out, s) || (s.has_attrs() && out.config.split_attributes)) {
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
            sg.split_if(out, base_indent.clone(), out.config.split_attributes, false);
        }
        append_attr(out, base_indent, sg, true, attr);
        if !out.config.split_attributes {
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
        sg.child((el).make_segs(out, base_indent));
        sg.seg_unsplit(out, " ");
        previous_margin_group = new_margin_group;
        i += 1;
    }
}

pub(crate) fn append_bracketed_statement_list(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    prefix_start: LineColumn,
    prefix: &'static str,
    attrs: Option<&Vec<Attribute>>,
    stmts: &Vec<impl FormattableStmt>,
    suffix_start: LineColumn,
) {
    if out.whitespaces.contains_key(&HashLineColumn(suffix_start)) {
        sg.initial_split();
    }
    append_whitespace(out, base_indent, sg, prefix_start);
    sg.seg(out, prefix);
    let indent = base_indent.indent();
    sg.split(out, indent.clone(), true);
    append_statement_list_raw(out, &indent, sg, attrs, stmts);
    append_whitespace(out, &indent, sg, suffix_start);
    sg.split(out, base_indent.clone(), false);
    sg.seg(out, "}");
}

pub(crate) fn new_sg_block(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    prefix_start: LineColumn,
    prefix: &'static str,
    attrs: Option<&Vec<Attribute>>,
    block: &Vec<impl FormattableStmt>,
    suffix_start: LineColumn,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    append_bracketed_statement_list(out, base_indent, &mut sg, prefix_start, prefix, attrs, block, suffix_start);
    sg.build(out)
}

pub(crate) fn new_sg_outer_attrs(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    attrs: &Vec<Attribute>,
    child: impl Formattable,
) -> SplitGroupIdx {
    if attrs.is_empty() {
        return child.make_segs(out, base_indent);
    }
    let mut sg = new_sg(out);
    for attr in attrs {
        match attr.style {
            syn::AttrStyle::Outer => { },
            syn::AttrStyle::Inner(_) => {
                continue;
            },
        };
        append_attr(out, base_indent, &mut sg, false, attr);
        if !out.config.split_attributes {
            sg.seg_unsplit(out, " ");
        }
        sg.split_if(out, base_indent.clone(), out.config.split_attributes, false);
    }
    sg.child(child.make_segs(out, base_indent));
    sg.build(out)
}

pub(crate) fn append_macro_bracketed(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    mac: &Macro,
    semi: bool,
) {
    append_macro_body_bracketed(out, base_indent, sg, &mac.delimiter, mac.tokens.clone());
    if semi {
        sg.seg(out, ";");
    }
}

pub(crate) fn new_sg_macro(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    mac: &Macro,
    semi: bool,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    sg.child(build_path(out, base_indent, &mac.path));
    sg.seg(out, "!");
    append_macro_bracketed(out, base_indent, &mut sg, mac, semi);
    sg.build(out)
}

pub(crate) fn append_macro_body_bracketed(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    delim: &MacroDelimiter,
    tokens: TokenStream,
) {
    let indent = base_indent.indent();
    match delim {
        syn::MacroDelimiter::Paren(x) => {
            sg.seg(out, "(");
            if !tokens.is_empty() || out.whitespaces.contains_key(&HashLineColumn(x.span.end().prev())) {
                sg.split(out, indent.clone(), true);
                append_macro_body(out, &indent, sg, tokens);
                append_whitespace(out, base_indent, sg, x.span.end().prev());
            }
            sg.split(out, base_indent.clone(), false);
            sg.seg(out, ")");
        },
        syn::MacroDelimiter::Brace(x) => {
            sg.seg(out, "{");
            sg.initial_split();
            if !tokens.is_empty() || out.whitespaces.contains_key(&HashLineColumn(x.span.end().prev())) {
                sg.split(out, indent.clone(), true);
                append_macro_body(out, &indent, sg, tokens);
                append_whitespace(out, base_indent, sg, x.span.end().prev());
            }
            sg.split(out, base_indent.clone(), false);
            sg.seg(out, "}");
        },
        syn::MacroDelimiter::Bracket(x) => {
            sg.seg(out, "[");
            if !tokens.is_empty() || out.whitespaces.contains_key(&HashLineColumn(x.span.end().prev())) {
                sg.split(out, indent.clone(), true);
                append_macro_body(out, &indent, sg, tokens);
                append_whitespace(out, base_indent, sg, x.span.end().prev());
            }
            sg.split(out, base_indent.clone(), false);
            sg.seg(out, "]");
        },
    }
}

pub(crate) fn append_macro_body(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    tokens: TokenStream,
) {
    if let Ok(exprs) = syn::parse2::<ExprCall>(quote!{
        f(#tokens)
    }) {
        if exprs.args.len() == 1 && matches!(exprs.args.iter().next(), Some(Expr::Verbatim(_))) {
            // not really parsed, continue
        } else {
            append_inline_list_raw(out, base_indent, sg, ",", &exprs.args, InlineListSuffix::<Expr>::None);
            return;
        }
    }
    if let Ok(block) = syn::parse2::<Block>(quote!{
        {
            #tokens
        }
    }) {
        if block.stmts.len() == 1 &&
            matches!(
                block.stmts.first(),
                Some(Stmt::Item(Item::Verbatim(_))) | Some(Stmt::Expr(Expr::Verbatim(_))) |
                    Some(Stmt::Semi(Expr::Verbatim(_), _))
            ) {
            // not really parsed, continue
        } else {
            append_statement_list_raw(out, base_indent, sg, None, &block.stmts);
            return;
        }
    }

    #[derive(PartialEq)]
    enum ConsecMode {
        // Start, joining punct (.)
        ConnectForward,
        // Idents, literals
        NoConnect,
        // Other punctuation
        Punct,
    }

    // Split token stream into "expressions" using `;` and `,` and then try to
    // re-evaluate each expression to use normal formatting.
    let mut substreams: Vec<(Vec<TokenTree>, Option<Punct>)> = vec![];
    {
        let mut top = vec![];
        for t in tokens {
            let (push, break_) = match &t {
                proc_macro2::TokenTree::Punct(p) if matches!(p.as_char(), ';' | ',') => {
                    (false, Some(Some(p.clone())))
                },
                proc_macro2::TokenTree::Group(g) if matches!(g.delimiter(), proc_macro2::Delimiter::Brace) => {
                    (true, Some(None))
                },
                _ => {
                    (true, None)
                },
            };
            if push {
                top.push(t);
            }
            if let Some(b) = break_ {
                'break_end : loop {
                    if let Some((_, suff)) = substreams.last_mut() {
                        if top.is_empty() && suff.is_none() {
                            *suff = b;
                            break 'break_end;
                        }
                    }
                    substreams.push((top.split_off(0), b));
                    break;
                }
            }
        }
        if !top.is_empty() {
            substreams.push((top, None));
        }
    }
    let substreams_len = substreams.len();
    for (i, sub) in substreams.into_iter().enumerate() {
        'nextsub : loop {
            if i > 0 {
                sg.split(out, base_indent.clone(), true);
            }
            let tokens = TokenStream::from_iter(sub.0);
            let punct = sub.1;
            if let Ok(exprs) = syn::parse2::<ExprCall>(quote!{
                f(#tokens #punct)
            }) {
                assert!(exprs.args.len() <= 1);
                if exprs.args.len() == 1 && matches!(exprs.args.iter().next(), Some(Expr::Verbatim(_))) {
                    // not really parsed, continue
                } else {
                    if let Some(e) = exprs.args.iter().next() {
                        sg.child(e.make_segs(out, base_indent));
                    }
                    if let Some(suf) = punct {
                        append_whitespace(out, base_indent, sg, suf.span().start());
                        sg.seg(out, suf);
                    }
                    break 'nextsub;
                }
            }
            if let Ok(block) = syn::parse2::<Block>(quote!{
                {
                    #tokens #punct
                }
            }) {
                if block.stmts.len() == 1 &&
                    matches!(
                        block.stmts.first(),
                        Some(Stmt::Item(Item::Verbatim(_))) | Some(Stmt::Expr(Expr::Verbatim(_))) |
                            Some(Stmt::Semi(Expr::Verbatim(_), _))
                    ) {
                    // not really parsed, continue
                } else {
                    append_statement_list_raw(out, base_indent, sg, None, &block.stmts);
                    break 'nextsub;
                }
            }
            {
                let mut mode = ConsecMode::ConnectForward;
                for t in tokens {
                    match t {
                        proc_macro2::TokenTree::Group(g) => {
                            append_whitespace(out, base_indent, sg, g.span_open().start());
                            sg.child({
                                let mut sg = new_sg(out);
                                let indent = base_indent.indent();
                                match g.delimiter() {
                                    proc_macro2::Delimiter::Parenthesis => {
                                        append_macro_body_bracketed(out, &indent, &mut sg, &MacroDelimiter::Paren({
                                            let mut delim = Paren::default();
                                            delim.span = g.span_close();
                                            delim
                                        }), g.stream());
                                    },
                                    proc_macro2::Delimiter::Brace => {
                                        match mode {
                                            ConsecMode::ConnectForward => { },
                                            _ => {
                                                sg.seg(out, " ");
                                            },
                                        }
                                        append_macro_body_bracketed(out, &indent, &mut sg, &MacroDelimiter::Brace({
                                            let mut delim = Brace::default();
                                            delim.span = g.span_close();
                                            delim
                                        }), g.stream());
                                    },
                                    proc_macro2::Delimiter::Bracket => {
                                        append_macro_body_bracketed(out, &indent, &mut sg, &MacroDelimiter::Bracket({
                                            let mut delim = Bracket::default();
                                            delim.span = g.span_close();
                                            delim
                                        }), g.stream());
                                    },
                                    proc_macro2::Delimiter::None => {
                                        // TODO needs verification
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                    },
                                }
                                sg.build(out)
                            });
                            mode = ConsecMode::NoConnect;
                        },
                        proc_macro2::TokenTree::Ident(i) => {
                            match mode {
                                ConsecMode::ConnectForward => { },
                                ConsecMode::NoConnect | ConsecMode::Punct => {
                                    sg.seg(out, " ");
                                },
                            }
                            append_whitespace(out, base_indent, sg, i.span().start());
                            sg.seg(out, &i.to_string());
                            mode = ConsecMode::NoConnect;
                        },
                        proc_macro2::TokenTree::Punct(p) => match p.as_char() {
                            '\'' | '$' | '#' => {
                                match mode {
                                    ConsecMode::ConnectForward => { },
                                    ConsecMode::NoConnect | ConsecMode::Punct => {
                                        sg.seg(out, " ");
                                    },
                                }
                                append_whitespace(out, base_indent, sg, p.span().start());
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::ConnectForward;
                            },
                            ':' => {
                                append_whitespace(out, base_indent, sg, p.span().start());
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::Punct;
                            },
                            '.' => {
                                append_whitespace(out, base_indent, sg, p.span().start());
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::ConnectForward;
                            },
                            _ => {
                                match mode {
                                    ConsecMode::ConnectForward => { },
                                    ConsecMode::NoConnect => {
                                        sg.seg(out, " ");
                                    },
                                    ConsecMode::Punct => { },
                                }
                                append_whitespace(out, base_indent, sg, p.span().start());
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::Punct;
                            },
                        },
                        proc_macro2::TokenTree::Literal(l) => {
                            match mode {
                                ConsecMode::ConnectForward => { },
                                ConsecMode::NoConnect | ConsecMode::Punct => {
                                    sg.seg(out, " ");
                                },
                            }
                            append_whitespace(out, base_indent, sg, l.span().start());
                            sg.seg(out, &l.to_string());
                            mode = ConsecMode::NoConnect;
                        },
                    }
                }
                if let Some(suf) = punct {
                    append_whitespace(out, base_indent, sg, suf.span().start());
                    sg.seg(out, suf);
                }
            }
            break;
        }
        if i < substreams_len - 1 {
            sg.seg_unsplit(out, " ");
        }
    }
}

pub(crate) fn append_whitespace(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    loc: LineColumn,
) {
    let whitespace = match out.whitespaces.remove(&HashLineColumn(loc)) {
        Some(c) => c,
        None => return,
    };
    let whitespace: Vec<Whitespace> = whitespace.into_iter().filter_map(|w| {
        match w.mode {
            WhitespaceMode::BlankLines(l) => {
                let use_lines = l.min(out.config.keep_max_blank_lines);
                if use_lines == 0 {
                    return None;
                }
                return Some(Whitespace {
                    loc: w.loc,
                    mode: WhitespaceMode::BlankLines(use_lines),
                });
            },
            WhitespaceMode::Comment(c) => {
                return Some(Whitespace {
                    loc: w.loc,
                    mode: WhitespaceMode::Comment(c),
                });
            },
        }
    }).collect();
    if whitespace.is_empty() {
        return;
    }
    sg.add(out, crate::Segment {
        node: sg.node,
        line: None,
        mode: crate::SegmentMode::All,
        content: crate::SegmentContent::Whitespace((base_indent.clone(), whitespace)),
    });
}

pub(crate) fn has_comments(out: &mut MakeSegsState, t: impl ToTokens) -> bool {
    t
        .to_token_stream()
        .into_iter()
        .next()
        .map(|t| out.whitespaces.contains_key(&HashLineColumn(t.span().start())))
        .unwrap_or(false)
}

impl FormattablePunct for Comma {
    fn span_start(&self) -> LineColumn {
        self.span.start()
    }
}

impl FormattablePunct for Or {
    fn span_start(&self) -> LineColumn {
        self.span.start()
    }
}

impl FormattablePunct for Add {
    fn span_start(&self) -> LineColumn {
        self.span.start()
    }
}
