use {
    crate::{
        Alignment,
        Formattable,
        FormattablePunct,
        FormattableStmt,
        IncMacroDepth,
        MakeSegsState,
        MarginGroup,
        SplitGroupBuilder,
        SplitGroupIdx,
        Whitespace,
        WhitespaceMode,
        check_split_brace_threshold,
        new_sg,
        sg_general_lists::{
            InlineListSuffix,
            append_inline_list_raw,
        },
        sg_type::build_path,
        whitespace::HashLineColumn,
    },
    proc_macro2::{
        LineColumn,
        Punct,
        TokenStream,
        TokenTree,
    },
    quote::{
        ToTokens,
        quote,
    },
    std::{
        fmt::Write,
        ops::{
            Bound,
            RangeBounds,
        },
    },
    syn::{
        Attribute,
        Block,
        Expr,
        ExprCall,
        Item,
        Macro,
        MacroDelimiter,
        Stmt,
        spanned::Spanned,
        token::{
            Brace,
            Bracket,
            Comma,
            Or,
            Paren,
            Plus,
        },
    },
};

pub(crate) fn append_attr(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    attr: &Attribute,
) {
    append_whitespace(out, base_indent, sg, attr.pound_token.span.start());
    sg.child({
        let mut sg = new_sg(out);
        let indent = base_indent.indent();
        let mut prefix = String::new();
        prefix.write_str(match &attr.style {
            syn::AttrStyle::Outer => "#[",
            syn::AttrStyle::Inner(_) => "#![",
        }).unwrap();
        sg.seg(out, prefix);
        match &attr.meta {
            syn::Meta::Path(m) => {
                sg.child(build_path(out, &indent, m));
            },
            syn::Meta::List(m) => {
                sg.child(build_path(out, &indent, &m.path));
                let _in_macro = IncMacroDepth::new(out);
                append_macro_body_bracketed(out, base_indent, &mut sg, &m.delimiter, m.tokens.clone());
            },
            syn::Meta::NameValue(m) => {
                sg.child(
                    new_sg_binary(
                        out,
                        base_indent,
                        &m.path,
                        m.eq_token.span.start(),
                        &format!(" {}", m.eq_token.to_token_stream()),
                        &m.value,
                    ),
                );
            },
        }
        append_whitespace(out, &indent, &mut sg, attr.bracket_token.span.close().start());
        sg.seg(out, "]");
        sg.build(out)
    });
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
    if stmts.is_empty() && !out.whitespaces.contains_key(&HashLineColumn(suffix_start)) &&
        !out.block_whitespaces.contains_key(&HashLineColumn(prefix_start)) &&
        !attrs.is_some_and(|attrs| attrs.iter().any(|a| matches!(a.style, syn::AttrStyle::Inner(_)))) {
        sg.seg_unsplit(out, " ");
        sg.split(out, base_indent.clone(), false);
        sg.seg(out, "}");
        return;
    }
    let indent = base_indent.indent();
    sg.split(out, indent.clone(), true);
    append_statement_list_raw(out, &indent, sg, Some(prefix_start), attrs, stmts);
    append_whitespace(out, &indent, sg, suffix_start);
    sg.split(out, base_indent.clone(), false);
    sg.seg(out, "}");
}

fn macro_parse_lossless(parsed: &impl ToTokens, original: &TokenStream) -> bool {
    fn flatten(ts: TokenStream, out: &mut Vec<String>) {
        for t in ts {
            match t {
                TokenTree::Group(g) => {
                    out.push(format!("{:?}", g.delimiter()));
                    flatten(g.stream(), out);
                    out.push("end".to_string());
                },
                TokenTree::Punct(p) => out.push(p.as_char().to_string()),
                t => {
                    let t = t.to_string();
                    if out.last().is_some_and(|l| l == ".") {
                        if let Some((a, b)) = t.split_once('.') {
                            if !a.is_empty() && !b.is_empty() && a.chars().all(|c| c.is_ascii_digit()) &&
                                b.chars().all(|c| c.is_ascii_digit()) {
                                out.push(a.to_string());
                                out.push(".".to_string());
                                out.push(b.to_string());
                                continue;
                            }
                        }
                    }
                    out.push(t);
                },
            }
        }
    }

    let mut parsed_tokens = vec![];
    flatten(parsed.to_token_stream(), &mut parsed_tokens);
    let mut original_tokens = vec![];
    flatten(original.clone(), &mut original_tokens);
    return parsed_tokens == original_tokens;
}

pub(crate) fn append_macro_body(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    tokens: TokenStream,
) {
    let wrapped = quote!{
        f(#tokens)
    };
    if let Ok(exprs) = syn::parse2::<ExprCall>(wrapped.clone()) {
        if (exprs.args.len() == 1 && matches!(exprs.args.iter().next(), Some(Expr::Verbatim(_)))) ||
            !macro_parse_lossless(&exprs, &wrapped) {
            // not really parsed, continue
        } else {
            append_inline_list_raw(out, base_indent, sg, ",", &exprs.args, if out.macro_depth.get() == 0 {
                InlineListSuffix::<Expr>::Punct
            } else {
                InlineListSuffix::<Expr>::VerbatimPunct
            });
            return;
        }
    }
    let wrapped = quote!{
        {
            #tokens
        }
    };
    if let Ok(block) = syn::parse2::<Block>(wrapped.clone()) {
        if (block.stmts.len() == 1 &&
            matches!(
                block.stmts.first(),
                Some(Stmt::Item(Item::Verbatim(_))) | Some(Stmt::Expr(Expr::Verbatim(_), _))
            )) ||
            !macro_parse_lossless(&block, &wrapped) {
            // not really parsed, continue
        } else {
            append_statement_list_raw(out, base_indent, sg, None, None, &block.stmts);
            return;
        }
    }

    // Split token stream into "expressions" (/substream) using `;` and `,` and then
    // try to format each expression.
    let mut substreams: Vec<(Vec<TokenTree>, Option<Punct>)> = vec![];
    {
        let mut top = vec![];
        for t in tokens {
            let (push, break_) = match &t {
                TokenTree::Punct(p) if matches!(p.as_char(), ';' | ',') => {
                    (false, Some(Some(p.clone())))
                },
                TokenTree::Group(g) if matches!(g.delimiter(), proc_macro2::Delimiter::Brace) => {
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
            let wrapped = quote!{
                f(#tokens #punct)
            };
            if let Ok(exprs) = syn::parse2::<ExprCall>(wrapped.clone()) {
                assert!(exprs.args.len() <= 1);
                if (exprs.args.len() == 1 && matches!(exprs.args.iter().next(), Some(Expr::Verbatim(_)))) ||
                    !macro_parse_lossless(&exprs, &wrapped) {
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
            let wrapped = quote!{
                {
                    #tokens #punct
                }
            };
            if let Ok(block) = syn::parse2::<Block>(wrapped.clone()) {
                if (block.stmts.len() == 1 &&
                    matches!(
                        block.stmts.first(),
                        Some(Stmt::Item(Item::Verbatim(_))) | Some(Stmt::Expr(Expr::Verbatim(_), _))
                    )) ||
                    !macro_parse_lossless(&block, &wrapped) {
                    // not really parsed, continue
                } else {
                    append_statement_list_raw(out, base_indent, sg, None, None, &block.stmts);
                    break 'nextsub;
                }
            }

            // Freeform formatting
            {
                /// Identify punctuation that connects things tightly
                fn is_pull_next_punct(p: &Punct) -> bool {
                    return match p.as_char() {
                        '.' => true,
                        '\'' => true,
                        '$' => true,
                        '#' => true,
                        _ => false,
                    };
                }

                // With exceptions, the default heterogeneous adjacent token tree behavior is to
                // push. For punctuation-adjacent, it depends on the punctuation type.
                fn is_hetero_push_next(prev: &Option<TokenTree>) -> bool {
                    return match &prev {
                        Some(prev) => match prev {
                            TokenTree::Group(_) => true,
                            TokenTree::Ident(_) | TokenTree::Literal(_) => true,
                            TokenTree::Punct(punct) => !is_pull_next_punct(&punct),
                        },
                        None => false,
                    };
                }

                let mut previous: Option<TokenTree> = None;
                for t in tokens {
                    match &t {
                        TokenTree::Group(g) => {
                            append_whitespace(out, base_indent, sg, g.span_open().start());
                            sg.child({
                                let mut sg = new_sg(out);
                                let indent = base_indent.indent();
                                match g.delimiter() {
                                    proc_macro2::Delimiter::Parenthesis => {
                                        append_macro_body_bracketed(out, &indent, &mut sg, &MacroDelimiter::Paren({
                                            let mut delim = Paren::default();
                                            delim.span = g.delim_span();
                                            delim
                                        }), g.stream());
                                    },
                                    proc_macro2::Delimiter::Brace => {
                                        if is_hetero_push_next(&previous) {
                                            sg.seg(out, " ");
                                        }
                                        append_macro_body_bracketed(out, &indent, &mut sg, &MacroDelimiter::Brace({
                                            let mut delim = Brace::default();
                                            delim.span = g.delim_span();
                                            delim
                                        }), g.stream());
                                    },
                                    proc_macro2::Delimiter::Bracket => {
                                        append_macro_body_bracketed(out, &indent, &mut sg, &MacroDelimiter::Bracket({
                                            let mut delim = Bracket::default();
                                            delim.span = g.delim_span();
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
                        },
                        TokenTree::Ident(i) => {
                            if is_hetero_push_next(&previous) {
                                sg.seg(out, " ");
                            }
                            append_whitespace(out, base_indent, sg, i.span().start());
                            sg.seg(out, &i.to_string());
                        },
                        TokenTree::Punct(p) => {
                            if match &previous {
                                Some(previous) => match previous {
                                    TokenTree::Literal(l) if
                                        p.as_char() == '.' &&
                                            l.to_string().starts_with(|c: char| c.is_ascii_digit()) => {
                                        l.span().end() != p.span().start()
                                    },
                                    TokenTree::Group(_) |
                                    TokenTree::Ident(_) |
                                    TokenTree::Literal(_) => match p.as_char() {
                                        ':' => false,
                                        '.' => false,
                                        _ => true,
                                    },
                                    TokenTree::Punct(prev_p) => prev_p.span().end() != p.span().start(),
                                },
                                None => false,
                            } {
                                sg.seg(out, " ");
                            }
                            append_whitespace(out, base_indent, sg, p.span().start());
                            sg.seg(out, &p.to_string());
                        },
                        TokenTree::Literal(l) => {
                            if is_hetero_push_next(&previous) {
                                sg.seg(out, " ");
                            }
                            append_whitespace(out, base_indent, sg, l.span().start());
                            sg.seg(out, &l.to_string());
                        },
                    }
                    previous = Some(t);
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

pub(crate) fn append_macro_body_bracketed(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    delim: &MacroDelimiter,
    tokens: TokenStream,
) {
    let indent = base_indent.indent();
    let (open, close, span) = match delim {
        syn::MacroDelimiter::Paren(x) => ("(", ")", x.span),
        syn::MacroDelimiter::Brace(x) => {
            sg.initial_split();
            ("{", "}", x.span)
        },
        syn::MacroDelimiter::Bracket(x) => ("[", "]", x.span),
    };
    sg.seg(out, open);
    let block_whitespaces = out.block_whitespaces.remove(&HashLineColumn(span.open().start()));
    let close_whitespace = out.whitespaces.contains_key(&HashLineColumn(span.close().start()));
    if close_whitespace {
        sg.initial_split();
    }
    if block_whitespaces.is_some() || !tokens.is_empty() || close_whitespace {
        sg.split(out, indent.clone(), true);
        if let Some(block_whitespaces) = block_whitespaces {
            sg.initial_split();
            append_whitespace_list(out, &indent, sg, block_whitespaces);
        }
        append_macro_body(out, &indent, sg, tokens);
        append_whitespace(out, &indent, sg, span.close().start());
    }
    sg.split(out, base_indent.clone(), false);
    sg.seg(out, close);
}

pub(crate) fn append_macro_bracketed(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    mac: &Macro,
    semi: bool,
) {
    let _in_macro = if !is_normal_rust_macro(mac) {
        Some(IncMacroDepth::new(out))
    } else {
        None
    };
    append_whitespace(out, base_indent, sg, mac.delimiter.span().open().start());
    append_macro_body_bracketed(out, base_indent, sg, &mac.delimiter, mac.tokens.clone());
    if semi {
        sg.seg(out, ";");
    }
}

pub(crate) fn append_statement_list_raw(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    block_whitespaces_key: Option<LineColumn>,
    attrs: Option<&Vec<Attribute>>,
    block: &Vec<impl FormattableStmt>,
) {
    let block_whitespaces = block_whitespaces_key.and_then(|k| out.block_whitespaces.remove(&HashLineColumn(k)));
    if block_whitespaces.is_some() || check_split_brace_threshold(out, block.len()) ||
        block.iter().any(|s| has_comments(out, s) || (s.has_attrs() && out.config.split_attributes)) {
        sg.initial_split();
    }
    sg.seg_unsplit(out, " ");
    if let Some(block_whitespaces) = block_whitespaces {
        append_whitespace_list(out, base_indent, sg, block_whitespaces);
    }
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
        out.record_element(attr.span());
        append_attr(out, base_indent, sg, attr);
        if !out.config.split_attributes {
            sg.seg_unsplit(out, " ");
        }
        previous_margin_group = MarginGroup::Attr;
        i += 1;
    }
    for el in block {
        let (new_margin_group, want_margin) = el.want_margin();
        let mut tentative_margin = None;
        if i > 0 {
            if previous_margin_group != new_margin_group || want_margin || has_comments(out, el) {
                sg.split(out, base_indent.clone(), true);
            } else {
                sg.split(out, base_indent.clone(), true);
                tentative_margin = Some(out.segs.len() - 1);
            }
            sg.split(out, base_indent.clone(), true);
        }
        out.record_element(el.span());
        let child = (el).make_segs(out, base_indent);
        if let Some(margin_i) = tentative_margin {
            let starts_with_comment = out.segs.get(margin_i + 2).is_some_and(|s| match &s.content {
                crate::SegmentContent::Whitespace((_, ws)) => ws
                    .iter()
                    .any(|w| matches!(w.mode, WhitespaceMode::Comment(_))),
                _ => false,
            });
            if !starts_with_comment {
                out.segs[margin_i].content = crate::SegmentContent::Text(String::new());
            }
        }
        sg.child(child);
        sg.seg_unsplit(out, " ");
        previous_margin_group = new_margin_group;
        i += 1;
    }
}

pub(crate) fn append_whitespace(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    loc: LineColumn,
) {
    let hl = HashLineColumn(loc);
    out.whitespace_anchors.insert(hl);
    let whitespace = match out.whitespaces.entry(hl) {
        std::collections::btree_map::Entry::Occupied(mut e) => {
            if e.get().0 > 1 {
                e.get_mut().0 -= 1;
                e.get().1.clone()
            } else {
                e.remove().1
            }
        },
        std::collections::btree_map::Entry::Vacant(_) => {
            return;
        },
    };
    append_whitespace_list(out, base_indent, sg, whitespace);
}

pub(crate) fn append_whitespace_list(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    whitespace: Vec<Whitespace>,
) {
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

    fn merge(previous: &mut Vec<Whitespace>, whitespace: Vec<Whitespace>) {
        for ws in whitespace {
            if let (Some(WhitespaceMode::Comment(previous)), WhitespaceMode::Comment(comment)) =
                (previous.last_mut().map(|w| &mut w.mode), &ws.mode) {
                if previous.mode == comment.mode {
                    previous.lines.push_str("\n");
                    previous.lines.push_str(&comment.lines);
                    continue;
                }
            }
            previous.push(ws);
        }
    }

    if let Some(crate::SegmentContent::Whitespace((_, previous))) = out.segs.last_mut().map(|s| &mut s.content) {
        merge(previous, whitespace);
        return;
    }
    let mut merged = vec![];
    merge(&mut merged, whitespace);
    let whitespace = merged;
    sg.add(out, crate::Segment {
        node: sg.node,
        line: None,
        mode: crate::SegmentMode::All,
        content: crate::SegmentContent::Whitespace((base_indent.clone(), whitespace)),
    });
}

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

pub(crate) fn has_comments(out: &mut MakeSegsState, t: impl ToTokens) -> bool {
    t
        .to_token_stream()
        .into_iter()
        .next()
        .map(|t| out.whitespaces.contains_key(&HashLineColumn(t.span().start())))
        .unwrap_or(false)
}

pub(crate) fn index_text(index: &syn::Index) -> String {
    if let Some(text) = index.span.source_text() {
        if !text.is_empty() && text.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
            return text;
        }
    }
    return index.index.to_string();
}

pub(crate) fn has_directive_comment(
    whitespaces: &std::collections::BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
    loc: LineColumn,
    directive: &str,
) -> bool {
    let hl = HashLineColumn(loc);
    let Some((_, ws_list)) = whitespaces.get(&hl) else {
        return false;
    };
    ws_list.iter().any(|ws| {
        let crate::WhitespaceMode::Comment(comment) = &ws.mode else {
            return false;
        };
        comment.mode == crate::CommentMode::Directive && comment.lines.lines().any(|line| line.trim() == directive)
    })
}

pub(crate) fn has_genem_skip_comment(out: &MakeSegsState, loc: LineColumn) -> bool {
    has_directive_comment(&out.whitespaces, loc, "genemichaels-skip")
}

fn is_normal_rust_macro(mac: &Macro) -> bool {
    mac.path.leading_colon.is_none() && mac.path.segments.len() == 1 &&
        mac
            .path
            .segments
            .first()
            .is_some_and(|s| s.ident == "vec" && matches!(s.arguments, syn::PathArguments::None))
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

pub(crate) fn new_sg_macro(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    mac: &Macro,
    semi: bool,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    sg.child(build_path(out, base_indent, &mac.path));
    append_whitespace(out, base_indent, &mut sg, mac.bang_token.span.start());
    sg.seg(out, "!");
    append_macro_bracketed(out, base_indent, &mut sg, mac, semi);
    sg.build(out)
}

pub(crate) fn new_sg_outer_attrs(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    attrs: &Vec<Attribute>,
    span_including_attrs: impl Fn() -> proc_macro2::Span,
    child: impl Formattable,
) -> SplitGroupIdx {
    if attrs.is_empty() && !out.has_genem_skip_comments {
        return child.make_segs(out, base_indent);
    }
    let span_including_attrs = span_including_attrs();
    let skip_comment = has_genem_skip_comment(out, span_including_attrs.start());
    if attrs.is_empty() && !skip_comment {
        return child.make_segs(out, base_indent);
    }
    let is_outer = |a: &&Attribute| matches!(a.style, syn::AttrStyle::Outer);
    let skip_fmt =
        skip_comment ||
            attrs
                .iter()
                .filter(is_outer)
                .any(|a| a.meta.to_token_stream().to_string().contains("rustfmt :: skip"));
    let skip_text = if skip_fmt {
        span_including_attrs.source_text()
    } else {
        None
    };
    let verbatim_start = match (&skip_text, attrs.iter().filter(is_outer).last()) {
        (Some(text), Some(attr)) => {
            let span_start = span_including_attrs.span().byte_range().start;
            let mut offset = attr.span().byte_range().end;
            let mut loc = attr.span().end();
            loop {
                let rest = &text[offset - span_start..];
                let skip = if rest.starts_with("//") {
                    rest.find('\n').unwrap_or(rest.len())
                } else if rest.starts_with("/*") {
                    let mut nesting = 0usize;
                    let mut i = 0usize;
                    while i < rest.len() {
                        if rest[i..].starts_with("/*") {
                            nesting += 1;
                            i += 2;
                        } else if rest[i..].starts_with("*/") {
                            nesting -= 1;
                            i += 2;
                            if nesting == 0 {
                                break;
                            }
                        } else {
                            i += rest[i..].chars().next().unwrap().len_utf8();
                        }
                    }
                    i
                } else if let Some(c) = rest.chars().next().filter(|c| c.is_whitespace()) {
                    c.len_utf8()
                } else {
                    break;
                };
                for c in rest[..skip].chars() {
                    if c == '\n' {
                        loc.line += 1;
                        loc.column = 0;
                    } else {
                        loc.column += 1;
                    }
                }
                offset += skip;
            }
            Some((offset, loc))
        },
        _ => None,
    };
    let mut sg = new_sg(out);
    for attr in attrs.iter().filter(is_outer) {
        if let Some((verbatim_offset, _)) = verbatim_start {
            let span_end = span_including_attrs.byte_range().end;
            if let Some((_, ws)) = out.whitespaces.get_mut(&HashLineColumn(attr.span().start())) {
                ws.retain(|w| match &w.mode {
                    WhitespaceMode::Comment(c) => c.orig_start_offset < verbatim_offset ||
                        c.orig_start_offset >= span_end,
                    WhitespaceMode::BlankLines(_) => true,
                });
            }
        }
        append_attr(out, base_indent, &mut sg, attr);
        if !out.config.split_attributes {
            sg.seg_unsplit(out, " ");
        }
        sg.split_if(out, base_indent.clone(), out.config.split_attributes, false);
    }
    'child: {
        'to_noskip_fmt: {
            let Some(text) = skip_text else {
                break 'to_noskip_fmt;
            };
            let after_attr_byte_offset;
            let after_attr_line_col;
            if let Some((offset, loc)) = verbatim_start {
                let attr = attrs.iter().filter(is_outer).last().unwrap();
                out.whitespace_anchors.insert(HashLineColumn(loc));
                if let Some((_, ws)) = out.whitespaces.remove(&HashLineColumn(loc)) {
                    let span_end = span_including_attrs.byte_range().end;
                    let ws = ws.into_iter().filter(|w| match &w.mode {
                        WhitespaceMode::Comment(c) => c.orig_start_offset < offset || c.orig_start_offset >= span_end,
                        WhitespaceMode::BlankLines(_) => true,
                    }).collect();
                    append_whitespace_list(out, base_indent, &mut sg, ws);
                }
                after_attr_byte_offset = offset;
                after_attr_line_col = Bound::Excluded(HashLineColumn(attr.span().end()));
            } else {
                append_whitespace(out, base_indent, &mut sg, span_including_attrs.start());
                after_attr_byte_offset = span_including_attrs.span().byte_range().start;
                after_attr_line_col = Bound::Excluded(HashLineColumn(span_including_attrs.start()));
            }
            sg.seg(out, &text[after_attr_byte_offset - span_including_attrs.span().byte_range().start..]);

            // Remove comments used within subtree from global block
            let remove_block_whitespaces_keys =
                out
                    .block_whitespaces
                    .range((after_attr_line_col, Bound::Included(HashLineColumn(span_including_attrs.end()))))
                    .map(|x| *x.0)
                    .collect::<Vec<_>>();
            for k in remove_block_whitespaces_keys {
                out.block_whitespaces.remove(&k);
            }
            let remove_whitespaces_keys =
                out
                    .whitespaces
                    .range((after_attr_line_col, Bound::Included(HashLineColumn(span_including_attrs.end()))))
                    .map(|x| *x.0)
                    .collect::<Vec<_>>();
            for k in remove_whitespaces_keys {
                let (_, ws) = out.whitespaces.remove(&k).unwrap();
                let after: Vec<Whitespace> = ws.into_iter().filter(|w| match &w.mode {
                    WhitespaceMode::BlankLines(_) => false,
                    WhitespaceMode::Comment(comment) => !match span_including_attrs.byte_range().end_bound() {
                        Bound::Included(x) => comment.orig_start_offset <= *x,
                        Bound::Excluded(x) => comment.orig_start_offset < *x,
                        Bound::Unbounded => unreachable!(),
                    },
                }).collect();
                if !after.is_empty() {
                    out.whitespaces.insert(k, (1, after));
                }
            }
            break 'child;
        }
        sg.child(child.make_segs(out, base_indent));
    }
    sg.build(out)
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

impl FormattablePunct for Plus {
    fn span_start(&self) -> LineColumn {
        self.span.start()
    }
}
