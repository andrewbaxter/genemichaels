use {
    crate::{
        Alignment,
        Formattable,
        HashLineColumn,
        MakeSegsState,
        SplitGroupIdx,
        new_sg,
        new_sg_lit,
        sg_general::{
            append_bracketed_statement_list,
            append_whitespace,
            new_sg_binary,
            new_sg_macro,
            new_sg_outer_attrs,
        },
        sg_general_lists::{
            InlineListSuffix,
            append_bracketed_list_common,
            append_bracketed_list_curly,
            append_inline_list_raw,
            new_sg_bracketed_list,
            new_sg_bracketed_list_common,
        },
        sg_type::{
            BuildRefMutability,
            append_path,
            build_extended_path,
            build_ref,
        },
    },
    quote::ToTokens,
    std::fmt::Write,
    syn::{
        Expr,
        FieldPat,
        Pat,
        spanned::Spanned,
    },
};

impl Formattable for &Pat {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        #[deny(clippy::wildcard_enum_match_arm)]
        match self {
            Pat::Ident(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut prefix = String::new();
                    let mut start = None;
                    if let Some(y) = x.by_ref {
                        prefix.write_str("ref ").unwrap();
                        start = Some(y.span.start());
                    }
                    if let Some(y) = x.mutability {
                        prefix.write_str("mut ").unwrap();
                        if start.is_none() {
                            start = Some(y.span.start());
                        }
                    }
                    prefix.write_str(&x.ident.to_string()).unwrap();
                    if start.is_none() {
                        start = Some(x.ident.span().start());
                    }
                    if let Some(at) = &x.subpat {
                        new_sg_binary(out, base_indent, |out: &mut MakeSegsState, base_indent: &Alignment| {
                            new_sg_lit(out, start.map(|s| (base_indent, s)), &prefix)
                        }, at.0.span.start(), " @", |out: &mut MakeSegsState, base_indent: &Alignment| {
                            loop {
                                let Pat::Tuple(t) = at.1.as_ref() else {
                                    break;
                                };
                                if t.elems.len() != 1 {
                                    break;
                                }
                                let Pat::Or(x) = t.elems.iter().next().unwrap() else {
                                    break;
                                };

                                // Not actually a tuple, but an @-bind | grouping snowflake syntax. Workaround
                                // https://github.com/dtolnay/syn/issues/1352
                                let mut sg0 = new_sg(out);
                                let sg = &mut sg0;
                                let prefix_start = t.paren_token.span.open().start();
                                let suffix_start = t.paren_token.span.close().start();
                                if out.whitespaces.contains_key(&HashLineColumn(suffix_start)) {
                                    sg.initial_split();
                                }
                                append_whitespace(out, base_indent, sg, prefix_start);
                                sg.seg(out, "(");
                                let indent = base_indent.indent();
                                let empty = x.leading_vert.is_some() || !x.cases.is_empty();
                                if empty {
                                    sg.split(out, indent.clone(), true);
                                    if let Some(v) = &x.leading_vert {
                                        append_whitespace(out, base_indent, sg, v.span.start());
                                        sg.seg(out, "| ");
                                    }
                                    append_inline_list_raw(
                                        out,
                                        base_indent,
                                        sg,
                                        " |",
                                        &x.cases,
                                        InlineListSuffix::<Expr>::None,
                                    );
                                }
                                append_whitespace(out, &indent, sg, suffix_start);
                                if !empty {
                                    sg.split(out, base_indent.clone(), false);
                                }
                                sg.seg(out, ")");
                                return sg0.build(out);
                            };

                            // A real tuple, do normal generation
                            at.1.make_segs(out, base_indent)
                        })
                    } else {
                        new_sg_lit(out, start.map(|s| (base_indent, s)), prefix)
                    }
                },
            ),
            Pat::Lit(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                self.span(),
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_whitespace(out, base_indent, &mut node, e.lit.span().start());
                    node.seg(out, e.lit.to_token_stream());
                    node.build(out)
                },
            ),
            Pat::Macro(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &x.mac, false)
                },
            ),
            Pat::Or(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    if let Some(t) = &x.leading_vert {
                        append_whitespace(out, base_indent, &mut sg, t.span.start());
                        sg.seg(out, "| ");
                    }
                    append_inline_list_raw(
                        out,
                        base_indent,
                        &mut sg,
                        " |",
                        &x.cases,
                        InlineListSuffix::<Expr>::None,
                    );
                    sg.build(out)
                },
            ),
            Pat::Path(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_extended_path(out, base_indent, &x.qself, &x.path)
                },
            ),
            Pat::Range(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let (tok, tok_loc) = match e.limits {
                        syn::RangeLimits::HalfOpen(x) => ("..", x.spans[0].start()),
                        syn::RangeLimits::Closed(x) => ("..=", x.spans[0].start()),
                    };
                    match (&e.start, &e.end) {
                        (None, None) => new_sg_lit(out, Some((base_indent, tok_loc)), tok),
                        (None, Some(r)) => {
                            let mut sg = new_sg(out);
                            append_whitespace(out, base_indent, &mut sg, tok_loc);
                            sg.seg(out, tok);
                            sg.child(r.as_ref().make_segs(out, base_indent));
                            sg.build(out)
                        },
                        (Some(l), None) => {
                            let mut sg = new_sg(out);
                            append_whitespace(out, base_indent, &mut sg, tok_loc);
                            sg.child(l.as_ref().make_segs(out, base_indent));
                            sg.seg(out, tok);
                            sg.build(out)
                        },
                        (Some(l), Some(r)) => new_sg_binary(
                            out,
                            base_indent,
                            l.as_ref(),
                            tok_loc,
                            &format!(" {}", tok),
                            r.as_ref(),
                        ),
                    }
                },
            ),
            Pat::Reference(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_ref(
                        out,
                        base_indent,
                        x.and_token.span.start(),
                        false,
                        x.mutability.map(|_| BuildRefMutability::Mut),
                        x.pat.as_ref(),
                    )
                },
            ),
            Pat::Rest(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_lit(out, Some((base_indent, x.dot2_token.spans[0].start())), "..")
                },
            ),
            Pat::Slice(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_bracketed_list_common(
                        out,
                        base_indent,
                        x.bracket_token.span.open().start(),
                        "[",
                        &x.elems,
                        x.bracket_token.span.close().start(),
                        "]",
                    )
                },
            ),
            Pat::Struct(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_path(
                        out,
                        &mut sg,
                        base_indent,
                        x.path.leading_colon.map(|t| Some(t.spans[0].start())),
                        x.path.segments.pairs(),
                    );
                    append_bracketed_list_curly(
                        out,
                        base_indent,
                        &mut sg,
                        x.brace_token.span.open().start(),
                        &x.fields,
                        x.rest.as_ref().map(|d| {
                            |out: &mut MakeSegsState, base_indent: &Alignment| {
                                new_sg_lit(out, Some((base_indent, d.dot2_token.spans[0].start())), "..")
                            }
                        }),
                        x.brace_token.span.close().start(),
                    );
                    sg.build(out)
                },
            ),
            Pat::Tuple(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_bracketed_list(
                        out,
                        base_indent,
                        x.paren_token.span.open().start(),
                        "(",
                        false,
                        ",",
                        &x.elems,
                        InlineListSuffix::UnitPunct::<Expr>,
                        x.paren_token.span.close().start(),
                        ")",
                    )
                },
            ),
            Pat::TupleStruct(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child((&x.path).make_segs(out, base_indent));
                    append_bracketed_list_common(
                        out,
                        base_indent,
                        &mut sg,
                        x.paren_token.span.open().start(),
                        "(",
                        &x.elems,
                        x.paren_token.span.close().start(),
                        ")",
                    );
                    sg.build(out)
                },
            ),
            Pat::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, x.pat.as_ref(), x.colon_token.span.start(), ":", x.ty.as_ref())
                },
            ),
            Pat::Verbatim(x) => new_sg_lit(out, None, x),
            Pat::Wild(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                self.span(),
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    new_sg_lit(out, Some((base_indent, x.underscore_token.span.start())), "_")
                },
            ),
            Pat::Const(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_whitespace(out, base_indent, &mut sg, e.const_token.span.start());
                    sg.seg(out, "const");
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        e.block.brace_token.span.open().start(),
                        " {",
                        Some(&e.attrs),
                        &e.block.stmts,
                        e.block.brace_token.span.close().start(),
                    );
                    sg.build(out)
                },
            ),
            Pat::Paren(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                self.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_whitespace(out, base_indent, &mut sg, e.paren_token.span.open().start());
                    sg.seg(out, "(");
                    sg.child(e.pat.make_segs(out, base_indent));
                    append_whitespace(out, base_indent, &mut sg, e.paren_token.span.close().start());
                    sg.seg(out, ")");
                    sg.build(out)
                },
            ),
            _ => unreachable!(),
        }
    }

    fn has_attrs(&self) -> bool {
        #[deny(clippy::wildcard_enum_match_arm)]
        match self {
            Pat::Ident(x) => !x.attrs.is_empty(),
            Pat::Lit(x) => !x.attrs.is_empty(),
            Pat::Macro(x) => !x.attrs.is_empty(),
            Pat::Or(x) => !x.attrs.is_empty(),
            Pat::Path(x) => !x.attrs.is_empty(),
            Pat::Range(x) => !x.attrs.is_empty(),
            Pat::Reference(x) => !x.attrs.is_empty(),
            Pat::Rest(x) => !x.attrs.is_empty(),
            Pat::Slice(x) => !x.attrs.is_empty(),
            Pat::Struct(x) => !x.attrs.is_empty(),
            Pat::Tuple(x) => !x.attrs.is_empty(),
            Pat::TupleStruct(x) => !x.attrs.is_empty(),
            Pat::Type(x) => !x.attrs.is_empty(),
            Pat::Verbatim(_) => false,
            Pat::Wild(x) => !x.attrs.is_empty(),
            Pat::Const(x) => !x.attrs.is_empty(),
            Pat::Paren(x) => !x.attrs.is_empty(),
            _ => unreachable!(),
        }
    }
}

impl Formattable for Pat {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        (&self).make_segs(out, base_indent)
    }

    fn has_attrs(&self) -> bool {
        (&self).has_attrs()
    }
}

impl Formattable for FieldPat {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_outer_attrs(
            out,
            base_indent,
            &self.attrs,
            self.span(),
            |out: &mut MakeSegsState, base_indent: &Alignment| {
                let mut sg = new_sg(out);
                if let Some(col) = &self.colon_token {
                    match &self.member {
                        syn::Member::Named(x) => sg.seg(out, x),
                        syn::Member::Unnamed(x) => sg.seg(out, x.index),
                    };
                    append_whitespace(out, base_indent, &mut sg, col.span.start());
                    sg.seg(out, ": ");
                }
                sg.child(self.pat.make_segs(out, base_indent));
                sg.build(out)
            },
        )
    }

    fn has_attrs(&self) -> bool {
        !self.attrs.is_empty()
    }
}
