use std::{
    fmt::Write,
};
use syn::{
    Expr,
    FieldPat,
    Pat,
};
use crate::{
    new_sg,
    new_sg_lit,
    sg_general::{
        new_sg_outer_attrs,
        new_sg_binary,
        new_sg_macro,
        append_whitespace,
    },
    sg_type::{
        build_extended_path,
        build_ref,
        append_path,
    },
    Alignment,
    Formattable,
    MakeSegsState,
    TrivialLineColMath,
    SplitGroupIdx,
    sg_general_lists::{
        append_inline_list_raw,
        append_bracketed_list_curly,
        new_sg_bracketed_list_common,
        append_bracketed_list_common,
        InlineListSuffix,
        new_sg_bracketed_list,
    },
    HashLineColumn,
};

impl Formattable for &Pat {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            Pat::Box(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    node.seg(out, "box ");
                    node.child(x.pat.as_ref().make_segs(out, base_indent));
                    node.build(out)
                },
            ),
            Pat::Ident(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
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
                                let t = match at.1.as_ref() {
                                    Pat::Tuple(t) => t,
                                    _ => break,
                                };
                                if t.elems.len() != 1 {
                                    break;
                                }
                                let x = match t.elems.iter().next().unwrap() {
                                    Pat::Or(x) => x,
                                    _ => break,
                                };

                                // Not actually a tuple, but an @-bind | grouping snowflake syntax. Workaround
                                // https://github.com/dtolnay/syn/issues/1352
                                let mut sg0 = new_sg(out);
                                let sg = &mut sg0;
                                let prefix_start = t.paren_token.span.start();
                                let suffix_start = t.paren_token.span.end().prev();
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
            Pat::Lit(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    x.expr.as_ref().make_segs(out, base_indent)
                },
            ),
            Pat::Macro(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &x.mac, false)
                },
            ),
            Pat::Or(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
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
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_extended_path(out, base_indent, &x.qself, &x.path)
                },
            ),
            Pat::Range(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let (tok_loc, tok) = match x.limits {
                        syn::RangeLimits::HalfOpen(x) => (x.spans[0].start(), " .."),
                        syn::RangeLimits::Closed(x) => (x.spans[0].start(), " ..="),
                    };
                    new_sg_binary(out, base_indent, x.lo.as_ref(), tok_loc, tok, x.hi.as_ref())
                },
            ),
            Pat::Reference(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_ref(out, base_indent, x.and_token.span.start(), x.mutability.is_some(), x.pat.as_ref())
                },
            ),
            Pat::Rest(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_lit(out, Some((base_indent, x.dot2_token.spans[0].start())), "..")
                },
            ),
            Pat::Slice(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_bracketed_list_common(
                        out,
                        base_indent,
                        x.bracket_token.span.start(),
                        "[",
                        &x.elems,
                        x.bracket_token.span.end().prev(),
                        "]",
                    )
                },
            ),
            Pat::Struct(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
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
                        x.brace_token.span.start(),
                        &x.fields,
                        x.dot2_token.as_ref().map(|d| {
                            |out: &mut MakeSegsState, base_indent: &Alignment| {
                                new_sg_lit(out, Some((base_indent, d.spans[0].start())), "..")
                            }
                        }),
                        x.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Pat::Tuple(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_bracketed_list(
                        out,
                        base_indent,
                        x.paren_token.span.start(),
                        "(",
                        false,
                        ",",
                        &x.elems,
                        InlineListSuffix::UnitPunct::<Expr>,
                        x.paren_token.span.end().prev(),
                        ")",
                    )
                },
            ),
            Pat::TupleStruct(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child((&x.path).make_segs(out, base_indent));
                    append_bracketed_list_common(
                        out,
                        base_indent,
                        &mut sg,
                        x.pat.paren_token.span.start(),
                        "(",
                        &x.pat.elems,
                        x.pat.paren_token.span.end().prev(),
                        ")",
                    );
                    sg.build(out)
                },
            ),
            Pat::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, x.pat.as_ref(), x.colon_token.span.start(), ":", x.ty.as_ref())
                },
            ),
            Pat::Verbatim(x) => new_sg_lit(out, None, x),
            Pat::Wild(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    new_sg_lit(out, Some((base_indent, x.underscore_token.span.start())), "_")
                },
            ),
            _ => unreachable!(),
        }
    }

    fn has_attrs(&self) -> bool {
        match self {
            Pat::Box(x) => !x.attrs.is_empty(),
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
        new_sg_outer_attrs(out, base_indent, &self.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
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
        })
    }

    fn has_attrs(&self) -> bool {
        !self.attrs.is_empty()
    }
}
