use std::{
    fmt::Write,
};
use proc_macro2::LineColumn;
use quote::ToTokens;
use syn::{
    AngleBracketedGenericArguments,
    BareFnArg,
    Expr,
    FnArg,
    GenericArgument,
    GenericParam,
    Generics,
    LifetimeParam,
    Path,
    QSelf,
    ReturnType,
    Token,
    Type,
    TypeParamBound,
    WhereClause,
    WherePredicate,
};
use crate::{
    new_sg,
    new_sg_lit,
    sg_general::{
        append_binary,
        append_whitespace,
        new_sg_outer_attrs,
        new_sg_binary,
        new_sg_macro,
    },
    Alignment,
    Formattable,
    MakeSegsState,
    SplitGroupBuilder,
    SplitGroupIdx,
    sg_general_lists::{
        append_inline_list,
        new_sg_bracketed_list_common,
        append_bracketed_list_common,
        new_sg_bracketed_list,
        InlineListSuffix,
    },
};

pub(crate) fn build_extended_path(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    qself: &Option<QSelf>,
    p: &Path,
) -> SplitGroupIdx {
    let mut node = new_sg(out);
    match qself {
        Some(qself) => {
            append_whitespace(out, base_indent, &mut node, qself.lt_token.span.start());
            node.seg(out, "<");
            let taken = match qself.position {
                0 => {
                    node.child(qself.ty.make_segs(out, base_indent));
                    0
                },
                n => {
                    node.child(qself.ty.make_segs(out, base_indent));
                    node.seg(out, " as ");
                    append_path(
                        out,
                        &mut node,
                        base_indent,
                        p.leading_colon.map(|t| Some(t.spans[0].start())),
                        p.segments.pairs().take(n),
                    );
                    n
                },
            };
            append_whitespace(out, base_indent, &mut node, qself.gt_token.span.start());
            node.seg(out, ">");
            append_path(out, &mut node, base_indent, Some(None), p.segments.pairs().skip(taken));
        },
        None => {
            append_path(
                out,
                &mut node,
                base_indent,
                p.leading_colon.map(|t| Some(t.spans[0].start())),
                p.segments.pairs(),
            );
        },
    };
    node.build(out)
}

pub(crate) fn build_path(out: &mut MakeSegsState, base_indent: &Alignment, path: &syn::Path) -> SplitGroupIdx {
    let mut node = new_sg(out);
    append_path(
        out,
        &mut node,
        base_indent,
        path.leading_colon.map(|t| Some(t.spans[0].start())),
        path.segments.pairs(),
    );
    node.build(out)
}

pub(crate) fn append_path<
    'a,
>(
    out: &mut MakeSegsState,
    node: &mut SplitGroupBuilder,
    base_indent: &Alignment,
    mut prefix: Option<Option<LineColumn>>,
    pairs: impl Iterator<Item = syn::punctuated::Pair<&'a syn::PathSegment, &'a Token![::]>>,
) {
    let indent = base_indent.clone();
    for (i, seg) in pairs.enumerate() {
        if i > 0 {
            node.split(out, indent.clone(), true);
        }
        if let Some(d) = prefix.take() {
            if let Some(t) = d {
                append_whitespace(out, base_indent, node, t);
            };
            node.seg(out, "::");
        }
        append_whitespace(out, base_indent, node, seg.value().ident.span().start());
        node.seg(out, &seg.value().ident.to_string());
        match &seg.value().arguments {
            syn::PathArguments::None => { },
            syn::PathArguments::AngleBracketed(a) => {
                node.child(
                    new_sg_bracketed_list_common(
                        out,
                        &indent,
                        a.lt_token.span.start(),
                        &format!("{}{}", match &a.colon2_token {
                            Some(_) => "::",
                            None => "",
                        }, "<"),
                        &a.args,
                        a.gt_token.span.start(),
                        ">",
                    ),
                );
            },
            syn::PathArguments::Parenthesized(a) => {
                node.child(
                    new_sg_bracketed_list_common(
                        out,
                        &indent,
                        a.paren_token.span.open().start(),
                        "(",
                        &a.inputs,
                        a.paren_token.span.close().start(),
                        ")",
                    ),
                );
                match &a.output {
                    ReturnType::Default => { },
                    ReturnType::Type(_, ty) => {
                        node.seg(out, " -> ");
                        node.child(ty.make_segs(out, &indent));
                    },
                }
            },
        };
        match seg.punct() {
            Some(p) => prefix = Some(Some(p.spans[0].start())),
            None => prefix = None,
        }
    }
}

pub(crate) fn build_ref(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    start: LineColumn,
    mutability: bool,
    expr: impl Formattable,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    append_whitespace(out, base_indent, &mut sg, start);
    sg.seg(out, "&");
    if mutability {
        sg.seg(out, "mut ");
    }
    sg.child(expr.make_segs(out, base_indent));
    sg.build(out)
}

pub(crate) fn build_array_type(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    start: LineColumn,
    expr: impl Formattable,
    len: &Expr,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    append_whitespace(out, base_indent, &mut sg, start);
    sg.seg(out, "[");
    sg.child(expr.make_segs(out, base_indent));
    sg.seg(out, "; ");
    sg.child(len.make_segs(out, base_indent));
    sg.seg(out, "]");
    sg.build(out)
}

pub(crate) fn build_generics_part_a(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    generics: &Generics,
) -> SplitGroupIdx {
    new_sg_bracketed_list_common(
        out,
        base_indent,
        // not really optional, just sometimes not parsed
        generics.lt_token.map(|s| s.span.start()).unwrap_or(LineColumn {
            line: 0,
            column: 0,
        }),
        "<",
        &generics.params,
        // not really optional, just sometimes not parsed
        generics.gt_token.map(|s| s.span.start()).unwrap_or(LineColumn {
            line: 0,
            column: 0,
        }),
        ">",
    )
}

pub(crate) fn append_generics(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    generics: &Generics,
) {
    if generics.params.is_empty() {
        return;
    }
    sg.child(build_generics_part_a(out, base_indent, generics));
    if let Some(wh) = &generics.where_clause {
        sg.child(build_generics_part_b(out, base_indent, wh));
    }
}

pub(crate) fn build_generics_part_b(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    wh: &WhereClause,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    if out.config.split_where {
        sg.initial_split();
    }
    sg.seg_unsplit(out, " ");
    append_whitespace(out, base_indent, &mut sg, wh.where_token.span.start());
    sg.split(out, base_indent.clone(), true);
    sg.seg(out, "where");
    sg.seg_unsplit(out, " ");

    // No final comma because can be followed by a ;, and ,; looks pretty odd (and is
    // rejected by the compiler)
    append_inline_list(out, base_indent, &mut sg, ",", &wh.predicates, InlineListSuffix::<Expr>::None);
    sg.build(out)
}

pub(crate) fn append_angle_bracketed_generics(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    generics: &AngleBracketedGenericArguments,
) {
    if generics.args.is_empty() {
        return;
    }
    sg.child(new_sg_bracketed_list_common(
        out,
        base_indent,
        // not really optional, just sometimes not parsed
        generics.lt_token.span.start(),
        &generics.lt_token.to_token_stream().to_string(),
        &generics.args,
        // not really optional, just sometimes not parsed
        generics.gt_token.span.start(),
        &generics.gt_token.to_token_stream().to_string(),
    ));
}

impl Formattable for WherePredicate {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            WherePredicate::Type(t) => {
                let mut sg = new_sg(out);
                if let Some(hot) = &t.lifetimes {
                    append_whitespace(out, base_indent, &mut sg, hot.for_token.span.start());
                    sg.seg(out, "for");
                    append_bracketed_list_common(
                        out,
                        base_indent,
                        &mut sg,
                        hot.lt_token.span.start(),
                        "<",
                        &hot.lifetimes,
                        hot.gt_token.span.start(),
                        "> ",
                    );
                }
                sg.child(t.bounded_ty.make_segs(out, base_indent));
                sg.seg(out, ":");
                sg.seg_unsplit(out, " ");
                append_inline_list(out, base_indent, &mut sg, " +", &t.bounds, InlineListSuffix::<Expr>::None);
                sg.build(out)
            },
            WherePredicate::Lifetime(l) => {
                let mut sg = new_sg(out);
                sg.seg(out, &l.lifetime);
                sg.seg(out, ":");
                sg.seg_unsplit(out, " ");
                append_inline_list(out, base_indent, &mut sg, " +", &l.bounds, InlineListSuffix::<Expr>::None);
                sg.build(out)
            },
            _ => unreachable!(),
        }
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for GenericParam {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            GenericParam::Type(t) => new_sg_outer_attrs(
                out,
                base_indent,
                &t.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut sg = new_sg(out);
                        append_whitespace(out, base_indent, &mut sg, t.ident.span().start());
                        sg.seg(out, &t.ident);
                        if t.colon_token.is_some() && !t.bounds.is_empty() {
                            sg.seg(out, ": ");
                            append_inline_list(
                                out,
                                base_indent,
                                &mut sg,
                                " +",
                                &t.bounds,
                                InlineListSuffix::<Expr>::None,
                            );
                        }
                        sg.build(out)
                    };
                    if let Some(def) = &t.default {
                        new_sg_binary(out, base_indent, build_base, t.eq_token.unwrap().span.start(), " =", def)
                    } else {
                        build_base(out, base_indent)
                    }
                },
            ),
            GenericParam::Lifetime(l) => l.make_segs(out, base_indent),
            GenericParam::Const(c) => new_sg_outer_attrs(
                out,
                base_indent,
                &c.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut sg = new_sg(out);
                        append_whitespace(out, base_indent, &mut sg, c.const_token.span.start());
                        sg.seg(out, "const ");
                        append_whitespace(out, base_indent, &mut sg, c.ident.span().start());
                        sg.seg(out, &c.ident.to_string());
                        sg.seg(out, ": ");
                        sg.child(c.ty.make_segs(out, base_indent));
                        sg.build(out)
                    };
                    if let Some(def) = &c.default {
                        new_sg_binary(out, base_indent, build_base, c.eq_token.unwrap().span.start(), " =", def)
                    } else {
                        build_base(out, base_indent)
                    }
                },
            ),
        }
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for TypeParamBound {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        let mut sg = new_sg(out);
        match self {
            syn::TypeParamBound::Trait(t) => {
                if t.paren_token.is_some() {
                    sg.seg(out, "(");
                }
                match t.modifier {
                    syn::TraitBoundModifier::None => { },
                    syn::TraitBoundModifier::Maybe(_) => sg.seg(out, "?"),
                }
                if let Some(hot) = &t.lifetimes {
                    append_whitespace(out, base_indent, &mut sg, hot.for_token.span.start());
                    sg.seg(out, "for");
                    append_bracketed_list_common(
                        out,
                        base_indent,
                        &mut sg,
                        hot.lt_token.span.start(),
                        "<",
                        &hot.lifetimes,
                        hot.gt_token.span.start(),
                        "> ",
                    );
                }
                append_path(
                    out,
                    &mut sg,
                    base_indent,
                    t.path.leading_colon.map(|t| Some(t.spans[0].start())),
                    t.path.segments.pairs(),
                );
                if t.paren_token.is_some() {
                    sg.seg(out, ")");
                }
            },
            syn::TypeParamBound::Lifetime(l) => {
                sg.seg(out, l.to_string());
            },
            _ => unreachable!(),
        }
        sg.build(out)
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for LifetimeParam {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_outer_attrs(out, base_indent, &self.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut node = new_sg(out);
            node.seg(out, &self.lifetime);
            if self.colon_token.is_some() {
                append_binary(out, base_indent, &mut node, ":", |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_inline_list(
                        out,
                        base_indent,
                        &mut node,
                        " +",
                        &self.bounds,
                        InlineListSuffix::None::<Expr>,
                    );
                    node.build(out)
                });
            }
            node.build(out)
        })
    }

    fn has_attrs(&self) -> bool {
        !self.attrs.is_empty()
    }
}

impl Formattable for syn::Lifetime {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_lit(out, Some((base_indent, self.apostrophe.start())), self)
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for &Type {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            Type::Array(x) => build_array_type(
                out,
                base_indent,
                x.bracket_token.span.open().start(),
                x.elem.as_ref(),
                &x.len,
            ),
            Type::BareFn(x) => {
                let mut sg = new_sg(out);
                if let Some(hot) = &x.lifetimes {
                    append_whitespace(out, base_indent, &mut sg, hot.for_token.span.start());
                    sg.seg(out, "for");
                    append_bracketed_list_common(
                        out,
                        base_indent,
                        &mut sg,
                        hot.lt_token.span.start(),
                        "<",
                        &hot.lifetimes,
                        hot.gt_token.span.start(),
                        "> ",
                    );
                }
                if let Some(un) = &x.unsafety {
                    append_whitespace(out, base_indent, &mut sg, un.span.start());
                    sg.seg(out, "unsafe ");
                }
                if let Some(abi) = &x.abi {
                    append_whitespace(out, base_indent, &mut sg, abi.extern_token.span.start());
                    sg.seg(out, "extern ");
                    if let Some(name) = &abi.name {
                        append_whitespace(out, base_indent, &mut sg, name.span().start());
                        sg.seg(out, format!("{} ", name.to_token_stream()));
                    }
                }
                append_whitespace(out, base_indent, &mut sg, x.fn_token.span.start());
                sg.seg(out, "fn");
                sg.child(
                    new_sg_bracketed_list(
                        out,
                        base_indent,
                        x.paren_token.span.open().start(),
                        "(",
                        false,
                        ",",
                        &x.inputs,
                        match &x.variadic {
                            Some(v) => InlineListSuffix::Extra(|out: &mut MakeSegsState, _base_indent: &Alignment| {
                                new_sg_lit(out, Some((base_indent, v.dots.spans[0].start())), "...")
                            }),
                            None => InlineListSuffix::Punct,
                        },
                        x.paren_token.span.close().start(),
                        ")",
                    ),
                );
                match &x.output {
                    ReturnType::Default => { },
                    ReturnType::Type(t, ty) => {
                        append_whitespace(out, base_indent, &mut sg, t.spans[0].start());
                        sg.seg(out, " -> ");
                        sg.child(ty.make_segs(out, base_indent));
                    },
                }
                sg.build(out)
            },
            Type::Group(x) => x.elem.make_segs(out, base_indent),
            Type::ImplTrait(x) => {
                let mut node = new_sg(out);
                append_binary(out, base_indent, &mut node, "impl", |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_inline_list(out, base_indent, &mut node, " +", &x.bounds, InlineListSuffix::<Expr>::None);
                    node.build(out)
                });
                node.build(out)
            },
            Type::Infer(x) => new_sg_lit(out, Some((base_indent, x.underscore_token.span.start())), "_"),
            Type::Macro(x) => new_sg_macro(out, base_indent, &x.mac, false),
            Type::Never(x) => new_sg_lit(out, Some((base_indent, x.bang_token.span.start())), "!"),
            Type::Paren(x) => {
                let mut node = new_sg(out);
                node.seg(out, "(");
                node.child(x.elem.make_segs(out, base_indent));
                node.seg(out, ")");
                node.build(out)
            },
            Type::Path(x) => build_extended_path(out, base_indent, &x.qself, &x.path),
            Type::Ptr(x) => {
                let mut node = new_sg(out);
                let mut prefix = String::new();
                prefix.write_str("*").unwrap();
                if x.const_token.is_some() {
                    prefix.write_str("const ").unwrap();
                }
                if x.mutability.is_some() {
                    prefix.write_str("mut ").unwrap();
                }
                node.seg(out, prefix);
                node.child(x.elem.make_segs(out, base_indent));
                node.build(out)
            },
            Type::Reference(x) => {
                let mut node = new_sg(out);
                append_whitespace(out, base_indent, &mut node, x.and_token.span.start());
                node.seg(out, "&");
                if let Some(l) = &x.lifetime {
                    append_whitespace(out, base_indent, &mut node, l.span().start());
                    node.seg(out, format!("{} ", l));
                }
                if let Some(m) = &x.mutability {
                    append_whitespace(out, base_indent, &mut node, m.span.start());
                    node.seg(out, "mut ");
                }
                node.child(x.elem.make_segs(out, base_indent));
                node.build(out)
            },
            Type::Slice(x) => {
                let mut node = new_sg(out);
                node.seg(out, "[");
                node.child(x.elem.make_segs(out, base_indent));
                node.seg(out, "]");
                node.build(out)
            },
            Type::TraitObject(x) => {
                let mut node = new_sg(out);
                append_binary(out, base_indent, &mut node, "dyn", |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_inline_list(out, base_indent, &mut node, " +", &x.bounds, InlineListSuffix::<Expr>::None);
                    node.build(out)
                });
                node.build(out)
            },
            Type::Tuple(x) => new_sg_bracketed_list(
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
            ),
            Type::Verbatim(x) => new_sg_lit(out, None, x),
            _ => unreachable!(),
        }
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for Type {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        (&self).make_segs(out, base_indent)
    }

    fn has_attrs(&self) -> bool {
        (&self).has_attrs()
    }
}

impl Formattable for BareFnArg {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_outer_attrs(out, base_indent, &self.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
            if let Some(name) = &self.name {
                let mut node = new_sg(out);
                node.seg(out, format!("{}: ", name.0));
                node.child(self.ty.make_segs(out, base_indent));
                node.build(out)
            } else {
                self.ty.make_segs(out, base_indent)
            }
        })
    }

    fn has_attrs(&self) -> bool {
        !self.attrs.is_empty()
    }
}

impl Formattable for FnArg {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            FnArg::Receiver(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_self = |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut sg = new_sg(out);
                        let mut need_space = false;
                        if let Some(y) = &x.reference {
                            append_whitespace(out, base_indent, &mut sg, y.0.span.start());
                            sg.seg(out, "&");
                            if let Some(lt) = &y.1 {
                                append_whitespace(out, base_indent, &mut sg, lt.apostrophe.start());
                                sg.seg(out, lt.to_string());
                                need_space = true;
                            }
                        }
                        if let Some(y) = &x.mutability {
                            append_whitespace(out, base_indent, &mut sg, y.span.start());
                            sg.seg(out, format!("{}mut", if need_space {
                                " "
                            } else {
                                ""
                            }));
                            need_space = true;
                        }
                        append_whitespace(out, base_indent, &mut sg, x.self_token.span.start());
                        sg.seg(out, format!("{}self", if need_space {
                            " "
                        } else {
                            ""
                        }));
                        sg.build(out)
                    };
                    if let Some(colon) = &x.colon_token {
                        return new_sg_binary(out, base_indent, build_self, colon.span.start(), ":", x.ty.as_ref());
                    } else {
                        return build_self(out, base_indent);
                    }
                },
            ),
            FnArg::Typed(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, x.pat.as_ref(), x.colon_token.span.start(), ":", x.ty.as_ref())
                },
            ),
        }
    }

    fn has_attrs(&self) -> bool {
        match self {
            FnArg::Receiver(x) => !x.attrs.is_empty(),
            FnArg::Typed(x) => !x.attrs.is_empty(),
        }
    }
}

impl Formattable for GenericArgument {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            GenericArgument::Lifetime(l) => l.make_segs(out, base_indent),
            GenericArgument::Type(t) => t.make_segs(out, base_indent),
            GenericArgument::Const(c) => c.make_segs(out, base_indent),
            GenericArgument::AssocType(b) => new_sg_binary(
                out,
                base_indent,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child(b.ident.make_segs(out, base_indent));
                    if let Some(g) = &b.generics {
                        append_angle_bracketed_generics(out, base_indent, &mut sg, g);
                    }
                    sg.build(out)
                },
                b.eq_token.span.start(),
                " =",
                &b.ty,
            ),
            GenericArgument::AssocConst(b) => new_sg_binary(
                out,
                base_indent,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child(b.ident.make_segs(out, base_indent));
                    if let Some(g) = &b.generics {
                        append_angle_bracketed_generics(out, base_indent, &mut sg, g);
                    }
                    sg.build(out)
                },
                b.eq_token.span.start(),
                " =",
                &b.value,
            ),
            GenericArgument::Constraint(c) => {
                let mut node = new_sg(out);
                node.seg(out, &c.ident);
                append_binary(out, base_indent, &mut node, " =", |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_inline_list(out, base_indent, &mut node, " +", &c.bounds, InlineListSuffix::<Expr>::None);
                    node.build(out)
                });
                node.build(out)
            },
            _ => todo!(),
        }
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for &Path {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        build_path(out, base_indent, self)
    }

    fn has_attrs(&self) -> bool {
        false
    }
}
