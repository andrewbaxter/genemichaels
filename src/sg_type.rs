use std::{
    fmt::Write,
};
use proc_macro2::LineColumn;
use quote::ToTokens;
use syn::{
    BareFnArg,
    Expr,
    FnArg,
    GenericArgument,
    GenericMethodArgument,
    GenericParam,
    Generics,
    LifetimeDef,
    Path,
    QSelf,
    ReturnType,
    Type,
    TypeParamBound,
    WherePredicate,
    WhereClause,
};
use crate::{
    new_sg,
    new_sg_lit,
    sg_general::{
        append_binary,
        append_comma_bracketed_list,
        append_comments,
        append_inline_list,
        new_sg_outer_attrs,
        new_sg_binary,
        new_sg_comma_bracketed_list,
        new_sg_comma_bracketed_list_ext,
        new_sg_macro,
    },
    Alignment,
    Formattable,
    MakeSegsState,
    SplitGroupBuilder,
    TrivialLineColMath,
    SplitGroupIdx,
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
            append_comments(out, base_indent, &mut node, qself.gt_token.span.start());
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

pub(crate) fn append_path<'a>(
    out: &mut MakeSegsState,
    node: &mut SplitGroupBuilder,
    base_indent: &Alignment,
    prefix: Option<Option<LineColumn>>,
    pairs: impl Iterator<Item = syn::punctuated::Pair<&'a syn::PathSegment, &'a syn::token::Colon2>>,
) {
    let indent = base_indent.clone();
    let mut prefix = prefix;
    for (i, seg) in pairs.enumerate() {
        if i > 0 {
            node.split(out, indent.clone(), true);
        }
        match prefix {
            Some(d) => {
                match d {
                    Some(t) => {
                        append_comments(out, base_indent, node, t);
                    },
                    None => { },
                };
                node.seg(out, "::");
            },
            None => { },
        }
        append_comments(out, base_indent, node, seg.value().ident.span().start());
        node.seg(out, &seg.value().ident.to_string());
        match &seg.value().arguments {
            syn::PathArguments::None => { },
            syn::PathArguments::AngleBracketed(a) => {
                node.child(
                    new_sg_comma_bracketed_list(
                        out,
                        &indent,
                        None::<&Expr>,
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
                    new_sg_comma_bracketed_list(
                        out,
                        &indent,
                        None::<&Expr>,
                        a.paren_token.span.start(),
                        "(",
                        &a.inputs,
                        a.paren_token.span.end().prev(),
                        ")",
                    ),
                );
                node.seg(out, " -> ");
                match &a.output {
                    ReturnType::Default => node.seg(out, "()"),
                    ReturnType::Type(_, ty) => node.child(ty.make_segs(out, &indent)),
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
    append_comments(out, base_indent, &mut sg, start);
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
    append_comments(out, base_indent, &mut sg, start);
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
    new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, 
        // not really optional, just sometimes not parsed
        generics.lt_token.map(|s| s.span.start()).unwrap_or(LineColumn{
            line: 0,
            column: 0,
        }), "<", &generics.params, 
        // not really optional, just sometimes not parsed
        generics.gt_token.map(|s| s.span.start()).unwrap_or(LineColumn{
            line: 0,
            column: 0,
        }), ">")
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
    sg.child({
        if let Some(wh) = &generics.where_clause {
            build_generics_part_b(
                out,
                base_indent,
                |out: &mut MakeSegsState, base_indent: &Alignment| build_generics_part_a(out, base_indent, generics),
                wh,
            )
        } else {
            build_generics_part_a(out, base_indent, generics)
        }
    });
}

pub(crate) fn build_generics_part_b(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: impl Formattable,
    wh: &WhereClause,
) -> SplitGroupIdx {
    new_sg_binary(out, base_indent, base, " where", |out: &mut MakeSegsState, base_indent: &Alignment| {
        let mut node = new_sg(out);
        append_inline_list(out, base_indent, &mut node, ",", true, &wh.predicates);
        node.build(out)
    })
}

impl Formattable for WherePredicate {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            WherePredicate::Type(t) => {
                let mut sg = new_sg(out);
                if let Some(hot) = &t.lifetimes {
                    append_comments(out, base_indent, &mut sg, hot.for_token.span.start());
                    sg.seg(out, "for");
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        "<",
                        &hot.lifetimes,
                        hot.gt_token.span.start(),
                        "> ",
                    );
                }
                sg.child(t.bounded_ty.make_segs(out, base_indent));
                sg.seg(out, ": ");
                append_inline_list(out, base_indent, &mut sg, " +", false, &t.bounds);
                sg.build(out)
            },
            WherePredicate::Lifetime(l) => {
                let mut node = new_sg(out);
                node.seg(out, &l.lifetime);
                node.seg(out, ": ");
                append_inline_list(out, base_indent, &mut node, " +", false, &l.bounds);
                node.build(out)
            },
            WherePredicate::Eq(e) => new_sg_binary(out, base_indent, &e.lhs_ty, " =", &e.rhs_ty),
        }
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
                        sg.seg(out, &t.ident);
                        if t.colon_token.is_some() {
                            sg.seg(out, ": ");
                            append_inline_list(out, base_indent, &mut sg, " +", false, &t.bounds);
                        }
                        sg.build(out)
                    };
                    if let Some(def) = &t.default {
                        new_sg_binary(out, base_indent, build_base, " =", def)
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
                        let mut prefix = String::new();
                        prefix.push_str("const ");
                        prefix.push_str(&c.ident.to_string());
                        prefix.push_str(": ");
                        let mut node = new_sg(out);
                        node.seg(out, prefix);
                        node.child(c.ty.make_segs(out, base_indent));
                        node.build(out)
                    };
                    if let Some(def) = &c.default {
                        new_sg_binary(out, base_indent, build_base, " =", def)
                    } else {
                        build_base(out, base_indent)
                    }
                },
            ),
        }
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
                    append_comments(out, base_indent, &mut sg, hot.for_token.span.start());
                    sg.seg(out, "for");
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
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
        }
        sg.build(out)
    }
}

impl Formattable for LifetimeDef {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_outer_attrs(out, base_indent, &self.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut node = new_sg(out);
            node.seg(out, &self.lifetime);
            if self.colon_token.is_some() {
                append_binary(out, base_indent, &mut node, ":", |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_inline_list(out, base_indent, &mut node, " +", false, &self.bounds);
                    node.build(out)
                });
            }
            node.build(out)
        })
    }
}

impl Formattable for syn::Lifetime {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_lit(out, Some((base_indent, self.apostrophe.start())), self)
    }
}

impl Formattable for &Type {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            Type::Array(x) => build_array_type(
                out,
                base_indent,
                x.bracket_token.span.start(),
                x.elem.as_ref(),
                &x.len,
            ),
            Type::BareFn(x) => {
                let mut sg = new_sg(out);
                if let Some(hot) = &x.lifetimes {
                    append_comments(out, base_indent, &mut sg, hot.for_token.span.start());
                    sg.seg(out, "for");
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        "<",
                        &hot.lifetimes,
                        hot.gt_token.span.start(),
                        "> ",
                    );
                }
                let mut prefix = String::new();
                if x.unsafety.is_some() {
                    prefix.push_str("unsafe ");
                }
                if let Some(abi) = &x.abi {
                    prefix.push_str("extern ");
                    if let Some(name) = &abi.name {
                        prefix.push_str(&name.to_token_stream().to_string());
                    }
                }
                prefix.push_str("fn");
                sg.seg(out, &prefix);
                if let Some(v) = &x.variadic {
                    sg.child(
                        new_sg_comma_bracketed_list_ext(
                            out,
                            base_indent,
                            None::<Expr>,
                            x.paren_token.span.start(),
                            "(",
                            &x.inputs,
                            |out: &mut MakeSegsState, _base_indent: &Alignment| {
                                new_sg_lit(out, Some((base_indent, v.dots.spans[0].start())), "...")
                            },
                            ")",
                        ),
                    );
                } else {
                    sg.child(
                        new_sg_comma_bracketed_list(
                            out,
                            base_indent,
                            None::<Expr>,
                            x.paren_token.span.start(),
                            "(",
                            &x.inputs,
                            x.paren_token.span.end().prev(),
                            ")",
                        ),
                    );
                }
                match &x.output {
                    ReturnType::Default => { },
                    ReturnType::Type(_, t) => {
                        sg.seg(out, " -> ");
                        sg.child(t.make_segs(out, base_indent));
                    },
                }
                sg.build(out)
            },
            Type::Group(x) => x.elem.make_segs(out, base_indent),
            Type::ImplTrait(x) => {
                let mut node = new_sg(out);
                append_binary(out, base_indent, &mut node, "impl", |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
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
                node.seg(out, "&");
                if let Some(l) = &x.lifetime {
                    node.seg(out, format!("{} ", l));
                }
                if x.mutability.is_some() {
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
                    append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                    node.build(out)
                });
                node.build(out)
            },
            Type::Tuple(x) => new_sg_comma_bracketed_list(
                out,
                base_indent,
                None::<Expr>,
                x.paren_token.span.start(),
                "(",
                &x.elems,
                x.paren_token.span.end().prev(),
                ")",
            ),
            Type::Verbatim(x) => new_sg_lit(out, None, x),
            _ => unreachable!(),
        }
    }
}

impl Formattable for Type {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        (&self).make_segs(out, base_indent)
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
}

impl Formattable for FnArg {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            FnArg::Receiver(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    let mut need_space = false;
                    if let Some(y) = &x.reference {
                        append_comments(out, base_indent, &mut sg, y.0.span.start());
                        sg.seg(out, "&");
                        if let Some(lt) = &y.1 {
                            append_comments(out, base_indent, &mut sg, lt.apostrophe.start());
                            sg.seg(out, lt.to_string());
                            need_space = true;
                        }
                    }
                    if let Some(y) = &x.mutability {
                        append_comments(out, base_indent, &mut sg, y.span.start());
                        sg.seg(out, format!("{}mut", if need_space {
                            " "
                        } else {
                            ""
                        }));
                        need_space = true;
                    }
                    append_comments(out, base_indent, &mut sg, x.self_token.span.start());
                    sg.seg(out, format!("{}self", if need_space {
                        " "
                    } else {
                        ""
                    }));
                    sg.build(out)
                },
            ),
            FnArg::Typed(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, x.pat.as_ref(), ":", x.ty.as_ref())
                },
            ),
        }
    }
}

impl Formattable for GenericArgument {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            GenericArgument::Lifetime(l) => l.make_segs(out, base_indent),
            GenericArgument::Type(t) => t.make_segs(out, base_indent),
            GenericArgument::Const(c) => c.make_segs(out, base_indent),
            GenericArgument::Binding(b) => new_sg_binary(out, base_indent, &b.ident, " =", &b.ty),
            GenericArgument::Constraint(c) => {
                let mut node = new_sg(out);
                node.seg(out, &c.ident);
                append_binary(out, base_indent, &mut node, " =", |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_inline_list(out, base_indent, &mut node, " +", false, &c.bounds);
                    node.build(out)
                });
                node.build(out)
            },
        }
    }
}

impl Formattable for GenericMethodArgument {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            GenericMethodArgument::Type(t) => t.make_segs(out, base_indent),
            GenericMethodArgument::Const(c) => c.make_segs(out, base_indent),
        }
    }
}

impl Formattable for &Path {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        build_path(out, base_indent, self)
    }
}
