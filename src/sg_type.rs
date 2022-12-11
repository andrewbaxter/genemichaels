use std::{cell::RefCell, fmt::Write, rc::Rc};

use quote::ToTokens;
use syn::{
    BareFnArg, Expr, FnArg, GenericArgument, GenericMethodArgument, GenericParam, Generics,
    LifetimeDef, Path, QSelf, ReturnType, TraitItem, Type, TypeParamBound, WherePredicate,
};

use crate::{
    new_sg, new_sg_lit,
    sg_general::{
        append_binary, append_comma_bracketed_list, append_inline_list, new_sg_attrs,
        new_sg_binary, new_sg_comma_bracketed_list, new_sg_comma_bracketed_list_ext, new_sg_macro,
    },
    Alignment, Formattable, MakeSegsState, SplitGroup, SplitGroupBuilder,
};

pub(crate) fn build_extended_path(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    qself: &Option<QSelf>,
    p: &Path,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    match qself {
        Some(qself) => {
            node.seg(out, "<");
            let taken = match qself.position {
                0 => {
                    node.child(qself.ty.make_segs(out, base_indent));
                    0
                }
                n => {
                    node.child(qself.ty.make_segs(out, base_indent));
                    node.seg(out, " as ");
                    append_path(
                        out,
                        &mut node,
                        base_indent,
                        p.leading_colon.is_some(),
                        p.segments.pairs().take(n),
                    );
                    n
                }
            };
            node.seg(out, ">");
            append_path(
                out,
                &mut node,
                base_indent,
                true,
                p.segments.pairs().skip(taken),
            );
        }
        None => {
            append_path(
                out,
                &mut node,
                base_indent,
                p.leading_colon.is_some(),
                p.segments.pairs(),
            );
        }
    };
    node.build()
}

pub(crate) fn build_path(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    path: &syn::Path,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    append_path(
        out,
        &mut node,
        base_indent,
        path.leading_colon.is_some(),
        path.segments.pairs(),
    );
    node.build()
}

pub(crate) fn append_path<'a>(
    out: &mut MakeSegsState,
    node: &mut SplitGroupBuilder,
    base_indent: &Alignment,
    prefix: bool,
    pairs: impl Iterator<Item = syn::punctuated::Pair<&'a syn::PathSegment, &'a syn::token::Colon2>>,
) {
    let indent = base_indent.clone();
    for (i, seg) in pairs.enumerate() {
        if i > 0 || prefix {
            node.seg(out, "::");
        }
        if i > 0 {
            node.split(out, indent.clone(), true);
        }
        node.seg(out, &seg.value().ident.to_string());
        match &seg.value().arguments {
            syn::PathArguments::None => {}
            syn::PathArguments::AngleBracketed(a) => {
                node.child(new_sg_comma_bracketed_list(
                    out,
                    &indent,
                    None::<&Expr>,
                    "<",
                    &a.args,
                    ">",
                ));
            }
            syn::PathArguments::Parenthesized(a) => {
                node.child(new_sg_comma_bracketed_list(
                    out,
                    &indent,
                    None::<&Expr>,
                    "(",
                    &a.inputs,
                    ")",
                ));
                node.seg(out, " -> ");
                match &a.output {
                    ReturnType::Default => node.seg(out, "()"),
                    ReturnType::Type(_, ty) => node.child(ty.make_segs(out, &indent)),
                }
            }
        };
    }
}

pub(crate) fn build_ref(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    mutability: bool,
    expr: impl Formattable,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.seg(out, "&");
    if mutability {
        node.seg(out, "mut ");
    }
    node.child(expr.make_segs(out, base_indent));
    node.build()
}

pub(crate) fn build_array_type(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    expr: impl Formattable,
    len: &Expr,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.seg(out, "[");
    node.child(expr.make_segs(out, base_indent));
    node.seg(out, "; ");
    node.child(len.make_segs(out, base_indent));
    node.seg(out, "]");
    node.build()
}

pub(crate) fn build_generics(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    generics: &Generics,
) -> Rc<RefCell<SplitGroup>> {
    if let Some(wh) = &generics.where_clause {
        new_sg_binary(
            out,
            base_indent,
            |out: &mut MakeSegsState, base_indent: &Alignment| {
                new_sg_comma_bracketed_list(
                    out,
                    base_indent,
                    None::<Expr>,
                    "<",
                    &generics.params,
                    ">",
                )
            },
            " where",
            |out: &mut MakeSegsState, base_indent: &Alignment| {
                let mut node = new_sg();
                append_inline_list(out, base_indent, &mut node, ",", true, &wh.predicates);
                node.build()
            },
        )
    } else {
        new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, "<", &generics.params, ">")
    }
}

impl Formattable for WherePredicate {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            WherePredicate::Type(t) => {
                let mut node = new_sg();
                if let Some(hot) = &t.lifetimes {
                    node.seg(out, "for");
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut node,
                        "<",
                        &hot.lifetimes,
                        "> ",
                    );
                }
                node.child(t.bounded_ty.make_segs(out, base_indent));
                node.seg(out, ": ");
                append_inline_list(out, base_indent, &mut node, " +", false, &t.bounds);
                node.build()
            }
            WherePredicate::Lifetime(l) => {
                let mut node = new_sg();
                node.seg(out, &l.lifetime);
                node.seg(out, ": ");
                append_inline_list(out, base_indent, &mut node, " +", false, &l.bounds);
                node.build()
            }
            WherePredicate::Eq(e) => new_sg_binary(out, base_indent, &e.lhs_ty, " =", &e.rhs_ty),
        }
    }
}

impl Formattable for GenericParam {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            GenericParam::Type(t) => new_sg_attrs(
                out,
                base_indent,
                &t.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut prefix = String::new();
                        prefix.push_str(&t.ident.to_string());
                        prefix.push_str(": ");
                        let mut node = new_sg();
                        node.seg(out, prefix);
                        append_inline_list(out, base_indent, &mut node, " +", false, &t.bounds);
                        node.build()
                    };
                    if let Some(def) = &t.default {
                        new_sg_binary(out, base_indent, build_base, " =", def)
                    } else {
                        build_base(out, base_indent)
                    }
                },
            ),
            GenericParam::Lifetime(l) => l.make_segs(out, base_indent),
            GenericParam::Const(c) => new_sg_attrs(
                out,
                base_indent,
                &c.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut prefix = String::new();
                        prefix.push_str("const ");
                        prefix.push_str(&c.ident.to_string());
                        prefix.push_str(": ");
                        let mut node = new_sg();
                        node.seg(out, prefix);
                        node.child(c.ty.make_segs(out, base_indent));
                        node.build()
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
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        let mut node = new_sg();
        match self {
            syn::TypeParamBound::Trait(t) => {
                if t.paren_token.is_some() {
                    node.seg(out, "(");
                }
                match t.modifier {
                    syn::TraitBoundModifier::None => {}
                    syn::TraitBoundModifier::Maybe(_) => node.seg(out, "?"),
                }
                if let Some(hot) = &t.lifetimes {
                    node.seg(out, "for");
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut node,
                        "<",
                        &hot.lifetimes,
                        "> ",
                    );
                }
                append_path(
                    out,
                    &mut node,
                    base_indent,
                    t.path.leading_colon.is_some(),
                    t.path.segments.pairs(),
                );
                if t.paren_token.is_some() {
                    node.seg(out, ")");
                }
            }
            syn::TypeParamBound::Lifetime(l) => {
                node.seg(out, l.to_string());
            }
        }
        node.build()
    }
}

impl Formattable for LifetimeDef {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        new_sg_attrs(
            out,
            base_indent,
            &self.attrs,
            |out: &mut MakeSegsState, base_indent: &Alignment| {
                let mut node = new_sg();
                node.seg(out, &self.lifetime);
                if self.colon_token.is_some() {
                    append_binary(
                        out,
                        base_indent,
                        &mut node,
                        ":",
                        |out: &mut MakeSegsState, base_indent: &Alignment| {
                            let mut node = new_sg();
                            append_inline_list(
                                out,
                                base_indent,
                                &mut node,
                                " +",
                                false,
                                &self.bounds,
                            );
                            node.build()
                        },
                    );
                }
                node.build()
            },
        )
    }
}

impl Formattable for syn::Lifetime {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        _base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        new_sg_lit(out, self)
    }
}

impl Formattable for &Type {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            Type::Array(x) => build_array_type(out, base_indent, x.elem.as_ref(), &x.len),
            Type::BareFn(x) => {
                let mut node = new_sg();
                if let Some(hot) = &x.lifetimes {
                    node.seg(out, "for");
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut node,
                        "<",
                        &hot.lifetimes,
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
                node.seg(out, &prefix);
                if x.variadic.is_some() {
                    node.child(new_sg_comma_bracketed_list_ext(
                        out,
                        base_indent,
                        None::<Expr>,
                        "(",
                        &x.inputs,
                        |out: &mut MakeSegsState, _base_indent: &Alignment| new_sg_lit(out, "..."),
                        ")",
                    ));
                } else {
                    node.child(new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        None::<Expr>,
                        "(",
                        &x.inputs,
                        ")",
                    ));
                }
                match &x.output {
                    ReturnType::Default => {}
                    ReturnType::Type(_, t) => {
                        node.seg(out, " -> ");
                        node.child(t.make_segs(out, base_indent));
                    }
                }
                node.build()
            }
            Type::Group(x) => x.elem.make_segs(out, base_indent),
            Type::ImplTrait(x) => {
                let mut node = new_sg();
                append_binary(
                    out,
                    base_indent,
                    &mut node,
                    "impl",
                    |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut node = new_sg();
                        append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                        node.build()
                    },
                );
                node.build()
            }
            Type::Infer(_) => new_sg_lit(out, "_"),
            Type::Macro(x) => new_sg_macro(out, base_indent, &x.mac, false),
            Type::Never(_) => new_sg_lit(out, "!"),
            Type::Paren(x) => {
                let mut node = new_sg();
                node.seg(out, "(");
                node.child(x.elem.make_segs(out, base_indent));
                node.seg(out, ")");
                node.build()
            }
            Type::Path(x) => build_extended_path(out, base_indent, &x.qself, &x.path),
            Type::Ptr(x) => {
                let mut node = new_sg();
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
                node.build()
            }
            Type::Reference(x) => {
                let mut node = new_sg();
                node.seg(out, "&");
                if let Some(l) = &x.lifetime {
                    node.seg(out, format!("{} ", l));
                }
                if x.mutability.is_some() {
                    node.seg(out, "mut ");
                }
                node.child(x.elem.make_segs(out, base_indent));
                node.build()
            }
            Type::Slice(x) => {
                let mut node = new_sg();
                node.seg(out, "[");
                node.child(x.elem.make_segs(out, base_indent));
                node.seg(out, "]");
                node.build()
            }
            Type::TraitObject(x) => {
                let mut node = new_sg();
                append_binary(
                    out,
                    base_indent,
                    &mut node,
                    "dyn",
                    |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut node = new_sg();
                        append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                        node.build()
                    },
                );
                node.build()
            }
            Type::Tuple(x) => {
                new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, "(", &x.elems, ")")
            }
            Type::Verbatim(x) => new_sg_lit(out, x),
            _ => unreachable!(),
        }
    }
}

impl Formattable for Type {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        (&self).make_segs(out, base_indent)
    }
}

impl Formattable for BareFnArg {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        new_sg_attrs(
            out,
            base_indent,
            &self.attrs,
            |out: &mut MakeSegsState, base_indent: &Alignment| {
                if let Some(name) = &self.name {
                    let mut node = new_sg();
                    node.seg(out, format!("{}: ", name.0));
                    node.child(self.ty.make_segs(out, base_indent));
                    node.build()
                } else {
                    self.ty.make_segs(out, base_indent)
                }
            },
        )
    }
}

impl Formattable for FnArg {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            FnArg::Receiver(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut prefix = String::new();
                    if x.reference.is_some() {
                        prefix.write_str("&").unwrap();
                    }
                    if x.mutability.is_some() {
                        prefix.write_str("mut ").unwrap();
                    }
                    prefix.write_str("self").unwrap();
                    new_sg_lit(out, prefix)
                },
            ),
            FnArg::Typed(x) => new_sg_attrs(
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
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            GenericArgument::Lifetime(l) => l.make_segs(out, base_indent),
            GenericArgument::Type(t) => t.make_segs(out, base_indent),
            GenericArgument::Const(c) => c.make_segs(out, base_indent),
            GenericArgument::Binding(b) => new_sg_binary(out, base_indent, &b.ident, " =", &b.ty),
            GenericArgument::Constraint(c) => {
                let mut node = new_sg();
                node.seg(out, &c.ident);
                append_binary(
                    out,
                    base_indent,
                    &mut node,
                    " =",
                    |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut node = new_sg();
                        append_inline_list(out, base_indent, &mut node, " +", false, &c.bounds);
                        node.build()
                    },
                );
                node.build()
            }
        }
    }
}
impl Formattable for GenericMethodArgument {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            GenericMethodArgument::Type(t) => t.make_segs(out, base_indent),
            GenericMethodArgument::Const(c) => c.make_segs(out, base_indent),
        }
    }
}

impl Formattable for TraitItem {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            TraitItem::Const(_) => todo!(),
            TraitItem::Method(_) => todo!(),
            TraitItem::Type(_) => todo!(),
            TraitItem::Macro(_) => todo!(),
            TraitItem::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}

impl Formattable for &Path {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        build_path(out, base_indent, self)
    }
}
