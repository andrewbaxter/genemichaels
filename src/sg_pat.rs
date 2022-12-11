use std::{cell::RefCell, fmt::Write, rc::Rc};

use syn::{Expr, FieldPat, Pat};

use crate::{
    new_sg, new_sg_lit,
    sg_general::{
        append_binary, append_inline_list, new_sg_attrs, new_sg_binary,
        new_sg_comma_bracketed_list, new_sg_comma_bracketed_list_ext, new_sg_macro,
    },
    sg_type::{build_extended_path, build_ref},
    Alignment, Formattable, MakeSegsState, SplitGroup,
};

impl Formattable for &Pat {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            Pat::Box(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "box ");
                    node.child(x.pat.as_ref().make_segs(out, base_indent));
                    node.build()
                },
            ),
            Pat::Ident(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut prefix = String::new();
                    if x.by_ref.is_some() {
                        prefix.write_str("const ").unwrap();
                    }
                    if x.mutability.is_some() {
                        prefix.write_str("mut ").unwrap();
                    }
                    prefix.write_str(&x.ident.to_string()).unwrap();
                    if let Some(at) = &x.subpat {
                        new_sg_binary(
                            out,
                            base_indent,
                            |out: &mut MakeSegsState, _build_indent: &Alignment| {
                                new_sg_lit(out, &prefix)
                            },
                            " @",
                            &*at.1,
                        )
                    } else {
                        new_sg_lit(out, prefix)
                    }
                },
            ),
            Pat::Lit(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    x.expr.as_ref().make_segs(out, base_indent)
                },
            ),
            Pat::Macro(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &x.mac)
                },
            ),
            Pat::Or(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    if x.leading_vert.is_some() {
                        node.seg(out, "| ");
                    }
                    append_inline_list(out, base_indent, &mut node, " |", false, &x.cases);
                    node.build()
                },
            ),
            Pat::Path(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_extended_path(out, base_indent, &x.qself, &x.path)
                },
            ),
            Pat::Range(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(
                        out,
                        base_indent,
                        x.lo.as_ref(),
                        match x.limits {
                            syn::RangeLimits::HalfOpen(_) => "..",
                            syn::RangeLimits::Closed(_) => "..=",
                        },
                        x.hi.as_ref(),
                    )
                },
            ),
            Pat::Reference(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_ref(out, base_indent, x.mutability.is_some(), x.pat.as_ref())
                },
            ),
            Pat::Rest(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| new_sg_lit(out, ".."),
            ),
            Pat::Slice(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, "[", &x.elems, "]")
                },
            ),
            Pat::Struct(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    if x.dot2_token.is_some() {
                        new_sg_comma_bracketed_list_ext(
                            out,
                            base_indent,
                            Some(&x.path),
                            "{",
                            &x.fields,
                            |out: &mut MakeSegsState, _base_indent: &Alignment| {
                                new_sg_lit(out, "..")
                            },
                            "}",
                        )
                    } else {
                        new_sg_comma_bracketed_list(
                            out,
                            base_indent,
                            Some(&x.path),
                            "{",
                            &x.fields,
                            "}",
                        )
                    }
                },
            ),
            Pat::Tuple(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, "(", &x.elems, ")")
                },
            ),
            Pat::TupleStruct(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        Some(&x.path),
                        "(",
                        &x.pat.elems,
                        ")",
                    )
                },
            ),
            Pat::Type(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, x.pat.as_ref(), ":", x.ty.as_ref())
                },
            ),
            Pat::Verbatim(x) => new_sg_lit(out, x),
            Pat::Wild(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| new_sg_lit(out, "_"),
            ),
            _ => unreachable!(),
        }
    }
}

impl Formattable for Pat {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        (&self).make_segs(out, base_indent)
    }
}

impl Formattable for FieldPat {
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
                let mut sg = new_sg();
                match &self.member {
                    syn::Member::Named(x) => sg.seg(out, x),
                    syn::Member::Unnamed(x) => sg.seg(out, x.index),
                };
                append_binary(out, base_indent, &mut sg, ":", self.pat.as_ref());
                sg.build()
            },
        )
    }
}
