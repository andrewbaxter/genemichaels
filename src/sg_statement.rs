use crate::{
    new_sg,
    new_sg_lit,
    sg_general::{
        append_binary,
        append_bracketed_statement_list,
        append_comma_bracketed_list,
        append_comments,
        append_inline_list,
        append_macro_body,
        new_sg_outer_attrs,
        new_sg_binary,
        new_sg_block,
        new_sg_comma_bracketed_list,
        new_sg_comma_bracketed_list_ext,
        new_sg_macro,
        append_macro_bracketed,
    },
    sg_type::{
        append_path,
        build_path,
        build_generics_part_b,
        build_generics_part_a,
        append_generics,
    },
    Alignment,
    Formattable,
    FormattableStmt,
    MakeSegsState,
    MarginGroup,
    SplitGroupBuilder,
    TrivialLineColMath,
    check_split_brace_threshold,
    SplitGroupIdx,
};
use quote::ToTokens;
use syn::{
    Expr,
    Field,
    ForeignItem,
    ImplItem,
    Item,
    ReturnType,
    Signature,
    Stmt,
    TraitItem,
    UseTree,
    Variant,
    Visibility,
    ItemImpl,
};

fn append_vis(out: &mut MakeSegsState, base_indent: &Alignment, node: &mut SplitGroupBuilder, vis: &Visibility) {
    match vis {
        syn::Visibility::Public(x) => {
            append_comments(out, base_indent, node, x.pub_token.span.start());
            node.seg(out, "pub ");
        },
        syn::Visibility::Crate(x) => {
            append_comments(out, base_indent, node, x.crate_token.span.start());
            node.seg(out, "crate ");
        },
        syn::Visibility::Restricted(r) => {
            append_comments(out, base_indent, node, r.pub_token.span.start());
            node.seg(out, "pub(");
            if r.in_token.is_some() {
                node.seg(out, "in ");
            }
            node.child({
                let mut node = new_sg(out);
                append_path(
                    out,
                    &mut node,
                    base_indent,
                    r.path.leading_colon.map(|t| Some(t.spans[0].start())),
                    r.path.segments.pairs(),
                );
                node.build(out)
            });
            node.seg(out, ") ");
        },
        syn::Visibility::Inherited => { },
    }
}

fn new_sg_sig(out: &mut MakeSegsState, base_indent: &Alignment, sig: &Signature) -> SplitGroupIdx {
    let mut sg = new_sg(out);

    fn build_base(out: &mut MakeSegsState, base_indent: &Alignment, sg: &mut SplitGroupBuilder, sig: &Signature) {
        let mut prefix = String::new();
        if let Some(x) = sig.constness {
            append_comments(out, base_indent, sg, x.span.start());
            prefix.push_str("const ");
        }
        if let Some(x) = sig.asyncness {
            append_comments(out, base_indent, sg, x.span.start());
            prefix.push_str("async ");
        }
        if let Some(x) = sig.unsafety {
            append_comments(out, base_indent, sg, x.span.start());
            prefix.push_str("unsafe ");
        }
        if let Some(abi) = &sig.abi {
            append_comments(out, base_indent, sg, abi.extern_token.span.start());
            prefix.push_str("extern ");
            if let Some(name) = &abi.name {
                prefix.push_str(&name.to_token_stream().to_string());
                prefix.push_str(" ");
            }
        }
        append_comments(out, base_indent, sg, sig.fn_token.span.start());
        prefix.push_str("fn ");
        prefix.push_str(&sig.ident.to_string());
        sg.seg(out, &prefix);
        if !sig.generics.params.is_empty() {
            sg.child(build_generics_part_a(out, base_indent, &sig.generics));
        }
        if let Some(v) = &sig.variadic {
            sg.child(
                new_sg_comma_bracketed_list_ext(
                    out,
                    base_indent,
                    None::<Expr>,
                    sig.paren_token.span.start(),
                    "(",
                    &sig.inputs,
                    |out: &mut MakeSegsState, base_indent: &Alignment| {
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
                    sig.paren_token.span.start(),
                    "(",
                    &sig.inputs,
                    sig.paren_token.span.end().prev(),
                    ")",
                ),
            );
        }
        match &sig.output {
            ReturnType::Default => { },
            ReturnType::Type(_, t) => {
                sg.seg(out, " -> ");
                sg.child(t.make_segs(out, base_indent));
            },
        }
    }

    if let Some(wh) = &sig.generics.where_clause {
        sg.child(build_generics_part_b(out, base_indent, |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut sg = new_sg(out);
            build_base(out, base_indent, &mut sg, sig);
            sg.build(out)
        }, wh))
    } else {
        build_base(out, base_indent, &mut sg, sig)
    }
    sg.build(out)
}

impl FormattableStmt for Stmt {
    fn want_margin(&self) -> (MarginGroup, bool) {
        match self {
            Stmt::Local(_) => (MarginGroup::None, false),
            Stmt::Item(i) => i.want_margin(),
            Stmt::Expr(_) => (MarginGroup::None, false),
            Stmt::Semi(_, _) => (MarginGroup::None, false),
        }
    }
}

impl Formattable for Stmt {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            Stmt::Local(l) => new_sg_outer_attrs(
                out,
                base_indent,
                &l.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, l.let_token.span.start());
                    sg.seg(out, "let ");
                    sg.child(l.pat.make_segs(out, base_indent));
                    if let Some(init) = &l.init {
                        append_binary(out, base_indent, &mut sg, " =", init.1.as_ref());
                    }
                    append_comments(out, base_indent, &mut sg, l.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            Stmt::Item(i) => i.make_segs(out, base_indent),
            Stmt::Expr(e) => e.make_segs(out, base_indent),
            Stmt::Semi(e, semi) => {
                let mut sg = new_sg(out);
                sg.child(e.make_segs(out, base_indent));
                append_comments(out, base_indent, &mut sg, semi.span.start());
                sg.seg(out, ";");
                sg.build(out)
            },
        }
    }
}

impl FormattableStmt for ForeignItem {
    fn want_margin(&self) -> (MarginGroup, bool) {
        (MarginGroup::None, false)
    }
}

impl Formattable for ForeignItem {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            ForeignItem::Fn(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    sg.child(new_sg_sig(out, base_indent, &x.sig));
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            ForeignItem::Static(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, x.static_token.span.start());
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    let mut prefix = String::new();
                    prefix.push_str("static ");
                    if let Some(x) = x.mutability {
                        append_comments(out, base_indent, &mut sg, x.span.start());
                        prefix.push_str("mut ");
                    }
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            ForeignItem::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.type_token.span.start());
                    let mut prefix = String::new();
                    prefix.push_str("type ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            ForeignItem::Macro(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &x.mac, x.semi_token.is_some())
                },
            ),
            ForeignItem::Verbatim(x) => new_sg_lit(out, None, x),
            _ => unreachable!(),
        }
    }
}

impl FormattableStmt for ImplItem {
    fn want_margin(&self) -> (MarginGroup, bool) {
        match self {
            ImplItem::Const(_) => (MarginGroup::None, false),
            ImplItem::Method(_) => (MarginGroup::BlockDef, true),
            ImplItem::Type(_) => (MarginGroup::None, false),
            ImplItem::Macro(_) => (MarginGroup::BlockDef, true),
            ImplItem::Verbatim(_) => (MarginGroup::None, false),
            _ => unreachable!(),
        }
    }
}

impl Formattable for ImplItem {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            ImplItem::Const(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child({
                        let mut sg = new_sg(out);
                        append_vis(out, base_indent, &mut sg, &x.vis);
                        let mut prefix = String::new();
                        if let Some(d) = x.defaultness {
                            append_comments(out, base_indent, &mut sg, d.span.start());
                            prefix.push_str("default ");
                        }
                        append_comments(out, base_indent, &mut sg, x.const_token.span.start());
                        prefix.push_str("const ");
                        prefix.push_str(&x.ident.to_string());
                        sg.seg(out, &prefix);
                        append_binary(out, base_indent, &mut sg, ":", &x.ty);
                        sg.build(out)
                    });
                    append_binary(out, base_indent, &mut sg, " =", &x.expr);
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            ImplItem::Method(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    sg.child(new_sg_sig(out, base_indent, &x.sig));
                    sg.child(
                        new_sg_block(
                            out,
                            base_indent,
                            x.block.brace_token.span.start(),
                            " {",
                            Some(&x.attrs),
                            &x.block.stmts,
                            x.block.brace_token.span.end().prev(),
                        ),
                    );
                    sg.reverse_children();
                    sg.build(out)
                },
            ),
            ImplItem::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    let mut prefix = String::new();
                    if let Some(d) = x.defaultness {
                        append_comments(out, base_indent, &mut sg, d.span.start());
                        prefix.push_str("default ");
                    }
                    append_comments(out, base_indent, &mut sg, x.type_token.span.start());
                    prefix.push_str("type ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    append_generics(out, base_indent, &mut sg, &x.generics);
                    append_binary(out, base_indent, &mut sg, " =", &x.ty);
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            ImplItem::Macro(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &x.mac, x.semi_token.is_some())
                },
            ),
            ImplItem::Verbatim(x) => new_sg_lit(out, None, x),
            _ => unreachable!(),
        }
    }
}

impl FormattableStmt for TraitItem {
    fn want_margin(&self) -> (MarginGroup, bool) {
        match self {
            TraitItem::Const(_) => (MarginGroup::None, false),
            TraitItem::Method(m) => (MarginGroup::BlockDef, m.default.is_some()),
            TraitItem::Type(_) => (MarginGroup::None, false),
            TraitItem::Macro(_) => (MarginGroup::BlockDef, true),
            TraitItem::Verbatim(_) => (MarginGroup::None, false),
            _ => unreachable!(),
        }
    }
}

impl Formattable for TraitItem {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            TraitItem::Const(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child({
                        let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                            let mut sg = new_sg(out);
                            append_comments(out, base_indent, &mut sg, x.const_token.span.start());
                            let mut prefix = String::new();
                            prefix.push_str("const ");
                            prefix.push_str(&x.ident.to_string());
                            sg.seg(out, &prefix);
                            append_binary(out, base_indent, &mut sg, ":", &x.ty);
                            sg.build(out)
                        };
                        if let Some(d) = &x.default {
                            new_sg_binary(out, base_indent, |out: &mut MakeSegsState, base_indent: &Alignment| {
                                build_base(out, base_indent)
                            }, d.0.span.start(), " =", &d.1)
                        } else {
                            build_base(out, base_indent)
                        }
                    });
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            TraitItem::Method(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child(new_sg_sig(out, base_indent, &x.sig));
                    if let Some(d) = &x.default {
                        sg.child(
                            new_sg_block(
                                out,
                                base_indent,
                                d.brace_token.span.start(),
                                " {",
                                None,
                                &d.stmts,
                                d.brace_token.span.end().prev(),
                            ),
                        );
                        sg.reverse_children();
                        sg.build(out)
                    } else {
                        append_comments(out, base_indent, &mut sg, x.semi_token.unwrap().span.start());
                        sg.seg(out, ";");
                        sg.build(out)
                    }
                },
            ),
            TraitItem::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut sg = new_sg(out);
                        append_comments(out, base_indent, &mut sg, x.type_token.span.start());
                        let mut prefix = String::new();
                        prefix.push_str("type ");
                        prefix.push_str(&x.ident.to_string());
                        sg.seg(out, &prefix);
                        append_generics(out, base_indent, &mut sg, &x.generics);
                        append_binary(
                            out,
                            base_indent,
                            &mut sg,
                            ":",
                            |out: &mut MakeSegsState, base_indent: &Alignment| {
                                let mut node = new_sg(out);
                                append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                                node.build(out)
                            },
                        );
                        sg.build(out)
                    };
                    let mut sg = new_sg(out);
                    sg.child({
                        match &x.default {
                            Some(d) => new_sg_binary(
                                out,
                                base_indent,
                                |out: &mut MakeSegsState, base_indent: &Alignment| {
                                    build_base(out, base_indent)
                                },
                                d.0.span.start(),
                                " =",
                                &d.1,
                            ),
                            None => build_base(out, base_indent),
                        }
                    });
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            TraitItem::Macro(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &x.mac, x.semi_token.is_some())
                },
            ),
            TraitItem::Verbatim(x) => new_sg_lit(out, None, x),
            _ => unreachable!(),
        }
    }
}

impl FormattableStmt for Item {
    fn want_margin(&self) -> (MarginGroup, bool) {
        match self {
            Item::Const(_) => (MarginGroup::None, false),
            Item::Enum(_) => (MarginGroup::BlockDef, true),
            Item::ExternCrate(_) => (MarginGroup::None, false),
            Item::Fn(_) => (MarginGroup::BlockDef, true),
            Item::ForeignMod(_) => (MarginGroup::BlockDef, true),
            Item::Impl(_) => (MarginGroup::BlockDef, true),
            Item::Macro(_) => (MarginGroup::BlockDef, true),
            Item::Macro2(_) => (MarginGroup::BlockDef, true),
            Item::Mod(m) => (MarginGroup::BlockDef, match &m.content {
                Some(_) => true,
                None => false,
            }),
            Item::Static(_) => (MarginGroup::None, false),
            Item::Struct(s) => (MarginGroup::BlockDef, match &s.fields {
                syn::Fields::Named(_) => true,
                syn::Fields::Unnamed(_) => true,
                syn::Fields::Unit => false,
            }),
            Item::Trait(_) => (MarginGroup::BlockDef, true),
            Item::TraitAlias(_) => (MarginGroup::None, false),
            Item::Type(_) => (MarginGroup::None, false),
            Item::Union(_) => (MarginGroup::BlockDef, true),
            Item::Use(_) => (MarginGroup::Import, false),
            Item::Verbatim(_) => (MarginGroup::None, false),
            _ => unreachable!(),
        }
    }
}

impl Formattable for Item {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            Item::Const(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.const_token.span.start());
                    sg.seg(out, "const ");
                    sg.seg(out, &x.ident.to_string());
                    sg.seg(out, ": ");
                    sg.child(x.ty.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut sg, " =", x.expr.as_ref());
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            Item::Enum(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    if check_split_brace_threshold(out, x.variants.len()) {
                        sg.initial_split();
                    }
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.enum_token.span.start());
                    sg.seg(out, "enum ");
                    sg.seg(out, &x.ident.to_string());
                    append_generics(out, base_indent, &mut sg, &x.generics);
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        &x.variants,
                        x.brace_token.span.end().prev(),
                        "}",
                    );
                    sg.build(out)
                },
            ),
            Item::ExternCrate(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, x.extern_token.span.start());
                    sg.seg(out, "extern crate ");
                    sg.seg(out, &x.ident);
                    if let Some(r) = &x.rename {
                        sg.seg(out, " as ");
                        sg.seg(out, &r.1);
                    }
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            Item::Fn(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    sg.child(new_sg_sig(out, base_indent, &x.sig));
                    sg.child(
                        new_sg_block(
                            out,
                            base_indent,
                            x.block.brace_token.span.start(),
                            " {",
                            Some(&x.attrs),
                            &x.block.stmts,
                            x.block.brace_token.span.end().prev(),
                        ),
                    );
                    sg.reverse_children();
                    sg.build(out)
                },
            ),
            Item::ForeignMod(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);

                    // extern token missing?
                    let mut prefix = String::new();
                    prefix.push_str("extern ");
                    if let Some(name) = &x.abi.name {
                        prefix.push_str(&name.to_token_stream().to_string());
                    }
                    sg.seg(out, &prefix);
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        Some(&x.attrs),
                        &x.items,
                        x.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Item::Impl(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    fn build_base(out: &mut MakeSegsState, base_indent: &Alignment, x: &ItemImpl) -> SplitGroupIdx {
                        let mut sg = new_sg(out);
                        let mut prefix = String::new();
                        if let Some(d) = x.defaultness {
                            append_comments(out, base_indent, &mut sg, d.span.start());
                            prefix.push_str("default ");
                        }
                        if let Some(u) = x.unsafety {
                            append_comments(out, base_indent, &mut sg, u.span.start());
                            prefix.push_str("unsafe ");
                        }
                        append_comments(out, base_indent, &mut sg, x.impl_token.span.start());
                        prefix.push_str("impl");
                        sg.seg(out, &prefix);
                        if !x.generics.params.is_empty() {
                            sg.child(build_generics_part_a(out, base_indent, &x.generics));
                        }
                        sg.seg(out, " ");
                        if let Some((bang, base, _)) = &x.trait_ {
                            if bang.is_some() {
                                sg.seg(out, "!");
                            }
                            sg.child(build_path(out, base_indent, &base));
                            sg.seg(out, " for ");
                        }
                        sg.child(x.self_ty.make_segs(out, base_indent));
                        sg.build(out)
                    }

                    let mut sg = new_sg(out);
                    if let Some(wh) = &x.generics.where_clause {
                        sg.child(
                            build_generics_part_b(
                                out,
                                base_indent,
                                |out: &mut MakeSegsState, base_indent: &Alignment| build_base(out, base_indent, x),
                                wh,
                            ),
                        );
                    } else {
                        sg.child(build_base(out, base_indent, x));
                    }
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        Some(&x.attrs),
                        &x.items,
                        x.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Item::Macro(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child(build_path(out, base_indent, &x.mac.path));
                    sg.seg(out, "!");
                    if let Some(n) = &x.ident {
                        sg.seg(out, &format!(" {}", n));
                    }
                    append_macro_bracketed(out, base_indent, &mut sg, &x.mac, x.semi_token.is_some());
                    sg.build(out)
                },
            ),
            Item::Macro2(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    sg.seg(out, "macro ");
                    sg.seg(out, &x.ident.to_string());
                    let indent = base_indent.indent();
                    append_macro_body(out, &indent, &mut sg, x.rules.clone());
                    sg.build(out)
                },
            ),
            Item::Mod(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.mod_token.span.start());
                    sg.seg(out, "mod ");
                    sg.seg(out, &x.ident.to_string());
                    if let Some(content) = &x.content {
                        append_bracketed_statement_list(
                            out,
                            base_indent,
                            &mut sg,
                            " {",
                            Some(&x.attrs),
                            &content.1,
                            content.0.span.end().prev(),
                        );
                    } else {
                        append_comments(out, base_indent, &mut sg, x.semi.unwrap().span.start());
                        sg.seg(out, ";");
                    }
                    sg.build(out)
                },
            ),
            Item::Static(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.static_token.span.start());
                    sg.seg(out, "static ");
                    sg.seg(out, &x.ident.to_string());
                    sg.seg(out, ": ");
                    sg.child(x.ty.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut sg, " =", x.expr.as_ref());
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            Item::Struct(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.struct_token.span.start());
                    sg.seg(out, "struct ");
                    sg.seg(out, &x.ident.to_string());
                    append_generics(out, base_indent, &mut sg, &x.generics);
                    match &x.fields {
                        syn::Fields::Named(s) => {
                            if check_split_brace_threshold(out, s.named.len()) {
                                sg.initial_split();
                            }
                            append_comma_bracketed_list(
                                out,
                                base_indent,
                                &mut sg,
                                " {",
                                &s.named,
                                s.brace_token.span.end().prev(),
                                "}",
                            );
                        },
                        syn::Fields::Unnamed(t) => {
                            append_comma_bracketed_list(
                                out,
                                base_indent,
                                &mut sg,
                                "(",
                                &t.unnamed,
                                t.paren_token.span.end().prev(),
                                ")",
                            );
                            append_comments(out, base_indent, &mut sg, x.semi_token.unwrap().span.start());
                            sg.seg(out, ";");
                        },
                        syn::Fields::Unit => {
                            append_comments(out, base_indent, &mut sg, x.semi_token.unwrap().span.start());
                            sg.seg(out, ";");
                        },
                    }
                    sg.build(out)
                },
            ),
            Item::Trait(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    let mut prefix = String::new();
                    if let Some(u) = x.unsafety {
                        append_comments(out, base_indent, &mut sg, u.span.start());
                        prefix.push_str("unsafe ");
                    }
                    if let Some(a) = x.auto_token {
                        append_comments(out, base_indent, &mut sg, a.span.start());
                        prefix.push_str("auto ");
                    }
                    append_comments(out, base_indent, &mut sg, x.trait_token.span.start());
                    prefix.push_str("trait ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    append_generics(out, base_indent, &mut sg, &x.generics);
                    if x.colon_token.is_some() {
                        sg.seg(out, ": ");
                        append_inline_list(out, base_indent, &mut sg, " +", false, &x.supertraits);
                    }
                    sg.child(
                        new_sg_block(
                            out,
                            base_indent,
                            x.brace_token.span.start(),
                            " {",
                            Some(&x.attrs),
                            &x.items,
                            x.brace_token.span.end().prev(),
                        ),
                    );
                    sg.build(out)
                },
            ),
            Item::TraitAlias(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.trait_token.span.start());
                    let mut prefix = String::new();
                    prefix.push_str("trait ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    append_generics(out, base_indent, &mut sg, &x.generics);
                    append_binary(out, base_indent, &mut sg, " =", |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut node = new_sg(out);
                        append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                        node.build(out)
                    });
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            Item::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.type_token.span.start());
                    let mut prefix = String::new();
                    prefix.push_str("type ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    append_generics(out, base_indent, &mut sg, &x.generics);
                    append_binary(out, base_indent, &mut sg, " =", x.ty.as_ref());
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            Item::Union(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    if check_split_brace_threshold(out, x.fields.named.len()) {
                        sg.initial_split();
                    }
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.union_token.span.start());
                    sg.seg(out, "union ");
                    sg.seg(out, &x.ident.to_string());
                    append_generics(out, base_indent, &mut sg, &x.generics);
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        &x.fields.named,
                        x.fields.brace_token.span.end().prev(),
                        "}",
                    );
                    sg.build(out)
                },
            ),
            Item::Use(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.use_token.span.start());
                    sg.seg(out, "use ");
                    if x.leading_colon.is_some() {
                        sg.seg(out, "::");
                    }
                    sg.child(x.tree.make_segs(out, base_indent));
                    append_comments(out, base_indent, &mut sg, x.semi_token.span.start());
                    sg.seg(out, ";");
                    sg.build(out)
                },
            ),
            Item::Verbatim(x) => {
                let mut node = new_sg(out);
                node.seg(out, &x.to_string());
                node.build(out)
            },
            _ => unreachable!(),
        }
    }
}

impl Formattable for Variant {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_outer_attrs(out, base_indent, &self.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut sg = new_sg(out);
            append_comments(out, base_indent, &mut sg, self.ident.span().start());
            sg.seg(out, &self.ident);
            match &self.fields {
                syn::Fields::Named(s) => {
                    if check_split_brace_threshold(out, s.named.len()) {
                        sg.initial_split();
                    }
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        &s.named,
                        s.brace_token.span.end().prev(),
                        "}",
                    );
                },
                syn::Fields::Unnamed(t) => {
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        "(",
                        &t.unnamed,
                        t.paren_token.span.end().prev(),
                        ")",
                    );
                },
                syn::Fields::Unit => { },
            }
            if let Some(e) = &self.discriminant {
                append_binary(out, base_indent, &mut sg, " =", &e.1);
            }
            sg.build(out)
        })
    }
}

impl Formattable for Field {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_outer_attrs(out, base_indent, &self.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut sg = new_sg(out);
            append_vis(out, base_indent, &mut sg, &self.vis);
            if let Some(n) = &self.ident {
                append_comments(out, base_indent, &mut sg, n.span().start());
                sg.seg(out, &format!("{}: ", n));
            }
            sg.child((&self.ty).make_segs(out, base_indent));
            sg.build(out)
        })
    }
}

impl Formattable for &UseTree {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        let mut sg = new_sg(out);
        match self {
            syn::UseTree::Path(x) => {
                append_comments(out, base_indent, &mut sg, x.ident.span().start());
                sg.seg(out, &format!("{}::", x.ident));
                sg.child(x.tree.make_segs(out, base_indent));
            },
            syn::UseTree::Name(x) => {
                append_comments(out, base_indent, &mut sg, x.ident.span().start());
                sg.seg(out, &x.ident.to_string());
            },
            syn::UseTree::Rename(x) => {
                append_comments(out, base_indent, &mut sg, x.ident.span().start());
                append_comments(out, base_indent, &mut sg, x.rename.span().start());
                sg.seg(out, format!("{} as {}", x.ident, x.rename));
            },
            syn::UseTree::Glob(_) => {
                sg.seg(out, "*");
            },
            syn::UseTree::Group(x) => {
                if check_split_brace_threshold(out, x.items.len()) {
                    sg.initial_split();
                }
                append_comma_bracketed_list(
                    out,
                    base_indent,
                    &mut sg,
                    "{",
                    &x.items,
                    x.brace_token.span.end().prev(),
                    "}",
                );
            },
        }
        sg.build(out)
    }
}

impl Formattable for UseTree {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        (&self).make_segs(out, base_indent)
    }
}
