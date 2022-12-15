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
    },
    sg_type::{append_path, build_path, build_generics_part_b, build_generics_part_a, build_generics},
    Alignment,
    Formattable,
    FormattableStmt,
    MakeSegsState,
    MarginGroup,
    SplitGroup,
    SplitGroupBuilder,
    TrivialLineColMath,
};
use quote::ToTokens;
use std::{cell::RefCell, rc::Rc};
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
            if r.in_token.is_some() { node.seg(out, "in "); }
            node.child(
                {
                    let mut node = new_sg();
                    append_path(
                        out,
                        &mut node,
                        base_indent,
                        r.path.leading_colon.map(|t| Some(t.spans[0].start())),
                        r.path.segments.pairs(),
                    );
                    node.build()
                },
            );
            node.seg(out, ") ");
        },
        syn::Visibility::Inherited => { },
    }
}

fn append_sig(out: &mut MakeSegsState, base_indent: &Alignment, sg: &mut SplitGroupBuilder, sig: &Signature) {
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
            if let Some(name) = &abi.name { prefix.push_str(&name.to_token_stream().to_string()); }
        }
        append_comments(out, base_indent, sg, sig.fn_token.span.start());
        prefix.push_str("fn ");
        prefix.push_str(&sig.ident.to_string());
        sg.seg(out, &prefix);
        if !sig.generics.params.is_empty() { sg.child(build_generics_part_a(out, base_indent, &sig.generics)); }
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
        match &sig.output { ReturnType::Default => { }, ReturnType::Type(_, t) => {
            sg.seg(out, " -> ");
            sg.child(t.make_segs(out, base_indent));
        } }
    }

    if let Some(wh) = &sig.generics.where_clause {
        sg.child(build_generics_part_b(out, base_indent, |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut sg = new_sg();
            build_base(out, base_indent, &mut sg, sig);
            sg.build()
        }, wh))
    } else { build_base(out, base_indent, sg, sig) }
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
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        match self {
            Stmt::Local(l) => {
                let mut sg = new_sg();
                append_comments(out, base_indent, &mut sg, l.let_token.span.start());
                sg.seg(out, "let ");
                sg.child(l.pat.make_segs(out, base_indent));
                if let Some(init) = &l.init { append_binary(out, base_indent, &mut sg, " =", init.1.as_ref()); }
                sg.seg(out, ";");
                sg.build()
            },
            Stmt::Item(i) => i.make_segs(out, base_indent),
            Stmt::Expr(e) => e.make_segs(out, base_indent),
            Stmt::Semi(e, _) => {
                let mut node = new_sg();
                node.child(e.make_segs(out, base_indent));
                node.seg(out, ";");
                node.build()
            },
        }
    }
}

impl FormattableStmt for ForeignItem { fn want_margin(&self) -> (MarginGroup, bool) { (MarginGroup::None, false) } }

impl Formattable for ForeignItem {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        match self {
            ForeignItem::Fn(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    append_sig(out, base_indent, &mut node, &x.sig);
                    node.seg(out, ";");
                    node.build()
                },
            ),
            ForeignItem::Static(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
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
                    sg.seg(out, ";");
                    sg.build()
                },
            ),
            ForeignItem::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.type_token.span.start());
                    let mut prefix = String::new();
                    prefix.push_str("type ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    sg.seg(out, ";");
                    sg.build()
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
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        match self {
            ImplItem::Const(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child({
                        let mut sg = new_sg();
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
                        sg.build()
                    });
                    append_binary(out, base_indent, &mut node, " =", &x.expr);
                    node.seg(out, ";");
                    node.build()
                },
            ),
            ImplItem::Method(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    append_sig(out, base_indent, &mut node, &x.sig);
                    node.child(
                        new_sg_block(
                            out,
                            base_indent,
                            x.block.brace_token.span.start(),
                            " {",
                            &x.block.stmts,
                            x.block.brace_token.span.end().prev(),
                        ),
                    );
                    let out = node.build();
                    out.as_ref().borrow_mut().children.reverse();
                    out
                },
            ),
            ImplItem::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
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
                    sg.child(build_generics(out, base_indent, &x.generics));
                    append_binary(out, base_indent, &mut sg, " =", &x.ty);
                    sg.seg(out, ";");
                    sg.build()
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
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        match self {
            TraitItem::Const(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child(
                        {
                            let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                                let mut sg = new_sg();
                                append_comments(out, base_indent, &mut sg, x.const_token.span.start());
                                let mut prefix = String::new();
                                prefix.push_str("const ");
                                prefix.push_str(&x.ident.to_string());
                                sg.seg(out, &prefix);
                                append_binary(out, base_indent, &mut sg, ":", &x.ty);
                                sg.build()
                            };
                            if let Some(d) = &x.default {
                                new_sg_binary(
                                    out,
                                    base_indent,
                                    |out: &mut MakeSegsState, base_indent: &Alignment| { build_base(out, base_indent) },
                                    " =",
                                    &d.1,
                                )
                            } else { build_base(out, base_indent) }
                        },
                    );
                    node.seg(out, ";");
                    node.build()
                },
            ),
            TraitItem::Method(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_sig(out, base_indent, &mut sg, &x.sig);
                    if let Some(d) = &x.default {
                        sg.child(
                            new_sg_block(
                                out,
                                base_indent,
                                d.brace_token.span.start(),
                                " {",
                                &d.stmts,
                                d.brace_token.span.end().prev(),
                            ),
                        );
                        let out = sg.build();
                        out.as_ref().borrow_mut().children.reverse();
                        out
                    } else {
                        sg.seg(out, ";");
                        sg.build()
                    }
                },
            ),
            TraitItem::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_base =
                        |out: &mut MakeSegsState, base_indent: &Alignment| {
                            let mut sg = new_sg();
                            append_comments(out, base_indent, &mut sg, x.type_token.span.start());
                            let mut prefix = String::new();
                            prefix.push_str("type ");
                            prefix.push_str(&x.ident.to_string());
                            sg.seg(out, &prefix);
                            if !x.generics.params.is_empty() { sg.child(build_generics(out, base_indent, &x.generics)); }
                            append_binary(
                                out,
                                base_indent,
                                &mut sg,
                                ":",
                                |out: &mut MakeSegsState, base_indent: &Alignment| {
                                    let mut node = new_sg();
                                    append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                                    node.build()
                                },
                            );
                            sg.build()
                        };
                    let mut node = new_sg();
                    node.child(
                        {
                            match &x.default {
                                Some(d) => new_sg_binary(
                                    out,
                                    base_indent,
                                    |out: &mut MakeSegsState, base_indent: &Alignment| {
                                        build_base(out, base_indent)
                                    },
                                    " =",
                                    &d.1,
                                ),
                                None => build_base(out, base_indent),
                            }
                        },
                    );
                    node.seg(out, ";");
                    node.build()
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
            Item::Mod(m) => (MarginGroup::BlockDef, match &m.content { Some(_) => true, None => false }),
            Item::Static(_) => (MarginGroup::None, false),
            Item::Struct(s) => (
                MarginGroup::BlockDef,
                match &s.fields {
                    syn::Fields::Named(_) => true,
                    syn::Fields::Unnamed(_) => true,
                    syn::Fields::Unit => false,
                },
            ),
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
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        match self {
            Item::Const(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.const_token.span.start());
                    sg.seg(out, "const ");
                    sg.seg(out, &x.ident.to_string());
                    sg.seg(out, ": ");
                    sg.child(x.ty.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut sg, " =", x.expr.as_ref());
                    sg.seg(out, ";");
                    sg.build()
                },
            ),
            Item::Enum(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.enum_token.span.start());
                    sg.seg(out, "enum ");
                    sg.seg(out, &x.ident.to_string());
                    if !x.generics.params.is_empty() { sg.child(build_generics(out, base_indent, &x.generics)); }
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        &x.variants,
                        x.brace_token.span.end().prev(),
                        "}",
                    );
                    sg.build()
                },
            ),
            Item::ExternCrate(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_comments(out, base_indent, &mut sg, x.extern_token.span.start());
                    sg.seg(out, "extern crate ");
                    sg.seg(out, &x.ident);
                    if let Some(r) = &x.rename {
                        sg.seg(out, " as ");
                        sg.seg(out, &r.1);
                    }
                    sg.seg(out, ";");
                    sg.build()
                },
            ),
            Item::Fn(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_sig(out, base_indent, &mut sg, &x.sig);
                    sg.child(
                        new_sg_block(
                            out,
                            base_indent,
                            x.block.brace_token.span.start(),
                            " {",
                            &x.block.stmts,
                            x.block.brace_token.span.end().prev(),
                        ),
                    );
                    let out = sg.build();
                    out.as_ref().borrow_mut().children.reverse();
                    out
                },
            ),
            Item::ForeignMod(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();

                    // extern token missing?
                    let mut prefix =
                        String::new();
                    prefix.push_str("extern ");
                    if let Some(name) = &x.abi.name { prefix.push_str(&name.to_token_stream().to_string()); }
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
                    sg.build()
                },
            ),
            Item::Impl(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    fn build_base(out: &mut MakeSegsState, base_indent: &Alignment, x: &ItemImpl) -> Rc<
                        RefCell<SplitGroup>,
                    > {
                        let mut sg = new_sg();
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
                            if bang.is_some() { sg.seg(out, "!"); }
                            sg.child(build_path(out, base_indent, &base));
                            sg.seg(out, " for ");
                        }
                        sg.child(x.self_ty.make_segs(out, base_indent));
                        sg.build()
                    }

                    let mut sg = new_sg();
                    if let Some(wh) = &x.generics.where_clause {
                        sg.child(
                            build_generics_part_b(
                                out,
                                base_indent,
                                |out: &mut MakeSegsState, base_indent: &Alignment| build_base(out, base_indent, x),
                                wh,
                            ),
                        );
                    } else { sg.child(build_base(out, base_indent, x)); }
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        Some(&x.attrs),
                        &x.items,
                        x.brace_token.span.end().prev(),
                    );
                    sg.build()
                },
            ),
            Item::Macro(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    sg.child(build_path(out, base_indent, &x.mac.path));
                    sg.seg(out, "!");
                    if let Some(n) = &x.ident { sg.seg(out, &format!(" {}", n)); }
                    let indent = base_indent.indent();
                    match &x.mac.delimiter { syn::MacroDelimiter::Paren(_) => {
                        sg.seg(out, "(");
                        append_macro_body(out, &indent, &mut sg, x.mac.tokens.to_token_stream());
                        sg.split(out, base_indent.clone(), false);
                        sg.seg(out, ")");
                    }, syn::MacroDelimiter::Brace(_) => {
                        sg.seg(out, "{");
                        append_macro_body(out, &indent, &mut sg, x.mac.tokens.to_token_stream());
                        sg.split(out, base_indent.clone(), false);
                        sg.seg(out, "}");
                    }, syn::MacroDelimiter::Bracket(_) => {
                        sg.seg(out, "[");
                        append_macro_body(out, &indent, &mut sg, x.mac.tokens.to_token_stream());
                        sg.split(out, base_indent.clone(), false);
                        sg.seg(out, "]");
                    } }
                    if x.semi_token.is_some() {
                    sg.seg(out, ";");
                    }
                    sg.build()
                },
            ),
            Item::Macro2(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    sg.seg(out, "macro ");
                    sg.seg(out, &x.ident.to_string());
                    let indent = base_indent.indent();
                    append_macro_body(out, &indent, &mut sg, x.rules.clone());
                    sg.build()
                },
            ),
            Item::Mod(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
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
                    } else { sg.seg(out, ";"); }
                    sg.build()
                },
            ),
            Item::Static(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.static_token.span.start());
                    sg.seg(out, "static ");
                    sg.seg(out, &x.ident.to_string());
                    sg.seg(out, ": ");
                    sg.child(x.ty.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut sg, " =", x.expr.as_ref());
                    sg.seg(out, ";");
                    sg.build()
                },
            ),
            Item::Struct(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.struct_token.span.start());
                    sg.seg(out, "struct ");
                    sg.seg(out, &x.ident.to_string());
                    if !x.generics.params.is_empty() { sg.child(build_generics(out, base_indent, &x.generics)); }
                    match &x.fields {
                        syn::Fields::Named(s) => {
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
                            sg.seg(out, ";");
                        },
                        syn::Fields::Unit => { sg.seg(out, ";"); },
                    }
                    sg.build()
                },
            ),
            Item::Trait(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
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
                    if !x.generics.params.is_empty() { sg.child(build_generics(out, base_indent, &x.generics)); }
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
                            &x.items,
                            x.brace_token.span.end().prev(),
                        ),
                    );
                    sg.build()
                },
            ),
            Item::TraitAlias(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.trait_token.span.start());
                    let mut prefix = String::new();
                    prefix.push_str("trait ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    if !x.generics.params.is_empty() { sg.child(build_generics(out, base_indent, &x.generics)); }
                    append_binary(out, base_indent, &mut sg, " =", |out: &mut MakeSegsState, base_indent: &Alignment| {
                        let mut node = new_sg();
                        append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                        node.build()
                    });
                    sg.seg(out, ";");
                    sg.build()
                },
            ),
            Item::Type(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.type_token.span.start());
                    let mut prefix = String::new();
                    prefix.push_str("type ");
                    prefix.push_str(&x.ident.to_string());
                    sg.seg(out, &prefix);
                    if !x.generics.params.is_empty() { sg.child(build_generics(out, base_indent, &x.generics)); }
                    append_binary(out, base_indent, &mut sg, " =", x.ty.as_ref());
                    sg.seg(out, ";");
                    sg.build()
                },
            ),
            Item::Union(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.union_token.span.start());
                    sg.seg(out, "union ");
                    sg.seg(out, &x.ident.to_string());
                    if !x.generics.params.is_empty() { sg.child(build_generics(out, base_indent, &x.generics)); }
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        &x.fields.named,
                        x.fields.brace_token.span.end().prev(),
                        "}",
                    );
                    sg.build()
                },
            ),
            Item::Use(x) => new_sg_outer_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    append_vis(out, base_indent, &mut sg, &x.vis);
                    append_comments(out, base_indent, &mut sg, x.use_token.span.start());
                    sg.seg(out, "use ");
                    if x.leading_colon.is_some() { sg.seg(out, "::"); }
                    sg.child(x.tree.make_segs(out, base_indent));
                    sg.seg(out, ";");
                    sg.build()
                },
            ),
            Item::Verbatim(x) => {
                let mut node = new_sg();
                node.seg(out, &x.to_string());
                node.build()
            },
            _ => unreachable!(),
        }
    }
}

impl Formattable for Variant {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        new_sg_outer_attrs(
            out,
            base_indent,
            &self.attrs,
            |out: &mut MakeSegsState, base_indent: &Alignment| {
                let mut sg = new_sg();
                append_comments(out, base_indent, &mut sg, self.ident.span().start());
                sg.seg(out, &self.ident);
                match &self.fields {
                    syn::Fields::Named(s) => {
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
                if let Some(e) = &self.discriminant { append_binary(out, base_indent, &mut sg, " = ", &e.1); }
                sg.build()
            },
        )
    }
}

impl Formattable for Field {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        new_sg_outer_attrs(out, base_indent, &self.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut sg = new_sg();
            append_vis(out, base_indent, &mut sg, &self.vis);
            if let Some(n) = &self.ident {
                append_comments(out, base_indent, &mut sg, n.span().start());
                sg.seg(out, &format!("{}: ", n));
            }
            sg.child((&self.ty).make_segs(out, base_indent));
            sg.build()
        })
    }
}

impl Formattable for &UseTree {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        let mut sg = new_sg();
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
            syn::UseTree::Glob(_) => { sg.seg(out, "*"); },
            syn::UseTree::Group(x) => {
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
        sg.build()
    }
}

impl Formattable for UseTree {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        (&self).make_segs(out, base_indent)
    }
}
