use std::{cell::RefCell, rc::Rc};

use quote::ToTokens;
use syn::{
    Expr, Field, ForeignItem, ImplItem, Item, ReturnType, Stmt, UseTree, Variant, Visibility,
};

use crate::{
    new_sg, new_sg_lit,
    sg_general::{
        append_binary, append_block, append_comma_bracketed_list, append_inline_list,
        append_macro_body, new_sg_attrs, new_sg_block, new_sg_comma_bracketed_list,
        new_sg_comma_bracketed_list_ext,
    },
    sg_type::{append_path, build_generics, build_path},
    Alignment, Formattable, MakeSegsState, SplitGroup, SplitGroupBuilder,
};

fn append_vis(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    vis: &Visibility,
) {
    match vis {
        syn::Visibility::Public(_) => node.seg(out, "pub "),
        syn::Visibility::Crate(_) => node.seg(out, "crate "),
        syn::Visibility::Restricted(r) => {
            node.seg(out, "pub(");
            if r.in_token.is_some() {
                node.seg(out, "in ");
            }
            node.child({
                let mut node = new_sg();
                append_path(
                    out,
                    &mut node,
                    base_indent,
                    r.path.leading_colon.is_some(),
                    r.path.segments.pairs(),
                );
                node.build()
            });
            node.seg(out, ") ");
        }
        syn::Visibility::Inherited => {}
    }
}

impl Formattable for Stmt {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            Stmt::Local(l) => {
                let mut node = new_sg();
                node.seg(out, "let ");
                node.child(l.pat.make_segs(out, base_indent));
                if let Some(init) = &l.init {
                    append_binary(out, base_indent, &mut node, " =", init.1.as_ref());
                }
                node.seg(out, ";");
                node.build()
            }
            Stmt::Item(i) => i.make_segs(out, base_indent),
            Stmt::Expr(e) => e.make_segs(out, base_indent),
            Stmt::Semi(e, _) => {
                let mut node = new_sg();
                node.child(e.make_segs(out, base_indent));
                node.seg(out, ";");
                node.build()
            }
        }
    }
}

impl Formattable for ForeignItem {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            ForeignItem::Fn(_) => todo!(),
            ForeignItem::Static(_) => todo!(),
            ForeignItem::Type(_) => todo!(),
            ForeignItem::Macro(_) => todo!(),
            ForeignItem::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}

impl Formattable for ImplItem {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            ImplItem::Const(_) => todo!(),
            ImplItem::Method(_) => todo!(),
            ImplItem::Type(_) => todo!(),
            ImplItem::Macro(_) => todo!(),
            ImplItem::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}

impl Formattable for Item {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            Item::Const(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "const ");
                    node.seg(out, &x.ident.to_string());
                    node.seg(out, ": ");
                    node.child(x.ty.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut node, " =", x.expr.as_ref());
                    node.seg(out, ";");
                    node.build()
                },
            ),
            Item::Enum(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "enum ");
                    node.seg(out, &x.ident.to_string());
                    if !x.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.generics));
                    }
                    append_comma_bracketed_list(out, base_indent, &mut node, "{", &x.variants, "}");
                    node.build()
                },
            ),
            Item::ExternCrate(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "extern crate ");
                    node.seg(out, &x.ident);
                    if let Some(r) = &x.rename {
                        node.seg(out, " as ");
                        node.seg(out, &r.1);
                    }
                    node.seg(out, ";");
                    node.build()
                },
            ),
            Item::Fn(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    let mut prefix = String::new();
                    if x.sig.constness.is_some() {
                        prefix.push_str("const ");
                    }
                    if x.sig.asyncness.is_some() {
                        prefix.push_str("async ");
                    }
                    if x.sig.unsafety.is_some() {
                        prefix.push_str("unsafe ");
                    }
                    if let Some(abi) = &x.sig.abi {
                        prefix.push_str("extern ");
                        if let Some(name) = &abi.name {
                            prefix.push_str(&name.to_token_stream().to_string());
                        }
                    }
                    prefix.push_str("fn ");
                    prefix.push_str(&x.sig.ident.to_string());
                    node.seg(out, &prefix);
                    if !x.sig.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.sig.generics));
                    }
                    if x.sig.variadic.is_some() {
                        node.child(new_sg_comma_bracketed_list_ext(
                            out,
                            base_indent,
                            None::<Expr>,
                            "(",
                            &x.sig.inputs,
                            |out: &mut MakeSegsState, _base_indent: &Alignment| {
                                new_sg_lit(out, "...")
                            },
                            ")",
                        ));
                    } else {
                        node.child(new_sg_comma_bracketed_list(
                            out,
                            base_indent,
                            None::<Expr>,
                            "(",
                            &x.sig.inputs,
                            ")",
                        ));
                    }
                    match &x.sig.output {
                        ReturnType::Default => {}
                        ReturnType::Type(_, t) => {
                            node.seg(out, " -> ");
                            node.child(t.make_segs(out, base_indent));
                        }
                    }
                    node.child(new_sg_block(out, base_indent, " {", &x.block.stmts));
                    node.build()
                },
            ),
            Item::ForeignMod(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    let mut prefix = String::new();
                    prefix.push_str("extern ");
                    if let Some(name) = &x.abi.name {
                        prefix.push_str(&name.to_token_stream().to_string());
                    }
                    node.seg(out, &prefix);
                    append_block(out, base_indent, &mut node, " {", &x.items);
                    node.build()
                },
            ),
            Item::Impl(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    let mut prefix = String::new();
                    if x.defaultness.is_some() {
                        prefix.push_str("default ");
                    }
                    if x.unsafety.is_some() {
                        prefix.push_str("unsafe ");
                    }
                    prefix.push_str("impl");
                    node.seg(out, &prefix);
                    if !x.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.generics));
                    }
                    if let Some((bang, base, _)) = &x.trait_ {
                        if bang.is_some() {
                            node.seg(out, " !");
                        } else {
                            node.seg(out, " ");
                        }
                        node.child(build_path(out, base_indent, &base));
                        node.seg(out, " for ");
                    }
                    node.child(x.self_ty.make_segs(out, base_indent));
                    append_block(out, base_indent, &mut node, " {", &x.items);
                    node.build()
                },
            ),
            Item::Macro(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child(build_path(out, base_indent, &x.mac.path));
                    node.seg(out, "!");
                    if let Some(n) = &x.ident {
                        node.seg(out, &format!(" {}", n));
                    }
                    let indent = base_indent.indent();
                    match &x.mac.delimiter {
                        syn::MacroDelimiter::Paren(_) => {
                            node.seg(out, "(");
                            append_macro_body(
                                out,
                                &indent,
                                &mut node,
                                x.mac.tokens.to_token_stream(),
                            );
                            node.split(out, base_indent.clone());
                            node.seg(out, ")");
                        }
                        syn::MacroDelimiter::Brace(_) => {
                            node.seg(out, "{");
                            append_macro_body(
                                out,
                                &indent,
                                &mut node,
                                x.mac.tokens.to_token_stream(),
                            );
                            node.split(out, base_indent.clone());
                            node.seg(out, "}");
                        }
                        syn::MacroDelimiter::Bracket(_) => {
                            node.seg(out, "[");
                            append_macro_body(
                                out,
                                &indent,
                                &mut node,
                                x.mac.tokens.to_token_stream(),
                            );
                            node.split(out, base_indent.clone());
                            node.seg(out, "]");
                        }
                    }
                    node.build()
                },
            ),
            Item::Macro2(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "macro ");
                    node.seg(out, &x.ident.to_string());
                    let indent = base_indent.indent();
                    append_macro_body(out, &indent, &mut node, x.rules.clone());
                    node.build()
                },
            ),
            Item::Mod(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "mod ");
                    node.seg(out, &x.ident.to_string());
                    if let Some(content) = &x.content {
                        append_block(out, base_indent, &mut node, " {", &content.1);
                    }
                    node.build()
                },
            ),
            Item::Static(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "static ");
                    node.seg(out, &x.ident.to_string());
                    node.seg(out, ": ");
                    node.child(x.ty.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut node, " =", x.expr.as_ref());
                    node.seg(out, ";");
                    node.build()
                },
            ),
            Item::Struct(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "struct ");
                    node.seg(out, &x.ident.to_string());
                    if !x.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.generics));
                    }
                    match &x.fields {
                        syn::Fields::Named(s) => {
                            append_comma_bracketed_list(
                                out,
                                base_indent,
                                &mut node,
                                "{",
                                &s.named,
                                "}",
                            );
                        }
                        syn::Fields::Unnamed(t) => {
                            append_comma_bracketed_list(
                                out,
                                base_indent,
                                &mut node,
                                "(",
                                &t.unnamed,
                                ")",
                            );
                            node.seg(out, ";");
                        }
                        syn::Fields::Unit => {
                            node.seg(out, ";");
                        }
                    }
                    node.build()
                },
            ),
            Item::Trait(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    let mut prefix = String::new();
                    if x.unsafety.is_some() {
                        prefix.push_str("unsafe ");
                    }
                    if x.auto_token.is_some() {
                        prefix.push_str("auto ");
                    }
                    prefix.push_str("trait ");
                    prefix.push_str(&x.ident.to_string());
                    node.seg(out, &prefix);
                    if !x.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.generics));
                    }
                    if x.colon_token.is_some() {
                        node.seg(out, ": ");
                        let indent = base_indent.indent();
                        for (i, pair) in x.supertraits.pairs().enumerate() {
                            if i > 0 {
                                /*
                                if let Some(p) = pair.punct() {
                                    node.add_comments(out, &indent, p.start());
                                }
                                */
                                node.seg(out, " +");
                                node.seg_unsplit(out, " ");
                            }
                            node.split(out, indent.clone());
                            node.child(pair.value().make_segs(out, &indent));
                        }
                    }
                    node.child(new_sg_block(out, base_indent, " {", &x.items));
                    node.build()
                },
            ),
            Item::TraitAlias(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    let mut prefix = String::new();
                    prefix.push_str("trait ");
                    prefix.push_str(&x.ident.to_string());
                    node.seg(out, &prefix);
                    if !x.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.generics));
                    }
                    append_binary(
                        out,
                        base_indent,
                        &mut node,
                        " =",
                        |out: &mut MakeSegsState, base_indent: &Alignment| {
                            let mut node = new_sg();
                            append_inline_list(out, base_indent, &mut node, " +", false, &x.bounds);
                            node.build()
                        },
                    );
                    node.seg(out, ";");
                    node.build()
                },
            ),
            Item::Type(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    let mut prefix = String::new();
                    prefix.push_str("type ");
                    prefix.push_str(&x.ident.to_string());
                    node.seg(out, &prefix);
                    if !x.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.generics));
                    }
                    append_binary(out, base_indent, &mut node, " =", x.ty.as_ref());
                    node.seg(out, ";");
                    node.build()
                },
            ),
            Item::Union(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "union ");
                    node.seg(out, &x.ident.to_string());
                    if !x.generics.params.is_empty() {
                        node.child(build_generics(out, base_indent, &x.generics));
                    }
                    append_comma_bracketed_list(
                        out,
                        base_indent,
                        &mut node,
                        "{",
                        &x.fields.named,
                        "}",
                    );
                    node.build()
                },
            ),
            Item::Use(x) => new_sg_attrs(
                out,
                base_indent,
                &x.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    append_vis(out, base_indent, &mut node, &x.vis);
                    node.seg(out, "use ");
                    if x.leading_colon.is_some() {
                        node.seg(out, "::");
                    }
                    node.child(x.tree.make_segs(out, base_indent));
                    node.seg(out, ";");
                    node.build()
                },
            ),
            Item::Verbatim(x) => {
                let mut node = new_sg();
                node.seg(out, &x.to_string());
                node.build()
            }
            _ => unreachable!(),
        }
    }
}

impl Formattable for Variant {
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
                node.seg(out, &self.ident);
                match &self.fields {
                    syn::Fields::Named(s) => {
                        append_comma_bracketed_list(
                            out,
                            base_indent,
                            &mut node,
                            "{",
                            &s.named,
                            "}",
                        );
                    }
                    syn::Fields::Unnamed(t) => {
                        append_comma_bracketed_list(
                            out,
                            base_indent,
                            &mut node,
                            "(",
                            &t.unnamed,
                            ")",
                        );
                    }
                    syn::Fields::Unit => {}
                }
                if let Some(e) = &self.discriminant {
                    append_binary(out, base_indent, &mut node, " = ", &e.1);
                }
                node.build()
            },
        )
    }
}

impl Formattable for Field {
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
                append_vis(out, base_indent, &mut node, &self.vis);
                if let Some(n) = &self.ident {
                    node.seg(out, &format!("{}: ", n));
                }
                node.child((&self.ty).make_segs(out, base_indent));
                node.build()
            },
        )
    }
}

impl Formattable for &UseTree {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        let mut node = new_sg();
        match self {
            syn::UseTree::Path(x) => {
                node.seg(out, &format!("{}::", x.ident));
                node.child(x.tree.make_segs(out, base_indent));
            }
            syn::UseTree::Name(x) => {
                node.seg(out, &x.ident.to_string());
            }
            syn::UseTree::Rename(x) => {
                node.seg(out, format!("{} as {}", x.ident, x.rename));
            }
            syn::UseTree::Glob(_) => {
                node.seg(out, "*");
            }
            syn::UseTree::Group(x) => {
                append_comma_bracketed_list(out, base_indent, &mut node, "{", &x.items, "}");
            }
        }
        node.build()
    }
}

impl Formattable for UseTree {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        (&self).make_segs(out, base_indent)
    }
}
