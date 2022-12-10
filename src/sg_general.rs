use std::{cell::RefCell, fmt::Write, rc::Rc};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{punctuated::Punctuated, Attribute, Block, ExprCall, Macro};

use crate::{
    new_sg, sg_type::build_path, Alignment, Formattable, MakeSegsState, SplitGroup,
    SplitGroupBuilder,
};

pub(crate) fn append_binary(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    tok: &str,
    right: impl Formattable,
) {
    node.seg(out, tok);
    let indent = base_indent.indent();
    node.split(out, indent.clone());
    node.seg_unsplit(out, " ");
    node.child(right.make_segs(out, &indent));
}

pub(crate) fn new_sg_binary(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    left: impl Formattable,
    tok: &str,
    right: impl Formattable,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.child(left.make_segs(out, base_indent));
    append_binary(out, base_indent, &mut node, tok, right);
    node.build()
}

pub(crate) fn build_rev_split_pair(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: impl Formattable,
    right: impl Formattable,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.child(base.make_segs(out, base_indent));
    node.split(out, base_indent.clone());
    node.child(right.make_segs(out, base_indent));
    let out = node.build();
    out.borrow_mut().children.reverse();
    out
}

pub(crate) fn append_statement_list(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    block: &Vec<impl Formattable>,
) {
    node.seg_unsplit(out, " ");
    let indent = base_indent.indent();
    for el in block {
        node.split(out, indent.clone());
        node.child((&el).make_segs(out, base_indent));
        node.seg_unsplit(out, " ");
    }
}

pub(crate) fn append_block(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    prefix: &'static str,
    block: &Vec<impl Formattable>,
) {
    node.seg(out, prefix);
    append_statement_list(out, base_indent, node, block);
    node.split(out, base_indent.clone());
    node.seg(out, "}");
}

pub(crate) fn new_sg_block(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    prefix: &'static str,
    block: &Vec<impl Formattable>,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    append_block(out, base_indent, &mut node, prefix, block);
    node.build()
}

pub(crate) fn append_inline_list<E: Formattable, T>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    punct: &str,
    punct_is_suffix: bool,
    exprs: &Punctuated<E, T>,
) {
    let indent = base_indent.indent();
    for (i, pair) in exprs.pairs().enumerate() {
        node.split(out, indent.clone());
        node.child(pair.value().make_segs(out, &indent));
        if i < exprs.len() - 1 {
            node.seg_split(out, punct);
            node.seg_unsplit(out, " ");
        } else {
            if punct_is_suffix {
                node.seg_split(out, punct);
            }
        }
    }
}

pub(crate) fn append_comma_bracketed_list<E: Formattable, T>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    suffix: &str,
) {
    //node.add_comments(out, base_indent, prefix_start);
    node.seg(out, prefix);
    append_inline_list(out, base_indent, node, ",", true, exprs);
    node.split(out, base_indent.clone());
    node.seg(out, suffix);
}

pub(crate) fn new_sg_comma_bracketed_list<E: Formattable, T>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: Option<impl Formattable>,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    suffix: &str,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    if let Some(base) = base {
        node.child(base.make_segs(out, base_indent));
    }
    append_comma_bracketed_list(out, base_indent, &mut node, prefix, exprs, suffix);
    node.build()
}

pub(crate) fn new_sg_comma_bracketed_list_ext<E: Formattable, T>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: Option<impl Formattable>,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    extra: impl Formattable,
    suffix: &str,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    if let Some(base) = base {
        node.child(base.make_segs(out, base_indent));
    }
    node.seg(out, prefix);
    let indent = base_indent.indent();
    for pair in exprs.pairs() {
        node.split(out, indent.clone());
        node.child((&pair.value()).make_segs(out, &indent));
        node.seg(out, ",");
        node.seg_unsplit(out, " ");
    }
    node.split(out, indent.clone());
    node.child(extra.make_segs(out, base_indent));
    node.seg_unsplit(out, " ");
    node.split(out, base_indent.clone());
    node.seg(out, suffix);
    node.build()
}

pub(crate) fn new_sg_attrs(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    attrs: &Vec<Attribute>,
    child: impl Formattable,
) -> Rc<RefCell<SplitGroup>> {
    if attrs.is_empty() {
        return child.make_segs(out, base_indent);
    }
    let mut node = new_sg();
    for attr in attrs {
        node.child({
            let mut node = new_sg();
            let mut prefix = String::new();
            prefix.write_str("#").unwrap();
            match attr.style {
                syn::AttrStyle::Outer => {}
                syn::AttrStyle::Inner(_) => {
                    prefix.write_str("!").unwrap();
                }
            };
            prefix.write_str("[").unwrap();
            append_macro_body(out, base_indent, &mut node, attr.tokens.clone());
            node.build()
        });
        node.split(out, base_indent.clone());
        node.seg_unsplit(out, " ");
    }
    node.child(child.make_segs(out, base_indent));
    node.build()
}

pub(crate) fn new_sg_macro(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    mac: &Macro,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.child(build_path(out, base_indent, &mac.path));
    node.seg(out, "!");
    let indent = base_indent.indent();
    match mac.delimiter {
        syn::MacroDelimiter::Paren(_) => {
            node.seg(out, "(");
            append_macro_body(out, &indent, &mut node, mac.tokens.clone());
            node.split(out, base_indent.clone());
            node.seg(out, ")");
        }
        syn::MacroDelimiter::Brace(_) => {
            node.seg(out, "{");
            append_macro_body(out, &indent, &mut node, mac.tokens.clone());
            node.split(out, base_indent.clone());
            node.seg(out, "}");
        }
        syn::MacroDelimiter::Bracket(_) => {
            node.seg(out, "[");
            append_macro_body(out, &indent, &mut node, mac.tokens.clone());
            node.split(out, base_indent.clone());
            node.seg(out, "]");
        }
    }
    node.build()
}

pub(crate) fn append_macro_body(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    tokens: TokenStream,
) {
    if let Ok(exprs) = syn::parse2::<ExprCall>(quote! { f(#tokens) }) {
        append_inline_list(out, base_indent, node, ",", true, &exprs.args);
    } else if let Ok(block) = syn::parse2::<Block>(quote! { { #tokens } }) {
        append_statement_list(out, base_indent, node, &block.stmts);
    } else {
        fn recurse_tokens(
            out: &mut MakeSegsState,
            base_indent: &Alignment,
            node: &mut SplitGroupBuilder,
            tokens: TokenStream,
        ) {
            let mut need_space = false;
            for t in tokens {
                match t {
                    proc_macro2::TokenTree::Group(g) => {
                        let mut child = new_sg();
                        let indent = base_indent.clone();
                        match g.delimiter() {
                            proc_macro2::Delimiter::Parenthesis => {
                                child.seg(out, "(");
                                recurse_tokens(out, base_indent, &mut child, g.stream());
                                child.split(out, indent);
                                child.seg(out, ")");
                            }
                            proc_macro2::Delimiter::Brace => {
                                child.seg(out, "{");
                                recurse_tokens(out, base_indent, &mut child, g.stream());
                                child.split(out, indent);
                                child.seg(out, "}");
                            }
                            proc_macro2::Delimiter::Bracket => {
                                child.seg(out, "[");
                                recurse_tokens(out, base_indent, &mut child, g.stream());
                                child.split(out, indent);
                                child.seg(out, "]");
                            }
                            proc_macro2::Delimiter::None => {
                                // TODO needs verification
                                recurse_tokens(out, base_indent, &mut child, g.stream());
                                child.split(out, indent);
                            }
                        }
                        node.child(child.build());
                        need_space = true;
                    }
                    proc_macro2::TokenTree::Ident(i) => {
                        if need_space {
                            node.seg(out, " ");
                        }
                        node.seg(out, &i.to_string());
                        need_space = true;
                    }
                    proc_macro2::TokenTree::Punct(p) => match p.as_char() {
                        ';' | ',' => {
                            node.seg(out, &p.to_string());
                            node.split(out, base_indent.clone());
                            node.seg_unsplit(out, " ");
                            need_space = false;
                        }
                        '.' => {
                            node.seg(out, &p.to_string());
                            need_space = false;
                        }
                        _ => {
                            if need_space {
                                node.seg(out, " ");
                            }
                            node.seg(out, &p.to_string());
                            need_space = true;
                        }
                    },
                    proc_macro2::TokenTree::Literal(l) => {
                        if need_space {
                            node.seg(out, " ");
                        }
                        node.seg(out, &l.to_string());
                        need_space = true;
                    }
                }
            }
        }
        recurse_tokens(out, base_indent, node, tokens)
    }
}
