use std::{cell::RefCell, fmt::Write, rc::Rc};

use proc_macro2::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, Attribute, Block, ExprCall, Macro};

use crate::{
    new_sg, sg_type::build_path, Alignment, Formattable, MakeSegsState, SplitGroup,
    SplitGroupBuilder,
};

pub(crate) fn build_rev_pair(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    base: impl Formattable,
    right: impl Formattable,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.child(base.make_segs(out, base_indent));
    node.seg(out, " ");
    node.child(right.make_segs(out, base_indent));
    let out = node.build();
    out.borrow_mut().children.reverse();
    out
}

pub(crate) fn append_binary(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    tok: &str,
    right: impl Formattable,
) {
    node.seg(out, tok);
    let indent = base_indent.indent();
    node.split(out, indent.clone(), true);
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

pub(crate) fn append_statement_list(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    node: &mut SplitGroupBuilder,
    block: &Vec<impl Formattable>,
) {
    if block.len() > 1 {
        let indent = base_indent.indent();
        for el in block {
            node.split_always(out, indent.clone(), true);
            node.child((&el).make_segs(out, &indent));
        }
    } else {
        node.seg_unsplit(out, " ");
        let indent = base_indent.indent();
        for el in block {
            node.split(out, indent.clone(), true);
            node.child((&el).make_segs(out, &indent));
            node.seg_unsplit(out, " ");
        }
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
    if block.len() > 1 {
        node.split_always(out, base_indent.clone(), false);
    } else {
        node.split(out, base_indent.clone(), false);
    }
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
        node.split(out, indent.clone(), true);
        node.child(pair.value().make_segs(out, &indent));
        if i < exprs.len() - 1 {
            node.seg(out, punct);
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
    node.split(out, base_indent.clone(), false);
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
        node.split(out, indent.clone(), true);
        node.child((&pair.value()).make_segs(out, &indent));
        node.seg(out, ",");
        node.seg_unsplit(out, " ");
    }
    node.split(out, indent.clone(), true);
    node.child(extra.make_segs(out, base_indent));
    node.seg_unsplit(out, " ");
    node.split(out, base_indent.clone(), false);
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
            node.seg(out, prefix);
            node.child(build_path(out, base_indent, &attr.path));
            append_macro_body(out, base_indent, &mut node, attr.tokens.clone());
            node.seg(out, "]");
            node.build()
        });
        node.split(out, base_indent.clone(), false);
        node.seg_unsplit(out, " ");
    }
    node.child(child.make_segs(out, base_indent));
    node.build()
}

pub(crate) fn new_sg_macro(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    mac: &Macro,
    semi: bool,
) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.child(build_path(out, base_indent, &mac.path));
    node.seg(out, "!");
    let indent = base_indent.indent();
    match mac.delimiter {
        syn::MacroDelimiter::Paren(_) => {
            node.seg(out, "(");
            append_macro_body(out, &indent, &mut node, mac.tokens.clone());
            node.split(out, base_indent.clone(), false);
            node.seg(out, ")");
        }
        syn::MacroDelimiter::Brace(_) => {
            node.seg(out, "{");
            append_macro_body(out, &indent, &mut node, mac.tokens.clone());
            node.split(out, base_indent.clone(), false);
            node.seg(out, "}");
        }
        syn::MacroDelimiter::Bracket(_) => {
            node.seg(out, "[");
            append_macro_body(out, &indent, &mut node, mac.tokens.clone());
            node.split(out, base_indent.clone(), false);
            node.seg(out, "]");
        }
    }
    if semi {
        node.seg(out, ";");
    }
    node.build()
}

pub(crate) fn append_macro_body(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    tokens: TokenStream,
) {
    if let Ok(exprs) = syn::parse2::<ExprCall>(quote! { f(#tokens) }) {
        append_inline_list(out, base_indent, sg, ",", true, &exprs.args);
    } else if let Ok(block) = syn::parse2::<Block>(quote! { { #tokens } }) {
        append_statement_list(out, base_indent, sg, &block.stmts);
    } else {
        #[derive(PartialEq)]
        enum ConsecMode {
            StartJoin, // Start, joining punct (.)
            IdentLit,  // Idents, literals
            Punct,     // Other punctuation
        }
        let mut mode = ConsecMode::StartJoin;

        let mut substreams = vec![];
        let mut top = vec![];
        for t in tokens {
            match t {
                proc_macro2::TokenTree::Punct(p)
                    if match p.as_char() {
                        ';' | ',' => true,
                        _ => false,
                    } =>
                {
                    substreams.push((top.split_off(0), Some(p)));
                }
                _ => {
                    top.push(t);
                }
            }
        }
        if !top.is_empty() {
            substreams.push((top, None));
        }

        let substreams_len = substreams.len();
        for (i, sub) in substreams.into_iter().enumerate() {
            sg.split(out, base_indent.clone(), true);
            let tokens = TokenStream::from_iter(sub.0);
            if let Ok(exprs) = syn::parse2::<ExprCall>(quote! { f(#tokens) }) {
                append_inline_list(out, base_indent, sg, ",", true, &exprs.args);
            } else if let Ok(block) = syn::parse2::<Block>(quote! { { #tokens } }) {
                append_statement_list(out, base_indent, sg, &block.stmts);
            } else {
                for t in tokens {
                    match t {
                        proc_macro2::TokenTree::Group(g) => {
                            sg.child({
                                let mut sg = new_sg();
                                let indent = base_indent.indent();
                                match g.delimiter() {
                                    proc_macro2::Delimiter::Parenthesis => {
                                        sg.seg(out, "(");
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                        sg.split(out, base_indent.clone(), false);
                                        sg.seg(out, ")");
                                    }
                                    proc_macro2::Delimiter::Brace => {
                                        match mode {
                                            ConsecMode::StartJoin => {}
                                            _ => {
                                                sg.seg(out, " ");
                                            }
                                        }
                                        sg.seg(out, "{");
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                        sg.split(out, base_indent.clone(), false);
                                        sg.seg(out, "}");
                                    }
                                    proc_macro2::Delimiter::Bracket => {
                                        sg.seg(out, "[");
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                        sg.split(out, base_indent.clone(), false);
                                        sg.seg(out, "]");
                                    }
                                    proc_macro2::Delimiter::None => {
                                        // TODO needs verification
                                        append_macro_body(out, &indent, &mut sg, g.stream());
                                    }
                                }
                                sg.build()
                            });

                            mode = ConsecMode::IdentLit;
                        }
                        proc_macro2::TokenTree::Ident(i) => {
                            match mode {
                                ConsecMode::StartJoin => {}
                                ConsecMode::IdentLit | ConsecMode::Punct => {
                                    sg.seg(out, " ");
                                }
                            }
                            sg.seg(out, &i.to_string());
                            mode = ConsecMode::IdentLit;
                        }
                        proc_macro2::TokenTree::Punct(p) => match p.as_char() {
                            '\'' => {
                                match mode {
                                    ConsecMode::StartJoin => {}
                                    ConsecMode::IdentLit | ConsecMode::Punct => {
                                        sg.seg(out, " ");
                                    }
                                }
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::StartJoin;
                            }
                            _ => {
                                match mode {
                                    ConsecMode::StartJoin => {}
                                    ConsecMode::IdentLit => {
                                        sg.seg(out, " ");
                                    }
                                    ConsecMode::Punct => {}
                                }
                                sg.seg(out, &p.to_string());
                                mode = ConsecMode::Punct;
                            }
                        },
                        proc_macro2::TokenTree::Literal(l) => {
                            match mode {
                                ConsecMode::StartJoin => {}
                                ConsecMode::IdentLit | ConsecMode::Punct => {
                                    sg.seg(out, " ");
                                }
                            }
                            sg.seg(out, &l.to_string());
                            mode = ConsecMode::IdentLit;
                        }
                    }
                }
            }
            if let Some(suf) = sub.1 {
                sg.seg(out, suf);
            }
            if i < substreams_len - 1 {
                sg.seg_unsplit(out, " ");
            }
        }
    }
}
