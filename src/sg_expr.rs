use std::{cell::RefCell, rc::Rc};

use quote::ToTokens;
use syn::{punctuated::Punctuated, token::Comma, Expr, ExprClosure};

use crate::{
    new_sg,
    sg_general::{
        append_binary, append_block, build_rev_pair, new_sg_attrs, new_sg_binary, new_sg_block,
        new_sg_comma_bracketed_list, new_sg_macro,
    },
    sg_type::{append_path, build_array_type, build_extended_path, build_path, build_ref},
    Alignment, Formattable, MakeSegsState, SplitGroup,
};

impl Formattable for Expr {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        (&self).make_segs(out, base_indent)
    }
}

impl Formattable for &Expr {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        match self {
            Expr::Array(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, "[", &e.elems, "]")
                },
            ),
            Expr::Assign(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, e.left.as_ref(), " =", e.right.as_ref())
                },
            ),
            Expr::AssignOp(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(
                        out,
                        base_indent,
                        e.left.as_ref(),
                        &format!(" {}", e.op.to_token_stream()),
                        e.right.as_ref(),
                    )
                },
            ),
            Expr::Async(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    if e.capture.is_some() {
                        new_sg_block(out, base_indent, "async move {", &e.block.stmts)
                    } else {
                        new_sg_block(out, base_indent, "async {", &e.block.stmts)
                    }
                },
            ),
            Expr::Await(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child(e.base.make_segs(out, base_indent));
                    node.seg(out, ".await");
                    node.build()
                },
            ),
            Expr::Binary(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(
                        out,
                        base_indent,
                        e.left.as_ref(),
                        &format!(" {}", e.op.to_token_stream()),
                        e.right.as_ref(),
                    )
                },
            ),
            Expr::Block(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_block(out, base_indent, "{", &e.block.stmts)
                },
            ),
            Expr::Box(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "box ");
                    node.child(e.expr.make_segs(out, base_indent));
                    node.build()
                },
            ),
            Expr::Break(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "break");
                    if let Some(l) = &e.label {
                        node.seg(out, &format!(" '{}", l.ident))
                    }
                    if let Some(e) = &e.expr {
                        node.seg(out, " ");
                        node.child(e.make_segs(out, base_indent));
                    }
                    node.build()
                },
            ),
            Expr::Call(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        Some(e.func.as_ref()),
                        "(",
                        &e.args,
                        ")",
                    )
                },
            ),
            Expr::Cast(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, e.expr.as_ref(), " as", e.ty.as_ref())
                },
            ),
            Expr::Closure(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    fn build_base(
                        out: &mut MakeSegsState,
                        base_indent: &Alignment,
                        e: &ExprClosure,
                    ) -> Rc<RefCell<SplitGroup>> {
                        let mut prefix = String::new();
                        let mut need_space = false;
                        if e.movability.is_some() {
                            prefix.push_str("static");
                            need_space = true;
                        }
                        if e.asyncness.is_some() {
                            if need_space {
                                prefix.push_str(" ");
                            }
                            prefix.push_str("async");
                            need_space = true;
                        }
                        if e.capture.is_some() {
                            if need_space {
                                prefix.push_str(" ");
                            }
                            prefix.push_str("move");
                            need_space = true;
                        }
                        if need_space {
                            prefix.push_str(" ");
                        }
                        prefix.push_str("|");
                        new_sg_comma_bracketed_list(
                            out,
                            base_indent,
                            None::<Expr>,
                            &prefix,
                            &e.inputs,
                            "|",
                        )
                    }
                    match &e.output {
                        syn::ReturnType::Default => build_rev_pair(
                            out,
                            base_indent,
                            |out: &mut MakeSegsState, base_indent: &Alignment| {
                                build_base(out, base_indent, e)
                            },
                            e.body.as_ref(),
                        ),
                        syn::ReturnType::Type(_, output) => build_rev_pair(
                            out,
                            base_indent,
                            |out: &mut MakeSegsState, base_indent: &Alignment| {
                                new_sg_binary(
                                    out,
                                    base_indent,
                                    |out: &mut MakeSegsState, base_indent: &Alignment| {
                                        build_base(out, base_indent, e)
                                    },
                                    " ->",
                                    output.as_ref(),
                                )
                            },
                            e.body.as_ref(),
                        ),
                    }
                },
            ),
            Expr::Continue(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut prefix = "continue".to_string();
                    if let Some(label) = &e.label {
                        prefix.push_str(" ");
                        prefix.push_str(&label.to_string());
                    }
                    let mut node = new_sg();
                    node.seg(out, &prefix);
                    node.build()
                },
            ),
            Expr::Field(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    // TODO chain here, follow calls, .await, ?
                    let mut node = new_sg();
                    node.child(e.base.make_segs(out, base_indent));
                    node.seg(
                        out,
                        &format!(
                            ".{}",
                            match &e.member {
                                syn::Member::Named(n) => n.to_string(),
                                syn::Member::Unnamed(u) => u.index.to_string(),
                            }
                        ),
                    );
                    node.build()
                },
            ),
            Expr::ForLoop(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child({
                        let mut node = new_sg();
                        node.child({
                            let mut node = new_sg();
                            if let Some(l) = &e.label {
                                node.seg(out, &format!("{} ", l.to_token_stream().to_string()));
                            }
                            node.seg(out, "for ");
                            node.child(e.pat.make_segs(out, base_indent));
                            node.build()
                        });
                        node.seg(out, " in ");
                        node.child(e.expr.make_segs(out, base_indent));
                        node.build()
                    });
                    append_block(out, base_indent, &mut node, " {", &e.body.stmts);
                    node.build()
                },
            ),
            Expr::Group(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    e.expr.make_segs(out, base_indent)
                },
            ),
            Expr::If(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child({
                        let mut node = new_sg();
                        node.child({
                            let mut node = new_sg();
                            node.seg(out, "if ");
                            node.child(e.cond.make_segs(out, base_indent));
                            node.build()
                        });
                        append_block(out, base_indent, &mut node, " {", &e.then_branch.stmts);
                        node.build()
                    });
                    if let Some((_, else_branch)) = &e.else_branch {
                        node.seg(out, " else ");
                        node.child(else_branch.make_segs(out, base_indent));
                    }
                    node.build()
                },
            ),
            Expr::Index(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut exprs: Punctuated<&Expr, Comma> = Punctuated::new();
                    exprs.push_value(e.index.as_ref());
                    new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        Some(e.expr.as_ref()),
                        "[",
                        &exprs,
                        "]",
                    )
                },
            ),
            Expr::Let(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "let ");
                    node.child(e.pat.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut node, " =", e.expr.as_ref());
                    node.build()
                },
            ),
            Expr::Lit(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, e.lit.to_token_stream());
                    node.build()
                },
            ),
            Expr::Loop(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    if let Some(l) = &e.label {
                        node.seg(out, format!("{} ", l.to_token_stream()));
                    }
                    append_block(out, base_indent, &mut node, "loop {", &e.body.stmts);
                    node.build()
                },
            ),
            Expr::Macro(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &e.mac, false)
                },
            ),
            Expr::Match(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    sg.seg(out, "match ");
                    sg.child(e.expr.make_segs(out, base_indent));
                    sg.seg(out, " {");
                    sg.seg_unsplit(out, " ");
                    let indent = base_indent.indent();
                    for (i, el) in e.arms.iter().enumerate() {
                        sg.split(out, indent.clone());
                        sg.child({
                            let mut sg = new_sg();
                            sg.child({
                                if let Some(guard) = &el.guard {
                                    new_sg_binary(out, &indent, &el.pat, " if", guard.1.as_ref())
                                } else {
                                    el.pat.make_segs(out, &indent)
                                }
                            });
                            sg.seg(out, " => ");
                            sg.child(el.body.make_segs(out, &indent));
                            let out = sg.build();
                            out.borrow_mut().children.reverse();
                            out
                        });
                        if i == e.arms.len() - 1 {
                            sg.seg_split(out, ",");
                        } else {
                            sg.seg(out, ",");
                        }
                        sg.seg_unsplit(out, " ");
                    }
                    sg.split(out, base_indent.clone());
                    sg.seg(out, "}");
                    sg.build()
                },
            ),
            Expr::MethodCall(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        Some(|out: &mut MakeSegsState, base_indent: &Alignment| {
                            let build_base = |out: &mut MakeSegsState, base_indent: &Alignment| {
                                let mut node = new_sg();
                                node.child(e.receiver.make_segs(out, base_indent));
                                node.seg(out, format!(".{}", e.method));
                                node.build()
                            };
                            if let Some(tf) = &e.turbofish {
                                new_sg_comma_bracketed_list(
                                    out,
                                    base_indent,
                                    Some(build_base),
                                    "::<",
                                    &tf.args,
                                    ">",
                                )
                            } else {
                                build_base(out, base_indent)
                            }
                        }),
                        "(",
                        &e.args,
                        ")",
                    )
                },
            ),
            Expr::Paren(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut exprs: Punctuated<&Expr, Comma> = Punctuated::new();
                    exprs.push_value(e.expr.as_ref());
                    new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, "(", &exprs, ")")
                },
            ),
            Expr::Path(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_extended_path(out, base_indent, &e.qself, &e.path)
                },
            ),
            Expr::Range(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let tok = match e.limits {
                        syn::RangeLimits::HalfOpen(_) => "..",
                        syn::RangeLimits::Closed(_) => "..=",
                    };
                    match (&e.from, &e.to) {
                        (None, None) => unreachable!(),
                        (None, Some(r)) => {
                            let mut node = new_sg();
                            node.seg(out, tok);
                            node.child(r.as_ref().make_segs(out, base_indent));
                            node.build()
                        }
                        (Some(l), None) => {
                            let mut node = new_sg();
                            node.child(l.as_ref().make_segs(out, base_indent));
                            node.seg(out, tok);
                            node.build()
                        }
                        (Some(l), Some(r)) => new_sg_binary(
                            out,
                            base_indent,
                            l.as_ref(),
                            &format!(" {}", tok),
                            r.as_ref(),
                        ),
                    }
                },
            ),
            Expr::Reference(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_ref(out, base_indent, e.mutability.is_some(), e.expr.as_ref())
                },
            ),
            Expr::Repeat(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_array_type(out, base_indent, e.expr.as_ref(), e.len.as_ref())
                },
            ),
            Expr::Return(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "return");
                    if let Some(e) = &e.expr {
                        node.seg(out, " ");
                        node.child(e.make_segs(out, base_indent));
                    }
                    node.build()
                },
            ),
            Expr::Struct(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child(build_path(out, base_indent, &e.path));
                    node.seg(out, " {");
                    let indent = base_indent.indent();
                    for (i, pair) in e.fields.pairs().enumerate() {
                        if i > 0 {
                            node.seg_split(out, ",");
                        }
                        node.seg_unsplit(out, " ");
                        node.split(out, indent.clone());
                        match &pair.value().member {
                            syn::Member::Named(n) => {
                                node.seg(out, &format!("{}: ", n));
                            }
                            syn::Member::Unnamed(_) => unreachable!(),
                        };
                        node.child(pair.value().expr.make_segs(out, &indent));
                    }
                    if let Some(rem) = &e.rest {
                        node.seg(out, ",");
                        node.seg_unsplit(out, " ");
                        node.split(out, indent.clone());
                        node.seg(out, "..");
                        node.child(rem.make_segs(out, &indent));
                    } else {
                        node.seg_split(out, ",");
                    }
                    node.seg_unsplit(out, " ");
                    node.split(out, base_indent.clone());
                    node.seg(out, "}");
                    node.build()
                },
            ),
            Expr::Try(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child(e.expr.make_segs(out, base_indent));
                    node.seg(out, "?");
                    node.build()
                },
            ),
            Expr::TryBlock(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "try ");
                    append_block(out, base_indent, &mut node, " {", &e.block.stmts);
                    node.build()
                },
            ),
            Expr::Tuple(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(out, base_indent, None::<Expr>, "(", &e.elems, ")")
                },
            ),
            Expr::Type(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.child(e.expr.make_segs(out, base_indent));
                    node.seg(out, ":");
                    node.seg_unsplit(out, " ");
                    let indent = base_indent.indent();
                    node.split(out, indent.clone());
                    node.child(e.ty.make_segs(out, &indent));
                    node.build()
                },
            ),
            Expr::Unary(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, &e.op.to_token_stream().to_string());
                    node.child(e.expr.make_segs(out, base_indent));
                    node.build()
                },
            ),
            Expr::Unsafe(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "unsafe ");
                    append_block(out, base_indent, &mut node, " {", &e.block.stmts);
                    node.build()
                },
            ),
            Expr::Verbatim(e) => {
                let mut node = new_sg();
                node.seg(out, &e.to_string());
                node.build()
            }
            Expr::While(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    if let Some(l) = &e.label {
                        node.seg(out, format!("{}: ", l.to_token_stream()));
                    }
                    node.seg(out, "while ");
                    node.child(e.cond.make_segs(out, base_indent));
                    append_block(out, base_indent, &mut node, " {", &e.body.stmts);
                    node.build()
                },
            ),
            Expr::Yield(e) => new_sg_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut node = new_sg();
                    node.seg(out, "yield");
                    if let Some(e) = &e.expr {
                        node.seg(out, " ");
                        node.child(e.make_segs(out, base_indent));
                    }
                    node.build()
                },
            ),
            _ => unreachable!(),
        }
    }
}
