use quote::ToTokens;
use syn::{
    Expr,
    ExprAwait,
    ExprClosure,
    ExprField,
    ExprMethodCall,
};
use crate::{
    new_sg,
    new_sg_lit,
    sg_general::{
        append_binary,
        append_bracketed_statement_list,
        append_comments,
        build_rev_pair,
        new_sg_outer_attrs,
        new_sg_binary,
        new_sg_block,
        new_sg_comma_bracketed_list,
        new_sg_macro,
    },
    sg_type::{
        build_array_type,
        build_extended_path,
        build_path,
        build_ref,
    },
    Alignment,
    Formattable,
    MakeSegsState,
    TrivialLineColMath,
    check_split_brace_threshold,
    SplitGroupIdx,
};

#[derive(Clone)]
enum Dotted<'a> {
    Await(&'a ExprAwait),
    Field(&'a ExprField),
    Method(&'a ExprMethodCall),
    // only if base is dotted
    Try(Box<Dotted<'a>>),
}

enum DottedRes<'a> {
    Dotted(Dotted<'a>),
    Leaf(&'a Expr),
}

fn get_dotted<'a>(e: &'a Expr) -> DottedRes<'a> {
    match e {
        Expr::Await(x) => DottedRes::Dotted(Dotted::Await(x)),
        Expr::Field(x) => DottedRes::Dotted(Dotted::Field(x)),
        Expr::MethodCall(x) => DottedRes::Dotted(Dotted::Method(x)),
        Expr::Try(x) => match get_dotted(&x.expr) {
            DottedRes::Dotted(d) => DottedRes::Dotted(Dotted::Try(Box::new(d))),
            DottedRes::Leaf(_) => DottedRes::Leaf(e),
        },
        _ => DottedRes::Leaf(e),
    }
}

fn gather_dotted<'a, 'b: 'a>(out: &'a mut Vec<Dotted<'b>>, root: Dotted<'b>) -> &'b dyn Formattable {
    out.push(root.clone());
    match match root {
        Dotted::Await(x) => get_dotted(&x.base),
        Dotted::Field(x) => get_dotted(&x.base),
        Dotted::Method(x) => get_dotted(&x.receiver),
        Dotted::Try(e) => match &*e {
            Dotted::Await(x) => get_dotted(&x.base),
            Dotted::Field(x) => get_dotted(&x.base),
            Dotted::Method(x) => get_dotted(&x.receiver),
            Dotted::Try(_) => unreachable!(),
        },
    } {
        DottedRes::Dotted(d) => gather_dotted(out, d),
        DottedRes::Leaf(l) => l,
    }
}

fn new_sg_dotted(out: &mut MakeSegsState, base_indent: &Alignment, root: Dotted) -> SplitGroupIdx {
    let mut children = vec![];
    let leaf = gather_dotted(&mut children, root);
    children.reverse();

    fn build_child(out: &mut MakeSegsState, base_indent: &Alignment, child: &Dotted) -> SplitGroupIdx {
        match child {
            Dotted::Await(x) => {
                new_sg_lit(out, Some((base_indent, x.dot_token.span.start())), ".await")
            },
            Dotted::Field(e) => new_sg_lit(
                out,
                Some((base_indent, e.dot_token.span.start())),
                format!(".{}", match &e.member {
                    syn::Member::Named(n) => n.to_string(),
                    syn::Member::Unnamed(u) => u.index.to_string(),
                }),
            ),
            Dotted::Method(e) => new_sg_comma_bracketed_list(
                out,
                base_indent,
                Some(|out: &mut MakeSegsState, base_indent: &Alignment| {
                    let build_base = |out: &mut MakeSegsState, _base_indent: &Alignment| {
                        new_sg_lit(out, Some((base_indent, e.dot_token.span.start())), format!(".{}", e.method))
                    };
                    if let Some(tf) = &e.turbofish {
                        new_sg_comma_bracketed_list(
                            out,
                            base_indent,
                            Some(build_base),
                            tf.colon2_token.spans[0].start(),
                            "::<",
                            &tf.args,
                            tf.lt_token.span.start(),
                            ">",
                        )
                    } else {
                        build_base(out, base_indent)
                    }
                }),
                e.paren_token.span.start(),
                "(",
                &e.args,
                e.paren_token.span.end().prev(),
                ")",
            ),
            Dotted::Try(inner) => {
                let mut sg = new_sg(out);
                sg.child(build_child(out, base_indent, inner.as_ref()));
                sg.seg(out, "?");
                sg.build(out)
            },
        }
    }

    let mut sg = new_sg(out);
    sg.child(leaf.make_segs(out, base_indent));
    if children.len() > 1 {
        let indent = base_indent.indent();
        for child in children {
            sg.split(out, indent.clone(), true);
            sg.child(build_child(out, &indent, &child));
        }
    } else {
        sg.child(build_child(out, base_indent, &children.get(0).unwrap()));
    }
    sg.build(out)
}

impl Formattable for Expr {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        (&self).make_segs(out, base_indent)
    }
}

impl Formattable for &Expr {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        match self {
            Expr::Array(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        None::<Expr>,
                        e.bracket_token.span.start(),
                        "[",
                        &e.elems,
                        e.bracket_token.span.end().prev(),
                        "]",
                    )
                },
            ),
            Expr::Assign(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, e.left.as_ref(), e.eq_token.span.start(), " =", e.right.as_ref())
                },
            ),
            Expr::AssignOp(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(
                        out,
                        base_indent,
                        e.left.as_ref(),
                        e.op.to_token_stream().into_iter().next().unwrap().span().start(),
                        &format!(" {}", e.op.to_token_stream()),
                        e.right.as_ref(),
                    )
                },
            ),
            Expr::Async(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    if let Some(c) = e.capture {
                        new_sg_block(
                            out,
                            base_indent,
                            c.span.start(),
                            "async move {",
                            Some(&e.attrs),
                            &e.block.stmts,
                            e.block.brace_token.span.end().prev(),
                        )
                    } else {
                        new_sg_block(
                            out,
                            base_indent,
                            e.async_token.span.start(),
                            "async {",
                            Some(&e.attrs),
                            &e.block.stmts,
                            e.block.brace_token.span.end().prev(),
                        )
                    }
                },
            ),
            Expr::Await(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_dotted(out, base_indent, Dotted::Await(e))
                },
            ),
            Expr::Binary(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(
                        out,
                        base_indent,
                        e.left.as_ref(),
                        e.op.to_token_stream().into_iter().next().unwrap().span().start(),
                        &format!(" {}", e.op.to_token_stream()),
                        e.right.as_ref(),
                    )
                },
            ),
            Expr::Block(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_block(
                        out,
                        base_indent,
                        e.block.brace_token.span.start(),
                        "{",
                        Some(&e.attrs),
                        &e.block.stmts,
                        e.block.brace_token.span.end().prev(),
                    )
                },
            ),
            Expr::Box(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.box_token.span.start());
                    sg.seg(out, "box ");
                    sg.child(e.expr.make_segs(out, base_indent));
                    sg.build(out)
                },
            ),
            Expr::Break(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.break_token.span.start());
                    sg.seg(out, "break");
                    if let Some(l) = &e.label {
                        sg.seg(out, &format!(" '{}", l.ident))
                    }
                    if let Some(e) = &e.expr {
                        sg.seg(out, " ");
                        sg.child(e.make_segs(out, base_indent));
                    }
                    sg.build(out)
                },
            ),
            Expr::Call(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        Some(e.func.as_ref()),
                        e.paren_token.span.start(),
                        "(",
                        &e.args,
                        e.paren_token.span.end().prev(),
                        ")",
                    )
                },
            ),
            Expr::Cast(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, e.expr.as_ref(), e.as_token.span.start(), " as", e.ty.as_ref())
                },
            ),
            Expr::Closure(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    fn build_base(
                        out: &mut MakeSegsState,
                        base_indent: &Alignment,
                        e: &ExprClosure,
                    ) -> SplitGroupIdx {
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
                            e.or1_token.span.start(),
                            &prefix,
                            &e.inputs,
                            e.or2_token.span.start(),
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
                        syn::ReturnType::Type(tok, output) => build_rev_pair(
                            out,
                            base_indent,
                            |out: &mut MakeSegsState, base_indent: &Alignment| {
                                new_sg_binary(out, base_indent, |out: &mut MakeSegsState, base_indent: &Alignment| {
                                    build_base(out, base_indent, e)
                                }, tok.spans[0].start(), " ->", output.as_ref())
                            },
                            e.body.as_ref(),
                        ),
                    }
                },
            ),
            Expr::Continue(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut prefix = "continue".to_string();
                    if let Some(label) = &e.label {
                        prefix.push_str(" ");
                        prefix.push_str(&label.to_string());
                    }
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.continue_token.span.start());
                    sg.seg(out, &prefix);
                    sg.build(out)
                },
            ),
            Expr::Field(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    new_sg_dotted(out, base_indent, Dotted::Field(e))
                },
            ),
            Expr::ForLoop(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child({
                        let mut sg = new_sg(out);
                        sg.child({
                            let mut sg = new_sg(out);
                            if let Some(l) = &e.label {
                                sg.seg(out, &format!("{}: ", l.name));
                            }
                            append_comments(out, base_indent, &mut sg, e.for_token.span.start());
                            sg.seg(out, "for ");
                            sg.child(e.pat.make_segs(out, base_indent));
                            sg.build(out)
                        });
                        sg.seg(out, " in ");
                        sg.child(e.expr.make_segs(out, base_indent));
                        sg.build(out)
                    });
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        Some(&e.attrs),
                        &e.body.stmts,
                        e.body.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Expr::Group(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    e.expr.make_segs(out, base_indent)
                },
            ),
            Expr::If(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child({
                        let mut sg = new_sg(out);
                        sg.child({
                            let mut sg = new_sg(out);
                            append_comments(out, base_indent, &mut sg, e.if_token.span.start());
                            sg.seg(out, "if ");
                            sg.child(e.cond.make_segs(out, base_indent));
                            sg.build(out)
                        });
                        append_bracketed_statement_list(
                            out,
                            base_indent,
                            &mut sg,
                            " {",
                            Some(&e.attrs),
                            &e.then_branch.stmts,
                            e.then_branch.brace_token.span.end().prev(),
                        );
                        sg.build(out)
                    });
                    if let Some((t, else_branch)) = &e.else_branch {
                        append_comments(out, base_indent, &mut sg, t.span.start());
                        sg.seg(out, " else ");
                        sg.child(else_branch.make_segs(out, base_indent));
                    }
                    sg.build(out)
                },
            ),
            Expr::Index(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    sg.child(e.expr.make_segs(out, base_indent));
                    sg.seg(out, "[");
                    sg.child(e.index.make_segs(out, base_indent));
                    sg.seg(out, "]");
                    sg.build(out)
                },
            ),
            Expr::Let(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.let_token.span.start());
                    sg.seg(out, "let ");
                    sg.child(e.pat.make_segs(out, base_indent));
                    append_binary(out, base_indent, &mut sg, " =", e.expr.as_ref());
                    sg.build(out)
                },
            ),
            Expr::Lit(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, _base_indent: &Alignment| {
                    let mut node = new_sg(out);
                    append_comments(out, base_indent, &mut node, e.lit.span().start());
                    node.seg(out, e.lit.to_token_stream());
                    node.build(out)
                },
            ),
            Expr::Loop(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    if let Some(l) = &e.label {
                        append_comments(out, base_indent, &mut sg, l.name.span().start());
                        sg.seg(out, format!("{} ", l.to_token_stream()));
                    }
                    append_comments(out, base_indent, &mut sg, e.loop_token.span.start());
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        "loop {",
                        Some(&e.attrs),
                        &e.body.stmts,
                        e.body.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Expr::Macro(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_macro(out, base_indent, &e.mac, false)
                },
            ),
            Expr::Match(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    if check_split_brace_threshold(out, e.arms.len()) {
                        sg.initial_split();
                    }
                    append_comments(out, base_indent, &mut sg, e.match_token.span.start());
                    sg.seg(out, "match ");
                    sg.child(e.expr.make_segs(out, base_indent));
                    sg.seg(out, " {");
                    sg.seg_unsplit(out, " ");
                    let indent = base_indent.indent();
                    for (i, arm) in e.arms.iter().enumerate() {
                        sg.split(out, indent.clone(), true);
                        sg.child(
                            new_sg_outer_attrs(
                                out,
                                &base_indent,
                                &arm.attrs,
                                |out: &mut MakeSegsState, base_indent: &Alignment| {
                                    let mut sg = new_sg(out);
                                    sg.child({
                                        if let Some(guard) = &arm.guard {
                                            new_sg_binary(
                                                out,
                                                &indent,
                                                &arm.pat,
                                                guard.0.span.start(),
                                                " if",
                                                guard.1.as_ref(),
                                            )
                                        } else {
                                            arm.pat.make_segs(out, &indent)
                                        }
                                    });
                                    append_comments(out, base_indent, &mut sg, arm.fat_arrow_token.spans[0].start());
                                    sg.seg(out, " => ");
                                    sg.child(arm.body.make_segs(out, &indent));
                                    sg.reverse_children();
                                    sg.build(out)
                                },
                            ),
                        );
                        if i == e.arms.len() - 1 {
                            sg.seg_split(out, ",");
                        } else {
                            sg.seg(out, ",");
                        }
                        sg.seg_unsplit(out, " ");
                    }
                    sg.split(out, base_indent.clone(), false);
                    append_comments(out, base_indent, &mut sg, e.brace_token.span.end().prev());
                    sg.seg(out, "}");
                    sg.build(out)
                },
            ),
            Expr::MethodCall(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_dotted(out, base_indent, Dotted::Method(e))
                },
            ),
            Expr::Paren(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.paren_token.span.start());
                    sg.seg(out, "(");
                    sg.child(e.expr.make_segs(out, base_indent));
                    append_comments(out, base_indent, &mut sg, e.paren_token.span.end().prev());
                    sg.seg(out, ")");
                    sg.build(out)
                },
            ),
            Expr::Path(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_extended_path(out, base_indent, &e.qself, &e.path)
                },
            ),
            Expr::Range(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let (tok, tok_loc) = match e.limits {
                        syn::RangeLimits::HalfOpen(x) => ("..", x.spans[0].start()),
                        syn::RangeLimits::Closed(x) => ("..=", x.spans[0].start()),
                    };
                    match (&e.from, &e.to) {
                        (None, None) => new_sg_lit(out, Some((base_indent, tok_loc)), tok),
                        (None, Some(r)) => {
                            let mut sg = new_sg(out);
                            append_comments(out, base_indent, &mut sg, tok_loc);
                            sg.seg(out, tok);
                            sg.child(r.as_ref().make_segs(out, base_indent));
                            sg.build(out)
                        },
                        (Some(l), None) => {
                            let mut sg = new_sg(out);
                            append_comments(out, base_indent, &mut sg, tok_loc);
                            sg.child(l.as_ref().make_segs(out, base_indent));
                            sg.seg(out, tok);
                            sg.build(out)
                        },
                        (Some(l), Some(r)) => new_sg_binary(
                            out,
                            base_indent,
                            l.as_ref(),
                            tok_loc,
                            &format!(" {}", tok),
                            r.as_ref(),
                        ),
                    }
                },
            ),
            Expr::Reference(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_ref(out, base_indent, e.and_token.span.start(), e.mutability.is_some(), e.expr.as_ref())
                },
            ),
            Expr::Repeat(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    build_array_type(out, base_indent, e.bracket_token.span.start(), e.expr.as_ref(), e.len.as_ref())
                },
            ),
            Expr::Return(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.return_token.span.start());
                    sg.seg(out, "return");
                    if let Some(e) = &e.expr {
                        sg.seg(out, " ");
                        sg.child(e.make_segs(out, base_indent));
                    }
                    sg.build(out)
                },
            ),
            Expr::Struct(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    if out
                        .split_brace_threshold
                        .map(|t| e.fields.len() + e.rest.iter().count() > t)
                        .unwrap_or(false) {
                        sg.initial_split();
                    }
                    sg.child(build_path(out, base_indent, &e.path));
                    sg.seg(out, " {");
                    sg.seg_unsplit(out, " ");
                    let indent = base_indent.indent();
                    let mut i = 0;
                    for pair in e.fields.pairs() {
                        if i > 0 {
                            sg.seg(out, ",");
                            sg.seg_unsplit(out, " ");
                        }
                        sg.split(out, indent.clone(), true);
                        let v = pair.value();
                        sg.child(
                            new_sg_outer_attrs(
                                out,
                                &indent,
                                &v.attrs,
                                |out: &mut MakeSegsState, base_indent: &Alignment| {
                                    let mut sg = new_sg(out);
                                    if let Some(col) = &v.colon_token {
                                        match &v.member {
                                            syn::Member::Named(n) => {
                                                append_comments(out, &indent, &mut sg, n.span().start());
                                                sg.seg(out, n);
                                            },
                                            syn::Member::Unnamed(i) => {
                                                append_comments(out, &indent, &mut sg, i.span.start());
                                                sg.seg(out, i.index);
                                            },
                                        };
                                        append_comments(out, &indent, &mut sg, col.span.start());
                                        sg.seg(out, ": ");
                                    }
                                    sg.child(pair.value().expr.make_segs(out, base_indent));
                                    sg.build(out)
                                },
                            ),
                        );
                        i += 1;
                    }
                    if let Some(dots) = &e.dot2_token {
                        if i > 0 {
                            sg.seg(out, ",");
                            sg.seg_unsplit(out, " ");
                        }
                        sg.split(out, indent.clone(), true);
                        append_comments(out, base_indent, &mut sg, dots.spans[0].start());
                        sg.seg(out, "..");
                        if let Some(rem) = &e.rest {
                            sg.child(rem.make_segs(out, &indent));
                        }
                        sg.seg_unsplit(out, " ");
                    } else {
                        sg.seg_split(out, ",");
                        sg.seg_unsplit(out, " ");
                    }
                    sg.split(out, base_indent.clone(), false);
                    append_comments(out, base_indent, &mut sg, e.brace_token.span.end().prev());
                    sg.seg(out, "}");
                    sg.build(out)
                },
            ),
            Expr::Try(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| match get_dotted(self) {
                    DottedRes::Dotted(d) => new_sg_dotted(out, base_indent, d),
                    DottedRes::Leaf(_) => {
                        let mut node = new_sg(out);
                        node.child(e.expr.make_segs(out, base_indent));
                        node.seg(out, "?");
                        node.build(out)
                    },
                },
            ),
            Expr::TryBlock(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.try_token.span.start());
                    sg.seg(out, "try ");
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        Some(&e.attrs),
                        &e.block.stmts,
                        e.block.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Expr::Tuple(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_comma_bracketed_list(
                        out,
                        base_indent,
                        None::<Expr>,
                        e.paren_token.span.start(),
                        "(",
                        &e.elems,
                        e.paren_token.span.end().prev(),
                        ")",
                    )
                },
            ),
            Expr::Type(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    new_sg_binary(out, base_indent, e.expr.as_ref(), e.colon_token.span.start(), ":", e.ty.as_ref())
                },
            ),
            Expr::Unary(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(
                        out,
                        base_indent,
                        &mut sg,
                        e.op.to_token_stream().into_iter().next().unwrap().span().start(),
                    );
                    sg.seg(out, &e.op.to_token_stream().to_string());
                    sg.child(e.expr.make_segs(out, base_indent));
                    sg.build(out)
                },
            ),
            Expr::Unsafe(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.unsafe_token.span.start());
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        "unsafe {",
                        Some(&e.attrs),
                        &e.block.stmts,
                        e.block.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Expr::Verbatim(e) => {
                let mut node = new_sg(out);
                node.seg(out, &e.to_string());
                node.build(out)
            },
            Expr::While(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    if let Some(l) = &e.label {
                        append_comments(out, base_indent, &mut sg, l.name.apostrophe.start());
                        sg.seg(out, format!("{}: ", l.name));
                    }
                    append_comments(out, base_indent, &mut sg, e.while_token.span.start());
                    sg.seg(out, "while ");
                    sg.child(e.cond.make_segs(out, base_indent));
                    append_bracketed_statement_list(
                        out,
                        base_indent,
                        &mut sg,
                        " {",
                        Some(&e.attrs),
                        &e.body.stmts,
                        e.body.brace_token.span.end().prev(),
                    );
                    sg.build(out)
                },
            ),
            Expr::Yield(e) => new_sg_outer_attrs(
                out,
                base_indent,
                &e.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_comments(out, base_indent, &mut sg, e.yield_token.span.start());
                    sg.seg(out, "yield");
                    if let Some(e) = &e.expr {
                        sg.seg(out, " ");
                        sg.child(e.make_segs(out, base_indent));
                    }
                    sg.build(out)
                },
            ),
            _ => unreachable!(),
        }
    }
}
