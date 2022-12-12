use anyhow::Result;
use comments::{format_md, Comment, HashLineColumn};
use proc_macro2::{Ident, LineColumn, TokenStream};
use quote::ToTokens;
use sg_general::append_comments;
use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};
use syn::File;

use crate::{comments::CommentExtractor, sg_general::new_sg_attrs};
// TODO
// line + block comments
//  block comments -> counted nested /* */, don't go by lines
//  grab before every token
//  add parts
// finish remaining elements
// wrapping (start from root, wrap any line any node seg is on)
pub(crate) mod comments;
pub(crate) mod sg_expr;
pub(crate) mod sg_general;
pub(crate) mod sg_pat;
pub(crate) mod sg_statement;
pub(crate) mod sg_type;
pub mod utils;

pub(crate) struct SplitGroup {
    pub(crate) children: Vec<Rc<RefCell<SplitGroup>>>,
    pub(crate) split: bool,
    pub(crate) segments: Vec<Rc<RefCell<Segment>>>,
}

pub(crate) enum SegmentMode {
    All,
    Unsplit,
    Split,
}

pub(crate) struct SegmentLine {
    pub(crate) line: Rc<RefCell<Line>>,
    pub(crate) seg_index: usize,
}

pub(crate) enum SegmentContent {
    Text(String),
    Comment((Alignment, Vec<Comment>)),
    Break(Alignment, bool),
}

pub(crate) struct Segment {
    pub(crate) node: Rc<RefCell<SplitGroup>>,
    pub(crate) line: Option<SegmentLine>,
    pub(crate) mode: SegmentMode,
    pub(crate) content: SegmentContent,
}

pub(crate) struct Line {
    pub(crate) lines: Rc<RefCell<Lines>>,
    pub(crate) index: usize,
    pub(crate) segs: Vec<Rc<RefCell<Segment>>>,
}

pub(crate) struct Lines {
    pub(crate) lines: Vec<Rc<RefCell<Line>>>,
}

pub(crate) struct MakeSegsState {
    pub(crate) line: Vec<Rc<RefCell<Segment>>>,
    pub(crate) comments: HashMap<HashLineColumn, Vec<Comment>>,
}

pub(crate) fn line_length(line: &RefCell<Line>) -> usize {
    let mut out = 0;
    for seg in &line.borrow().segs {
        match &seg.as_ref().borrow().content {
            SegmentContent::Text(t) => out += t.len(),
            SegmentContent::Break(b, _) => {
                if seg.as_ref().borrow().node.as_ref().borrow().split {
                    out += b.get();
                }
            }
            SegmentContent::Comment(_) => {}
        };
    }
    out
}

pub(crate) fn split_group(node: &RefCell<SplitGroup>) {
    node.borrow_mut().split = true;
    for seg in &node.borrow().segments {
        let res = {
            let seg = seg.as_ref().borrow();
            match (&seg.mode, &seg.content) {
                (SegmentMode::Split, SegmentContent::Break(_, _)) => {
                    let line = seg.borrow().line.as_ref().unwrap();
                    Some((line.line.clone(), line.seg_index))
                }
                _ => None,
            }
        };
        match res {
            Some((line, index)) => {
                split_line_at(&line, index);
            }
            None => {}
        };
    }
}

pub(crate) fn split_line_at(line: &RefCell<Line>, at: usize) {
    let new_segs = line.borrow_mut().segs.split_off(at);
    if let Some(s) = new_segs.get(0) {
        match &s.as_ref().borrow().content {
            SegmentContent::Break(a, activate) if *activate => {
                a.activate();
            }
            _ => {}
        }
    }
    insert_line(
        line.borrow().lines.clone(),
        line.borrow().index + 1,
        new_segs,
    );
}

pub(crate) fn insert_line(lines: Rc<RefCell<Lines>>, at: usize, segs: Vec<Rc<RefCell<Segment>>>) {
    let new_line = Rc::new(RefCell::new(Line {
        lines: lines.clone(),
        index: at,
        segs: segs,
    }));
    lines.borrow_mut().lines.insert(at, new_line.clone());
    for (i, seg) in new_line.as_ref().borrow().segs.iter().enumerate() {
        let mut seg = seg.as_ref().borrow_mut();
        let mut line = seg.line.as_mut().unwrap();
        line.seg_index = i;
        line.line = new_line.clone();
    }
    for (i, line) in lines
        .as_ref()
        .borrow()
        .lines
        .iter()
        .enumerate()
        .skip(at + 1)
    {
        line.borrow_mut().index = i;
    }
}

fn render_line_str(max_width: usize, line: &RefCell<Line>) -> String {
    let mut rendered = String::new();
    render_line(max_width, &mut rendered, line);
    rendered
}

fn render_line(max_width: usize, rendered: &mut String, line: &RefCell<Line>) {
    for seg in &line.borrow().segs {
        let seg = seg.as_ref().borrow();
        match (&seg.mode, seg.node.as_ref().borrow().split) {
            (SegmentMode::All, _) => {}
            (SegmentMode::Unsplit, true) => continue,
            (SegmentMode::Unsplit, false) => {}
            (SegmentMode::Split, true) => {}
            (SegmentMode::Split, false) => continue,
        };
        match &seg.content {
            SegmentContent::Text(t) => {
                rendered.push_str(&t);
            }
            SegmentContent::Break(b, activate) => {
                if *activate {
                    b.activate();
                }
                rendered.push_str(&" ".repeat(b.get()));
            }
            SegmentContent::Comment((b, comments)) => {
                for comment in comments {
                    format_md(
                        rendered,
                        max_width,
                        &format!(
                            "{}//{} ",
                            " ".repeat(b.get()),
                            match comment.mode {
                                comments::CommentMode::Normal => "",
                                comments::CommentMode::DocInner => "!",
                                comments::CommentMode::DocOuter => "/",
                            }
                        ),
                        &comment.lines,
                    );
                }
            }
        }
    }
}

pub(crate) struct Alignment_ {
    pub(crate) parent: Option<Alignment>,
    pub(crate) active: bool,
}

#[derive(Clone)]
pub(crate) struct Alignment(pub(crate) Rc<RefCell<Alignment_>>);

impl Alignment {
    pub(crate) fn indent(&self) -> Alignment {
        Alignment(Rc::new(RefCell::new(Alignment_ {
            parent: Some(self.clone()),
            active: false,
        })))
    }

    pub(crate) fn activate(&self) -> usize {
        self.0.borrow_mut().active = true;
        self.get()
    }

    pub(crate) fn get(&self) -> usize {
        let parent = match &self.0.as_ref().borrow().parent {
            Some(p) => p.get(),
            None => {
                return 0usize;
            }
        };
        if self.0.as_ref().borrow().active {
            4usize + parent
        } else {
            parent
        }
    }
}

pub(crate) struct SplitGroupBuilder {
    node: Rc<RefCell<SplitGroup>>,
    children: Vec<Rc<RefCell<SplitGroup>>>,
    segs: Vec<Rc<RefCell<Segment>>>,
}

impl SplitGroupBuilder {
    pub(crate) fn add(&mut self, out_segs: &mut MakeSegsState, seg: Rc<RefCell<Segment>>) {
        self.segs.push(seg.clone());
        out_segs.line.push(seg);
    }

    pub(crate) fn child(&mut self, child: Rc<RefCell<SplitGroup>>) {
        self.children.push(child);
    }

    pub(crate) fn build(self) -> Rc<RefCell<SplitGroup>> {
        let mut n1 = self.node.borrow_mut();
        n1.segments.extend(self.segs);
        n1.children.extend(self.children);
        drop(n1);
        return self.node;
    }

    //

    pub(crate) fn seg(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(
            out,
            Rc::new(RefCell::new(Segment {
                node: self.node.clone(),
                line: None,
                mode: SegmentMode::All,
                content: SegmentContent::Text(text.to_string()),
            })),
        );
    }

    pub(crate) fn seg_split(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(
            out,
            Rc::new(RefCell::new(Segment {
                node: self.node.clone(),
                line: None,
                mode: SegmentMode::Split,
                content: SegmentContent::Text(text.to_string()),
            })),
        );
    }

    pub(crate) fn seg_unsplit(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(
            out,
            Rc::new(RefCell::new(Segment {
                node: self.node.clone(),
                line: None,
                mode: SegmentMode::Unsplit,
                content: SegmentContent::Text(text.to_string()),
            })),
        );
    }

    pub(crate) fn split(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(
            out,
            Rc::new(RefCell::new(Segment {
                node: self.node.clone(),
                line: None,
                mode: SegmentMode::Split,
                content: SegmentContent::Break(alignment, activate),
            })),
        );
    }

    pub(crate) fn split_always(
        &mut self,
        out: &mut MakeSegsState,
        alignment: Alignment,
        activate: bool,
    ) {
        self.add(
            out,
            Rc::new(RefCell::new(Segment {
                node: self.node.clone(),
                line: None,
                mode: SegmentMode::All,
                content: SegmentContent::Break(alignment, activate),
            })),
        );
    }
}

pub(crate) fn new_sg() -> SplitGroupBuilder {
    SplitGroupBuilder {
        node: Rc::new(RefCell::new(SplitGroup {
            split: false,
            segments: vec![],
            children: vec![],
        })),
        segs: vec![],
        children: vec![],
    }
}

pub(crate) fn new_sg_lit(
    out: &mut MakeSegsState,
    start: Option<(&Alignment, LineColumn)>,
    text: impl ToString,
) -> Rc<RefCell<SplitGroup>> {
    let mut sg = new_sg();
    if let Some(loc) = start {
        append_comments(out, loc.0, &mut sg, loc.1);
    }
    sg.seg(out, text.to_string());
    sg.build()
}

pub(crate) trait Formattable {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>>;
}

impl<F: Fn(&mut MakeSegsState, &Alignment) -> Rc<RefCell<SplitGroup>>> Formattable for F {
    fn make_segs(
        &self,
        line: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        self(line, base_indent)
    }
}

impl Formattable for Ident {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        new_sg_lit(out, Some((base_indent, self.span().start())), self)
    }
}

impl Formattable for &Ident {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        (*self).make_segs(out, base_indent)
    }
}

pub fn format_str(code: &str, max_len: usize) -> Result<String> {
    format_ast(code, syn::parse_file(code)?, max_len)
}

pub fn format_ast(source: &str, ast: File, max_len: usize) -> Result<String> {
    // Extract comments
    let comments = {
        let mut comment_extractor = CommentExtractor {
            source: source,
            last_loc: LineColumn { line: 1, column: 0 },
            last_offset: 0,
            comments: HashMap::new(),
        };

        fn recurse(comment_extractor: &mut CommentExtractor, ts: TokenStream) {
            for t in ts {
                match t {
                    proc_macro2::TokenTree::Group(g) => {
                        comment_extractor.extract_to(g.span_open().start());
                        comment_extractor.advance_to(g.span_open().end());
                        recurse(comment_extractor, g.stream());
                        comment_extractor.extract_to(g.span_close().start());
                        comment_extractor.advance_to(g.span_close().end());
                    }
                    proc_macro2::TokenTree::Ident(g) => {
                        comment_extractor.extract_to(g.span().start());
                        comment_extractor.advance_to(g.span().end());
                    }
                    proc_macro2::TokenTree::Punct(g) => {
                        comment_extractor.extract_to(g.span().start());
                        comment_extractor.advance_to(g.span().end());
                    }
                    proc_macro2::TokenTree::Literal(g) => {
                        comment_extractor.extract_to(g.span().start());
                        comment_extractor.advance_to(g.span().end());
                    }
                }
            }
        }
        recurse(&mut comment_extractor, ast.to_token_stream());

        comment_extractor.comments
    };

    // Build text
    let mut out = MakeSegsState {
        line: vec![],
        comments: comments,
    };
    let root = new_sg_attrs(
        &mut out,
        &Alignment(Rc::new(RefCell::new(Alignment_ {
            parent: None,
            active: false,
        }))),
        &ast.attrs,
        |out: &mut MakeSegsState, base_indent: &Alignment| {
            let mut node = new_sg();
            for i in &ast.items {
                node.split(out, base_indent.clone(), false);
                node.child(i.make_segs(out, base_indent));
            }
            node.build()
        },
    );
    let lines = Rc::new(RefCell::new(Lines { lines: vec![] }));
    let line = Rc::new(RefCell::new(Line {
        lines: lines.clone(),
        index: 0,
        segs: out.line,
    }));
    lines.borrow_mut().lines.push(line.clone());
    for line in &lines.as_ref().borrow().lines {
        for seg in &line.as_ref().borrow().segs {
            seg.as_ref().borrow_mut().line = Some(SegmentLine {
                line: line.clone(),
                seg_index: 0,
            });
        }
    }

    // Do initial splits
    {
        let mut i = 0usize;
        let mut skip_first = false;
        while i < lines.as_ref().borrow().lines.len() {
            let mut res = None;
            {
                let lines = lines.as_ref().borrow();
                let line = lines.lines.get(i).unwrap();
                'segs: for (i, seg) in line.as_ref().borrow().segs.iter().enumerate() {
                    if i == 0 && skip_first {
                        skip_first = false;
                        continue;
                    }
                    let seg = seg.as_ref().borrow();
                    match (&seg.content, &seg.mode) {
                        (SegmentContent::Break(_, _), SegmentMode::All)
                        | (SegmentContent::Comment(_), _) => {
                            res = Some((line.clone(), i));
                            break 'segs;
                        }
                        _ => {}
                    };
                }
            }
            if let Some((line, i)) = res {
                split_line_at(&line, i);
                skip_first = true;
            }
            i += 1;
        }
    }

    // Do width based splitting
    fn recurse(max_len: usize, node: &RefCell<SplitGroup>) {
        let mut over = false;
        for seg in &node.borrow().segments {
            let seg = seg.as_ref().borrow();
            let line = seg.line.as_ref().unwrap();
            let len = line_length(&line.line.as_ref());
            if len > max_len {
                over = true;
                break;
            }
        }
        if over {
            split_group(node);
        }
        for child in &node.borrow().children {
            recurse(max_len, child.as_ref());
        }
    }
    recurse(max_len, root.as_ref());

    // Render
    let mut rendered = String::new();
    if let Some(shebang) = ast.shebang {
        rendered.push_str(&shebang);
        rendered.push_str("\n");
    }
    for line in &lines.as_ref().borrow().lines {
        println!("{}", render_line_str(max_len, line));
        //render_line(&mut rendered, line);
        //rendered.push_str("\n").unwrap();
    }
    for (_, comm) in out.comments {
        for c in comm {
            println!("Unconsumed comment:\n{}", c.lines);
        }
    }
    Ok(rendered)
}
