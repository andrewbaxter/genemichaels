use anyhow::Result;
use comments::{format_md, HashLineColumn};
use proc_macro2::{Ident, LineColumn, TokenStream};
use quote::ToTokens;
use sg_general::append_comments;
use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};
use syn::File;
use crate::{comments::CommentExtractor};

pub(crate) mod comments;
pub(crate) mod sg_expr;
pub(crate) mod sg_general;
pub(crate) mod sg_pat;
pub(crate) mod sg_statement;
pub(crate) mod sg_type;
pub(crate) mod sg_root;
pub mod utils;

pub(crate) trait TrivialLineColMath { 
    // syn doesn't provide end token spans often, in which case the start span covers everything.  This is a dumb method to 
    // take the end of that and move back one char to hopefully get the start of the end token.
    fn prev(&self) -> LineColumn; }

impl TrivialLineColMath for LineColumn { fn prev(&self) -> LineColumn {
    let mut out = self.clone();
    out.column -= 1;
    out
} }

#[derive(PartialEq, Clone, Copy, Debug)] pub enum CommentMode {Normal, DocInner, DocOuter}

#[derive(Debug)] pub struct Comment {pub(crate) mode: CommentMode, pub(crate) lines: String}

pub struct SplitGroup {
    pub(crate) children: Vec<Rc<RefCell<SplitGroup>>>,
    pub(crate) split: bool,
    pub(crate) segments: Vec<Rc<RefCell<Segment>>>,
}

pub(crate) enum SegmentMode {All, Unsplit, Split}

pub(crate) struct SegmentLine {pub(crate) line: Rc<RefCell<Line>>, pub(crate) seg_index: usize}

pub(crate) enum SegmentContent {Text(String), Comment((Alignment, Vec<Comment>)), Break(Alignment, bool)}

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

pub(crate) struct Lines {pub(crate) lines: Vec<Rc<RefCell<Line>>>}

pub struct MakeSegsState {
    pub(crate) line: Vec<Rc<RefCell<Segment>>>,
    pub(crate) comments: HashMap<HashLineColumn, Vec<Comment>>,
}

pub(crate) fn line_length(line: &RefCell<Line>) -> usize {
    let mut out = 0;
    for seg in &line.borrow().segs {
        match &seg.as_ref().borrow().content {
            SegmentContent::Text(t) => out += t.len(),
            SegmentContent::Break(b, _) => {
                if seg.as_ref().borrow().node.as_ref().borrow().split { out += b.get(); }
            },
            SegmentContent::Comment(_) => { },
        };
    }
    out
}

pub(crate) fn split_group(node: &RefCell<SplitGroup>) {
    node.borrow_mut().split = true;
    for seg in &node.borrow().segments {
        let res = {
            let seg = seg.as_ref().borrow();
            match (&seg.mode, &seg.content) { (SegmentMode::Split, SegmentContent::Break(_, _)) => {
                let line = seg.borrow().line.as_ref().unwrap();
                Some((line.line.clone(), line.seg_index))
            }, _ => None }
        };
        match res { Some((line, index)) => { split_line_at(&line, index); }, None => { } };
    }
}

pub(crate) fn split_line_at(line: &RefCell<Line>, at: usize) {
    let new_segs = line.borrow_mut().segs.split_off(at);
    if let Some(s) = new_segs.get(0) {
        match &s.as_ref().borrow().content {
            SegmentContent::Break(a, activate) if *activate => { a.activate(); },
            _ => { },
        }
    }
    insert_line(line.borrow().lines.clone(), line.borrow().index + 1, new_segs);
}

pub(crate) fn insert_line(lines: Rc<RefCell<Lines>>, at: usize, segs: Vec<Rc<RefCell<Segment>>>) {
    let new_line = Rc::new(RefCell::new(Line {lines: lines.clone(), index: at, segs: segs}));
    lines.borrow_mut().lines.insert(at, new_line.clone());
    for (i, seg) in new_line.as_ref().borrow().segs.iter().enumerate() {
        let mut seg = seg.as_ref().borrow_mut();
        let mut line = seg.line.as_mut().unwrap();
        line.seg_index = i;
        line.line = new_line.clone();
    }
    for (i, line) in lines.as_ref().borrow().lines.iter().enumerate().skip(at + 1) { line.borrow_mut().index = i; }
}

pub(crate) struct Alignment_ {pub(crate) parent: Option<Alignment>, pub(crate) active: bool}

#[derive(Clone)] pub struct Alignment(Rc<RefCell<Alignment_>>);

impl Alignment {
    pub(crate) fn indent(&self) -> Alignment {
        Alignment(Rc::new(RefCell::new(Alignment_ {parent: Some(self.clone()), active: false})))
    }

    pub(crate) fn activate(&self) -> usize {
        self.0.borrow_mut().active = true;
        self.get()
    }

    pub(crate) fn get(&self) -> usize {
        let parent = match &self.0.as_ref().borrow().parent { Some(p) => p.get(), None => { return 0usize; } };
        if self.0.as_ref().borrow().active { 4usize + parent } else { parent }
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

    pub(crate) fn child(&mut self, child: Rc<RefCell<SplitGroup>>) { self.children.push(child); }

    pub(crate) fn build(self) -> Rc<RefCell<SplitGroup>> {
        let mut n1 = self.node.borrow_mut();
        n1.segments.extend(self.segs);
        n1.children.extend(self.children);
        drop(n1);
        return self.node;
    }

    pub(crate) fn seg(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(
            out,
            Rc::new(
                RefCell::new(
                    Segment {
                        node: self.node.clone(),
                        line: None,
                        mode: SegmentMode::All,
                        content: SegmentContent::Text(text.to_string()),
                    },
                ),
            ),
        );
    }

    pub(crate) fn seg_split(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(
            out,
            Rc::new(
                RefCell::new(
                    Segment {
                        node: self.node.clone(),
                        line: None,
                        mode: SegmentMode::Split,
                        content: SegmentContent::Text(text.to_string()),
                    },
                ),
            ),
        );
    }

    pub(crate) fn seg_unsplit(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(
            out,
            Rc::new(
                RefCell::new(
                    Segment {
                        node: self.node.clone(),
                        line: None,
                        mode: SegmentMode::Unsplit,
                        content: SegmentContent::Text(text.to_string()),
                    },
                ),
            ),
        );
    }

    pub(crate) fn split(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(
            out,
            Rc::new(
                RefCell::new(
                    Segment {
                        node: self.node.clone(),
                        line: None,
                        mode: SegmentMode::Split,
                        content: SegmentContent::Break(alignment, activate),
                    },
                ),
            ),
        );
    }

    pub(crate) fn split_always(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(
            out,
            Rc::new(
                RefCell::new(
                    Segment {
                        node: self.node.clone(),
                        line: None,
                        mode: SegmentMode::All,
                        content: SegmentContent::Break(alignment, activate),
                    },
                ),
            ),
        );
    }
}

pub(crate) fn new_sg() -> SplitGroupBuilder {
    SplitGroupBuilder {
        node: Rc::new(RefCell::new(SplitGroup {split: false, segments: vec![], children: vec![]})),
        segs: vec![],
        children: vec![],
    }
}

pub(crate) fn new_sg_lit(
    out: &mut MakeSegsState,
    start: Option<(&Alignment, LineColumn)>,
    text: impl ToString,
) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut sg = new_sg();
    if let Some(loc) = start { append_comments(out, loc.0, &mut sg, loc.1); }
    sg.seg(out, text.to_string());
    sg.build()
}

#[derive(PartialEq)] pub(crate) enum MarginGroup {BlockDef, Import, None}

pub(crate) trait FormattableStmt: ToTokens + Formattable { fn want_margin(&self) -> (MarginGroup, bool); }

pub trait Formattable {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>>;
}

impl<F: Fn(&mut MakeSegsState, &Alignment) -> Rc<RefCell<SplitGroup>>> Formattable for F {
    fn make_segs(&self, line: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        self(line, base_indent)
    }
}

impl Formattable for Ident {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        new_sg_lit(out, Some((base_indent, self.span().start())), self)
    }
}

impl Formattable for &Ident {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        (*self).make_segs(out, base_indent)
    }
}

pub struct FormatConfig {
    #[doc= " Try to wrap to this width"] pub max_width: usize,
    #[doc= " If a node is split, all parents of the node must also be split"] pub root_splits: bool,
    #[doc= " If a node has comments, split it"] pub split_comments: bool,
}

pub struct FormatRes {pub rendered: String, pub lost_comments: HashMap<HashLineColumn, Vec<Comment>>}

pub fn format_str(source: &str, config: &FormatConfig) -> Result<FormatRes> {
    let ast: File = syn::parse_file(source)?;

    // Extract comments
    let mut comment_extractor =
        CommentExtractor {
            source: source,
            last_loc: LineColumn {line: 1, column: 0},
            last_offset: 0,
            comments: HashMap::new(),
        };

    fn recurse(comment_extractor: &mut CommentExtractor, ts: TokenStream) {
        for t in ts { match t { proc_macro2::TokenTree::Group(g) => {
            comment_extractor.extract_to(g.span_open().start());
            comment_extractor.advance_to(g.span_open().end());
            recurse(comment_extractor, g.stream());
            comment_extractor.extract_to(g.span_close().start());
            comment_extractor.advance_to(g.span_close().end());
        }, proc_macro2::TokenTree::Ident(g) => {
            comment_extractor.extract_to(g.span().start());
            comment_extractor.advance_to(g.span().end());
        }, proc_macro2::TokenTree::Punct(g) => {
            comment_extractor.extract_to(g.span().start());
            comment_extractor.advance_to(g.span().end());
        }, proc_macro2::TokenTree::Literal(g) => {
            comment_extractor.extract_to(g.span().start());
            comment_extractor.advance_to(g.span().end());
        } } }
    }

    recurse(&mut comment_extractor, ast.to_token_stream());
    format_ast(ast, config, comment_extractor.comments)
}

pub fn format_ast(
    ast: impl Formattable,
    config: &FormatConfig,
    comments: HashMap<HashLineColumn, Vec<Comment>>,
) -> Result<
    FormatRes,
> {
    // Build text
    let mut out = MakeSegsState {line: vec![], comments: comments};
    let base_indent = Alignment(Rc::new(RefCell::new(Alignment_ {parent: None, active: false})));
    let root = ast.make_segs(&mut out, &base_indent);
    let lines = Rc::new(RefCell::new(Lines {lines: vec![]}));
    let line = Rc::new(RefCell::new(Line {lines: lines.clone(), index: 0, segs: out.line}));
    lines.borrow_mut().lines.push(line.clone());
    for line in &lines.as_ref().borrow().lines {
        for seg in &line.as_ref().borrow().segs {
            seg.as_ref().borrow_mut().line = Some(SegmentLine {line: line.clone(), seg_index: 0});
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
                'segs : for (i, seg) in line.as_ref().borrow().segs.iter().enumerate() {
                    if i == 0 && skip_first {
                        skip_first = false;
                        continue;
                    }
                    let seg = seg.as_ref().borrow();
                    match (&seg.content, &seg.mode) {
                        (SegmentContent::Break(_, _), SegmentMode::All) | (SegmentContent::Comment(_), _) => {
                            res = Some((line.clone(), i));
                            break 'segs;
                        },
                        _ => { },
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

    // Do width based splitting, other splitting
    fn recurse(config: &FormatConfig, node: &RefCell<SplitGroup>) -> bool {
        let mut split = false;
        for seg in &node.borrow().segments {
            let seg = seg.as_ref().borrow();
            match &seg.content { SegmentContent::Comment(_) if config.split_comments => {
                split = true;
                break;
            }, _ => { } };
            let line = seg.line.as_ref().unwrap();
            let len = line_length(&line.line.as_ref());
            if len > config.max_width {
                split = true;
                break;
            }
        }
        if split { split_group(node); }
        let mut split_from_child = false;
        for child in &node.borrow().children {
            let new_split_from_child = recurse(config, child.as_ref());
            split_from_child = split_from_child || new_split_from_child;
        }
        if !split && split_from_child { split_group(node); }
        config.root_splits && (split || split_from_child)
    }

    recurse(config, root.as_ref());

    // Render
    let mut rendered = String::new();
    let lines = lines.as_ref().borrow();
    for (line_i, line) in lines.lines.iter().enumerate() {
        let line = line.as_ref().borrow();

        // first line always empty due to first break
        if line.segs.is_empty() { continue; }

        // since comments are always new lines we end up with duped newlines sometimes if there's a (break), (comment) on consec 
        // lines skip the break
        if line.segs.len() == 1 &&
            lines
                .lines
                .get(line_i + 1)
                .and_then(|l| l.as_ref().borrow().segs.iter().next().map(|l| l.clone()))
                .map(|l| match &l.as_ref().borrow().content { SegmentContent::Comment(_) => true, _ => false })
                .unwrap_or(false) {
            continue;
        }
        for seg in &line.segs {
            let seg = seg.as_ref().borrow();
            match (&seg.mode, seg.node.as_ref().borrow().split) {
                (SegmentMode::All, _) => { },
                (SegmentMode::Unsplit, true) => continue,
                (SegmentMode::Unsplit, false) => { },
                (SegmentMode::Split, true) => { },
                (SegmentMode::Split, false) => continue,
            };
            match &seg.content {
                SegmentContent::Text(t) => { rendered.push_str(&t); },
                SegmentContent::Break(b, activate) => {
                    if *activate { b.activate(); }
                    if line.segs.len() > 1 { 
                        // if empty line (=just break), don't write indent
                        rendered.push_str(&" ".repeat(b.get())); }
                },
                SegmentContent::Comment((b, comments)) => {
                    for comment in comments {
                        format_md(
                            &mut rendered,
                            config.max_width,
                            &format!("{}//{} ",
                                " ".repeat(b.get()),
                                match comment.mode {
                                    CommentMode::Normal => "",
                                    CommentMode::DocInner => "!",
                                    CommentMode::DocOuter => "/",
                                },
                            ),
                            &comment.lines,
                        );
                    }
                },
            }
        }
        rendered.push_str("\n");
    }
    Ok(FormatRes {rendered: rendered, lost_comments: out.comments})
}
