use anyhow::{
    Result,
};
use comments::{
    format_md,
    HashLineColumn,
};
use proc_macro2::{
    Ident,
    LineColumn,
};
use quote::ToTokens;
use sg_general::append_comments;
use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};
use syn::File;

pub(crate) mod comments;
pub(crate) mod sg_expr;
pub(crate) mod sg_general;
pub(crate) mod sg_pat;
pub(crate) mod sg_statement;
pub(crate) mod sg_type;
pub(crate) mod sg_root;
pub mod utils;

pub(crate) trait TrivialLineColMath {
    // syn doesn't provide end token spans often, in which case the start span covers
    // everything.  This is a dumb method to take the end of that and move back one char to
    // hopefully get the start of the end token.
    fn prev(&self) -> LineColumn;
}

impl TrivialLineColMath for LineColumn {
    fn prev(&self) -> LineColumn {
        let mut out = self.clone();
        out.column -= 1;
        out
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum CommentMode {
    Normal,
    DocInner,
    DocOuter,
    Verbatim,
}

#[derive(Debug)]
pub struct Comment {
    pub(crate) loc: LineColumn,
    pub(crate) mode: CommentMode,
    pub(crate) lines: String,
}

pub struct SplitGroup {
    pub(crate) children: Vec<Rc<RefCell<SplitGroup>>>,
    pub(crate) split: bool,
    pub(crate) segments: Vec<Rc<RefCell<Segment>>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum SegmentMode {
    All,
    Unsplit,
    Split,
}

pub(crate) struct SegmentLine {
    pub(crate) line: Rc<RefCell<Line>>,
    pub(crate) seg_index: usize,
}

#[derive(Debug)]
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

impl std::fmt::Debug for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.mode, &self.content, self.node.as_ref().borrow().split).fmt(f)
    }
}

pub(crate) struct Line {
    pub(crate) lines: Rc<RefCell<Lines>>,
    pub(crate) index: usize,
    pub(crate) segs: Vec<Rc<RefCell<Segment>>>,
}

pub(crate) struct Lines {
    pub(crate) lines: Vec<Rc<RefCell<Line>>>,
}

pub struct MakeSegsState {
    pub(crate) line: Vec<Rc<RefCell<Segment>>>,
    pub(crate) comments: HashMap<HashLineColumn, Vec<Comment>>,
    pub(crate) split_brace_threshold: Option<usize>,
    split_attributes: bool,
}

pub(crate) fn check_split_brace_threshold(out: &MakeSegsState, count: usize) -> bool {
    return out.split_brace_threshold.map(|t| count >= t).unwrap_or(false)
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
            match (&seg.mode, &seg.content) {
                (SegmentMode::Split, SegmentContent::Break(_, _)) => {
                    let line = seg.borrow().line.as_ref().unwrap();
                    Some((line.line.clone(), line.seg_index))
                },
                _ => None,
            }
        };
        match res {
            Some((line, index)) => {
                split_line_at(&line, index, None);
            },
            None => { },
        };
    }
}

pub(crate) fn split_line_at(line: &RefCell<Line>, at: usize, inject_start: Option<Rc<RefCell<Segment>>>) {
    let mut new_segs = vec![];
    if let Some(s) = inject_start {
        new_segs.push(s);
    }
    new_segs.extend(line.borrow_mut().segs.split_off(at));
    {
        let s = new_segs.get(0).unwrap();
        match &s.as_ref().borrow().content {
            SegmentContent::Break(a, activate) => {
                if *activate {
                    a.activate();
                }
            },
            SegmentContent::Comment((a, _)) => {
                a.activate();
            },
            _ => { },
        };
    }
    insert_line(line.borrow().lines.clone(), line.borrow().index + 1, new_segs);
}

pub(crate) fn insert_line(lines: Rc<RefCell<Lines>>, at: usize, segs: Vec<Rc<RefCell<Segment>>>) {
    let new_line = Rc::new(RefCell::new(Line{
        lines: lines.clone(),
        index: at,
        segs: segs,
    }));
    lines.borrow_mut().lines.insert(at, new_line.clone());
    for (i, seg) in new_line.as_ref().borrow().segs.iter().enumerate() {
        let mut seg = seg.as_ref().borrow_mut();
        match seg.line.as_mut() {
            Some(l) => {
                l.seg_index = i;
                l.line = new_line.clone();
            },
            None => {
                seg.line = Some(SegmentLine{
                    line: new_line.clone(),
                    seg_index: i,
                });
            },
        };
    }
    for (i, line) in lines.as_ref().borrow().lines.iter().enumerate().skip(at + 1) {
        line.borrow_mut().index = i;
    }
    for (i, line) in lines.as_ref().borrow().lines.iter().enumerate() {
        assert_eq!(line.as_ref().borrow().index, i, "line index wrong; after insert at line {}", at);
        for (j, seg) in line.as_ref().borrow().segs.iter().enumerate() {
            assert_eq!(
                seg.as_ref().borrow().line.as_ref().unwrap().seg_index,
                j,
                "seg index wrong; on line {}, after insert at line {}",
                i,
                at
            );
        }
    }
}

pub(crate) struct Alignment_ {
    pub(crate) parent: Option<Alignment>,
    pub(crate) active: bool,
}

#[derive(Clone)]
pub struct Alignment(Rc<RefCell<Alignment_>>);

impl std::fmt::Debug for Alignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "(align)".fmt(f)
    }
}

impl Alignment {
    pub(crate) fn indent(&self) -> Alignment {
        Alignment(Rc::new(RefCell::new(Alignment_{
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
            },
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

    pub(crate) fn initial_split(&mut self) {
        self.node.as_ref().borrow_mut().split = true;
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

    pub(crate) fn seg(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Rc::new(RefCell::new(Segment{
            node: self.node.clone(),
            line: None,
            mode: SegmentMode::All,
            content: SegmentContent::Text(text.to_string()),
        })));
    }

    pub(crate) fn seg_split(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Rc::new(RefCell::new(Segment{
            node: self.node.clone(),
            line: None,
            mode: SegmentMode::Split,
            content: SegmentContent::Text(text.to_string()),
        })));
    }

    pub(crate) fn seg_unsplit(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Rc::new(RefCell::new(Segment{
            node: self.node.clone(),
            line: None,
            mode: SegmentMode::Unsplit,
            content: SegmentContent::Text(text.to_string()),
        })));
    }

    pub(crate) fn split_if(&mut self, out: &mut MakeSegsState, alignment: Alignment, always: bool, activate: bool) {
        self.add(out, Rc::new(RefCell::new(Segment{
            node: self.node.clone(),
            line: None,
            mode: if always {
                SegmentMode::All
            } else {
                SegmentMode::Split
            },
            content: SegmentContent::Break(alignment, activate),
        })));
    }

    pub(crate) fn split(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(out, Rc::new(RefCell::new(Segment{
            node: self.node.clone(),
            line: None,
            mode: SegmentMode::Split,
            content: SegmentContent::Break(alignment, activate),
        })));
    }

    pub(crate) fn split_always(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(out, Rc::new(RefCell::new(Segment{
            node: self.node.clone(),
            line: None,
            mode: SegmentMode::All,
            content: SegmentContent::Break(alignment, activate),
        })));
    }
}

pub(crate) fn new_sg() -> SplitGroupBuilder {
    SplitGroupBuilder{
        node: Rc::new(RefCell::new(SplitGroup{
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
) -> Rc<
    RefCell<SplitGroup>,
> {
    let mut sg = new_sg();
    if let Some(loc) = start {
        append_comments(out, loc.0, &mut sg, loc.1);
    }
    sg.seg(out, text.to_string());
    sg.build()
}

#[derive(PartialEq)]
pub(crate) enum MarginGroup {
    Attr,
    BlockDef,
    Import,
    None,
}

pub(crate) trait FormattableStmt: ToTokens + Formattable {
    fn want_margin(&self) -> (MarginGroup, bool);
}

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
    /// Try to wrap to this width
    pub max_width: usize,
    /// If a node is split, all parents of the node must also be split
    pub root_splits: bool,
    pub split_brace_threshold: Option<usize>,
    pub split_attributes: bool,
    pub comment_width: Option<usize>,
    pub comment_errors_fatal: bool,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self{
            max_width: 120,
            root_splits: false,
            split_brace_threshold: Some(0usize),
            split_attributes: true,
            comment_width: Some(80usize),
            comment_errors_fatal: false,
        }
    }
}

pub struct FormatRes {
    pub rendered: String,
    pub lost_comments: HashMap<HashLineColumn, Vec<Comment>>,
}

pub use comments::extract_comments;

pub fn format_str(source: &str, config: &FormatConfig) -> Result<FormatRes> {
    let (comments, tokens) = extract_comments(source)?;
    format_ast(syn::parse2::<File>(tokens)?, config, comments)
}

pub fn format_ast(
    ast: impl Formattable,
    config: &FormatConfig,
    comments: HashMap<HashLineColumn, Vec<Comment>>,
) -> Result<
    FormatRes,
> {
    // Build text
    let mut out = MakeSegsState{
        line: vec![],
        comments: comments,
        split_brace_threshold: config.split_brace_threshold,
        split_attributes: config.split_attributes,
    };
    let base_indent = Alignment(Rc::new(RefCell::new(Alignment_{
        parent: None,
        active: false,
    })));
    let root = ast.make_segs(&mut out, &base_indent);
    let lines = Rc::new(RefCell::new(Lines{ lines: vec![] }));
    let line = Rc::new(RefCell::new(Line{
        lines: lines.clone(),
        index: 0,
        segs: out.line,
    }));
    lines.borrow_mut().lines.push(line.clone());
    for line in &lines.as_ref().borrow().lines {
        for (j, seg) in line.as_ref().borrow().segs.iter().enumerate() {
            seg.as_ref().borrow_mut().line = Some(SegmentLine{
                line: line.clone(),
                seg_index: j,
            });
        }
    }
    for (i, line) in lines.as_ref().borrow().lines.iter().enumerate() {
        assert_eq!(line.as_ref().borrow().index, i, "line index wrong; initial");
        for (j, seg) in line.as_ref().borrow().segs.iter().enumerate() {
            assert_eq!(
                seg.as_ref().borrow().line.as_ref().unwrap().seg_index,
                j,
                "seg index wrong; on line {}, initial",
                i
            );
        }
    }

    // Do initial splits
    //
    // * initially split nodes
    //
    // * always split break segments
    //
    // * comments segments
    {
        let synth_seg_node = new_sg().build();
        let mut i = 0usize;
        let mut skip_first = false;
        let mut prev_comment = None;
        while i < lines.as_ref().borrow().lines.len() {
            let mut res = None;
            {
                let lines = lines.as_ref().borrow();
                let line = lines.lines.get(i).unwrap();
                'segs : loop {
                    for (i, seg) in line.as_ref().borrow().segs.iter().enumerate() {
                        if i == 0 && skip_first {
                            skip_first = false;
                            continue;
                        }
                        let seg = seg.as_ref().borrow();
                        let node = seg.node.as_ref().borrow();
                        match (&seg.content, match (&seg.mode, node.split) {
                            (SegmentMode::All, true) => true,
                            (SegmentMode::All, false) => true,
                            (SegmentMode::Unsplit, true) => false,
                            (SegmentMode::Unsplit, false) => true,
                            (SegmentMode::Split, true) => true,
                            (SegmentMode::Split, false) => false,
                        }) {
                            (SegmentContent::Break(_, _), true) => {
                                res = Some((line.clone(), i, None));
                                prev_comment = None;
                                break 'segs;
                            },
                            (SegmentContent::Comment(c), _) => {
                                res = Some((line.clone(), i, None));
                                prev_comment = Some(c.0.clone());
                                break 'segs;
                            },
                            (_, _) => {
                                if let Some(a) = prev_comment.take() {
                                    res = Some((line.clone(), i, Some(Rc::new(RefCell::new(Segment{
                                        node: synth_seg_node.clone(),
                                        line: None,
                                        mode: SegmentMode::All,
                                        content: SegmentContent::Break(a, true),
                                    })))));
                                    break 'segs;
                                }
                            },
                        };
                    }
                    prev_comment = None;
                    break;
                }
            }
            if let Some((line, at, insert_start)) = res {
                split_line_at(&line, at, insert_start);
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
            let line = seg.line.as_ref().unwrap();
            let len = line_length(&line.line.as_ref());
            if len > config.max_width {
                split = true;
                break;
            }
        }
        if split {
            split_group(node);
        }
        let mut split_from_child = false;
        for child in &node.borrow().children {
            let new_split_from_child = recurse(config, child.as_ref());
            split_from_child = split_from_child || new_split_from_child;
        }
        if !split && split_from_child {
            split_group(node);
        }
        config.root_splits && (split || split_from_child)
    }

    recurse(config, root.as_ref());

    // Render
    let mut rendered = String::new();
    let lines = lines.as_ref().borrow();
    'lineloop: for (line_i, line) in lines.lines.iter().enumerate() {
        let line = line.as_ref().borrow();
        let segs = line.segs.iter().filter_map(|seg| {
            if {
                let seg = seg.as_ref().borrow();
                let node = seg.node.as_ref().borrow();
                match (&seg.mode, node.split) {
                    (SegmentMode::All, _) => true,
                    (SegmentMode::Unsplit, true) => false,
                    (SegmentMode::Unsplit, false) => true,
                    (SegmentMode::Split, true) => true,
                    (SegmentMode::Split, false) => false,
                }
            } {
                Some(seg.clone())
            } else {
                None
            }
        }).collect::<Vec<Rc<RefCell<Segment>>>>();
        if line.segs.is_empty() {
            continue;
        }
        for seg in &segs {
            let seg = seg.as_ref().borrow();
            match &seg.content {
                SegmentContent::Text(t) => {
                    rendered.push_str(&t);
                },
                SegmentContent::Break(b, activate) => {
                    // since comments are always new lines we end up with duped newlines sometimes if there's a (break),
                    // (comment) on consec lines. skip the break
                    if segs.len() == 1 &&
                        lines
                            .lines
                            .get(line_i + 1)
                            .and_then(|l| l.as_ref().borrow().segs.iter().next().map(|l| l.clone()))
                            .map(|l| match &l.as_ref().borrow().content {
                                SegmentContent::Comment(_) => true,
                                _ => false,
                            })
                            .unwrap_or(false) {
                        continue 'lineloop;
                    }
                    if *activate {
                        b.activate();
                    }
                    if segs.len() > 1 {
                        // if empty line (=just break), don't write indent
                        rendered.push_str(&" ".repeat(b.get()));
                    }
                },
                SegmentContent::Comment((b, comments)) => {
                    for (i, comment) in comments.iter().enumerate() {
                        if i > 0 {
                            rendered.push_str("\n");
                        }
                        let prefix = format!("{}//{} ", " ".repeat(b.get()), match comment.mode {
                            CommentMode::Normal => "",
                            CommentMode::DocInner => "!",
                            CommentMode::DocOuter => "/",
                            CommentMode::Verbatim => ".",
                        });
                        let verbatim = match comment.mode {
                            CommentMode::Verbatim => {
                                true
                            },
                            _ => {
                                match format_md(
                                    &mut rendered,
                                    config.max_width,
                                    config.comment_width,
                                    &prefix,
                                    &comment.lines,
                                ) {
                                    Err(e) => {
                                        let message =
                                            format!(
                                                "Error formatting comments before {}:{}: \n{}",
                                                comment.loc.line,
                                                comment.loc.column,
                                                comment.lines
                                            );
                                        if config.comment_errors_fatal {
                                            return Err(e.context(message));
                                        } else {
                                            eprintln!("{:?}", e.context(message));
                                        }
                                        true
                                    },
                                    Ok(_) => {
                                        false
                                    },
                                }
                            },
                        };
                        if verbatim {
                            for line in comment.lines.lines() {
                                rendered.push_str(format!("{}[{}]", prefix, line).trim());
                                rendered.push_str("\n");
                            }
                        }
                    }
                },
            }
        }
        rendered.push_str("\n");
    }
    Ok(FormatRes{
        rendered: rendered,
        lost_comments: out.comments,
    })
}
