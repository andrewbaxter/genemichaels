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

#[derive(Clone, Copy)]
pub struct SplitGroupIdx(usize);

#[derive(Clone, Copy)]
pub struct SegmentIdx(usize);

#[derive(Clone, Copy)]
pub(crate) struct LineIdx(usize);

pub struct SplitGroup {
    pub(crate) children: Vec<SplitGroupIdx>,
    pub(crate) split: bool,
    pub(crate) segments: Vec<SegmentIdx>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum SegmentMode {
    All,
    Unsplit,
    Split,
}

pub(crate) struct SegmentLine {
    pub(crate) line: LineIdx,
    pub(crate) seg_index: usize,
}

#[derive(Debug)]
pub(crate) enum SegmentContent {
    Text(String),
    Comment((Alignment, Vec<Comment>)),
    Break(Alignment, bool),
}

pub(crate) struct Segment {
    pub(crate) node: SplitGroupIdx,
    pub(crate) line: Option<SegmentLine>,
    pub(crate) mode: SegmentMode,
    pub(crate) content: SegmentContent,
}

pub(crate) struct Line {
    index: usize,
    segs: Vec<SegmentIdx>,
}

struct Lines {
    owned_lines: Vec<Line>,
    lines: Vec<LineIdx>,
}

pub struct MakeSegsState {
    nodes: Vec<SplitGroup>,
    segs: Vec<Segment>,
    comments: HashMap<HashLineColumn, Vec<Comment>>,
    split_brace_threshold: Option<usize>,
    split_attributes: bool,
}

pub(crate) fn check_split_brace_threshold(out: &MakeSegsState, count: usize) -> bool {
    return out.split_brace_threshold.map(|t| count >= t).unwrap_or(false)
}

pub(crate) fn line_length(out: &MakeSegsState, lines: &Lines, line_i: LineIdx) -> usize {
    let mut len = 0;
    for seg_i in &lines.owned_lines.get(line_i.0).unwrap().segs {
        let seg = out.segs.get(seg_i.0).unwrap();
        match &seg.content {
            SegmentContent::Text(t) => len += t.len(),
            SegmentContent::Break(b, _) => {
                if out.nodes.get(seg.node.0).unwrap().split {
                    len += b.get();
                }
            },
            SegmentContent::Comment(_) => { },
        };
    }
    len
}

pub(crate) fn split_group(out: &mut MakeSegsState, lines: &mut Lines, sg_i: SplitGroupIdx) {
    let mut sg = out.nodes.get_mut(sg_i.0).unwrap();
    sg.split = true;
    for seg_i in &sg.segments.clone() {
        let res = {
            let seg = out.segs.get(seg_i.0).unwrap();
            match (&seg.mode, &seg.content) {
                (SegmentMode::Split, SegmentContent::Break(_, _)) => {
                    let seg_line = seg.line.as_ref().unwrap();
                    Some((seg_line.line, seg_line.seg_index))
                },
                _ => None,
            }
        };
        match res {
            Some((line_i, off)) => {
                split_line_at(out, lines, line_i, off, None);
            },
            None => { },
        };
    }
}

pub(crate) fn split_line_at(
    out: &mut MakeSegsState,
    lines: &mut Lines,
    line_idx: LineIdx,
    off: usize,
    inject_start: Option<SegmentIdx>,
) {
    let line = lines.owned_lines.get_mut(line_idx.0).unwrap();
    let mut new_segs = vec![];
    if let Some(s) = inject_start {
        new_segs.push(s);
    }
    new_segs.extend(line.segs.split_off(off));
    {
        let seg_i = new_segs.get(0).unwrap();
        let seg = out.segs.get(seg_i.0).unwrap();
        match &seg.content {
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
    let insert_at = line.index + 1;
    insert_line(out, lines, insert_at, new_segs);
}

pub(crate) fn insert_line(out: &mut MakeSegsState, lines: &mut Lines, at: usize, segs: Vec<SegmentIdx>) {
    let line_i = LineIdx(lines.owned_lines.len());
    lines.owned_lines.push(Line{
        index: at,
        segs: segs,
    });
    for (i, seg_i) in lines.owned_lines.get(line_i.0).unwrap().segs.iter().enumerate() {
        let mut seg = out.segs.get_mut(seg_i.0).unwrap();
        match seg.line.as_mut() {
            Some(l) => {
                l.line = line_i;
                l.seg_index = i;
            },
            None => {
                seg.line = Some(SegmentLine{
                    line: line_i,
                    seg_index: i,
                });
            },
        };
    }
    lines.lines.insert(at, line_i);
    for (i, line_i) in lines.lines.iter().enumerate().skip(at + 1) {
        lines.owned_lines.get_mut(line_i.0).unwrap().index = i;
    }
    for (i, line_i) in lines.lines.iter().enumerate() {
        let line = lines.owned_lines.get(line_i.0).unwrap();
        assert_eq!(line.index, i, "line index wrong; after insert at line {}", at);
        for (j, seg_i) in line.segs.iter().enumerate() {
            assert_eq!(
                out.segs.get(seg_i.0).unwrap().line.as_ref().unwrap().seg_index,
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
        if self.0.as_ref().borrow_mut().active {
            4usize + parent
        } else {
            parent
        }
    }
}

pub(crate) struct SplitGroupBuilder {
    node: SplitGroupIdx,
    initial_split: bool,
    reverse_children: bool,
    segs: Vec<SegmentIdx>,
    children: Vec<SplitGroupIdx>,
}

impl SplitGroupBuilder {
    pub(crate) fn add(&mut self, out: &mut MakeSegsState, seg: Segment) {
        let idx = SegmentIdx(out.segs.len());
        out.segs.push(seg);
        self.segs.push(idx);
    }

    pub(crate) fn initial_split(&mut self) {
        self.initial_split = true;
    }

    pub(crate) fn reverse_children(&mut self) {
        self.reverse_children = true;
    }

    pub(crate) fn child(&mut self, child: SplitGroupIdx) {
        self.children.push(child);
    }

    pub(crate) fn build(self, out: &mut MakeSegsState) -> SplitGroupIdx {
        let mut sg = out.nodes.get_mut(self.node.0).unwrap();
        sg.split = self.initial_split;
        sg.children = self.children;
        if self.reverse_children {
            sg.children.reverse();
        }
        sg.segments = self.segs;
        for seg in &sg.segments {
            out.segs.get_mut(seg.0).unwrap().node = self.node;
        }
        return self.node;
    }

    pub(crate) fn seg(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Segment{
            node: self.node,
            line: None,
            mode: SegmentMode::All,
            content: SegmentContent::Text(text.to_string()),
        });
    }

    pub(crate) fn seg_split(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Segment{
            node: self.node,
            line: None,
            mode: SegmentMode::Split,
            content: SegmentContent::Text(text.to_string()),
        });
    }

    pub(crate) fn seg_unsplit(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Segment{
            node: self.node,
            line: None,
            mode: SegmentMode::Unsplit,
            content: SegmentContent::Text(text.to_string()),
        });
    }

    pub(crate) fn split_if(&mut self, out: &mut MakeSegsState, alignment: Alignment, always: bool, activate: bool) {
        self.add(out, Segment{
            node: self.node,
            line: None,
            mode: if always {
                SegmentMode::All
            } else {
                SegmentMode::Split
            },
            content: SegmentContent::Break(alignment, activate),
        });
    }

    pub(crate) fn split(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(out, Segment{
            node: self.node,
            line: None,
            mode: SegmentMode::Split,
            content: SegmentContent::Break(alignment, activate),
        });
    }

    pub(crate) fn split_always(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(out, Segment{
            node: self.node,
            line: None,
            mode: SegmentMode::All,
            content: SegmentContent::Break(alignment, activate),
        });
    }
}

pub(crate) fn new_sg(out: &mut MakeSegsState) -> SplitGroupBuilder {
    let idx = SplitGroupIdx(out.nodes.len());
    out.nodes.push(SplitGroup{
        split: false,
        segments: vec![],
        children: vec![],
    });
    SplitGroupBuilder{
        node: idx,
        segs: vec![],
        children: vec![],
        initial_split: false,
        reverse_children: false,
    }
}

pub(crate) fn new_sg_lit(
    out: &mut MakeSegsState,
    start: Option<(&Alignment, LineColumn)>,
    text: impl ToString,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    if let Some(loc) = start {
        append_comments(out, loc.0, &mut sg, loc.1);
    }
    sg.seg(out, text.to_string());
    sg.build(out)
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
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx;
}

impl<F: Fn(&mut MakeSegsState, &Alignment) -> SplitGroupIdx> Formattable for F {
    fn make_segs(&self, line: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        self(line, base_indent)
    }
}

impl Formattable for Ident {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_lit(out, Some((base_indent, self.span().start())), self)
    }
}

impl Formattable for &Ident {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
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
            split_brace_threshold: Some(1usize),
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
        nodes: vec![],
        segs: vec![],
        comments: comments,
        split_brace_threshold: config.split_brace_threshold,
        split_attributes: config.split_attributes,
    };
    let base_indent = Alignment(Rc::new(RefCell::new(Alignment_{
        parent: None,
        active: false,
    })));
    let root = ast.make_segs(&mut out, &base_indent);
    let mut lines = Lines{
        lines: vec![],
        owned_lines: vec![],
    };
    {
        let line_i = LineIdx(lines.owned_lines.len());
        lines.owned_lines.push(Line{
            index: 0,
            segs: out.segs.iter().enumerate().map(|(i, _)| SegmentIdx(i)).collect(),
        });
        lines.lines.push(line_i);
        for line_i in &lines.lines {
            for (j, seg_i) in lines.owned_lines.get(line_i.0).unwrap().segs.iter().enumerate() {
                out.segs.get_mut(seg_i.0).unwrap().line = Some(SegmentLine{
                    line: *line_i,
                    seg_index: j,
                });
            }
        }
    }
    for (i, line_i) in lines.lines.iter().enumerate() {
        let line = lines.owned_lines.get(line_i.0).unwrap();
        assert_eq!(line.index, i, "line index wrong; initial");
        for (j, seg_i) in line.segs.iter().enumerate() {
            assert_eq!(
                out.segs.get(seg_i.0).unwrap().line.as_ref().unwrap().seg_index,
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
        let synth_seg_node = new_sg(&mut out).build(&mut out);
        let mut i = 0usize;
        let mut skip_first = false;
        let mut prev_comment = None;
        while i < lines.lines.len() {
            let mut res = None;
            {
                let line_i = lines.lines.get(i).unwrap();
                'segs : loop {
                    for (i, seg_i) in lines.owned_lines.get(line_i.0).unwrap().segs.iter().enumerate() {
                        if i == 0 && skip_first {
                            skip_first = false;
                            continue;
                        }
                        let seg = out.segs.get(seg_i.0).unwrap();
                        let node = out.nodes.get(seg.node.0).unwrap();
                        match (&seg.content, match (&seg.mode, node.split) {
                            (SegmentMode::All, true) => true,
                            (SegmentMode::All, false) => true,
                            (SegmentMode::Unsplit, true) => false,
                            (SegmentMode::Unsplit, false) => true,
                            (SegmentMode::Split, true) => true,
                            (SegmentMode::Split, false) => false,
                        }) {
                            (SegmentContent::Break(_, _), true) => {
                                res = Some((line_i.clone(), i, None));
                                prev_comment = None;
                                break 'segs;
                            },
                            (SegmentContent::Comment(c), _) => {
                                res = Some((line_i.clone(), i, None));
                                prev_comment = Some(c.0.clone());
                                break 'segs;
                            },
                            (_, _) => {
                                if let Some(a) = prev_comment.take() {
                                    let seg_i = SegmentIdx(out.segs.len());
                                    out.segs.push(Segment{
                                        node: synth_seg_node.clone(),
                                        line: None,
                                        mode: SegmentMode::All,
                                        content: SegmentContent::Break(a, true),
                                    });
                                    res = Some((*line_i, i, Some(seg_i)));
                                    break 'segs;
                                }
                            },
                        };
                    }
                    prev_comment = None;
                    break;
                }
            }
            if let Some((line_i, at, insert_start)) = res {
                split_line_at(&mut out, &mut lines, line_i, at, insert_start);
                skip_first = true;
            }
            i += 1;
        }
    }

    // Do width based splitting, other splitting
    fn recurse(out: &mut MakeSegsState, lines: &mut Lines, config: &FormatConfig, sg_i: SplitGroupIdx) -> bool {
        let mut split = false;
        for seg_i in &out.nodes.get(sg_i.0).unwrap().segments {
            let seg = out.segs.get(seg_i.0).unwrap();
            let len = line_length(out, lines, seg.line.as_ref().unwrap().line);
            if len > config.max_width {
                split = true;
                break;
            }
        }
        if split {
            split_group(out, lines, sg_i);
        }
        let mut split_from_child = false;
        for child_sg_i in &out.nodes.get(sg_i.0).unwrap().children.clone() {
            let new_split_from_child = recurse(out, lines, config, *child_sg_i);
            split_from_child = split_from_child || new_split_from_child;
        }
        if !split && split_from_child {
            split_group(out, lines, sg_i);
        }
        config.root_splits && (split || split_from_child)
    }

    recurse(&mut out, &mut lines, config, root);

    // Render
    let mut rendered = String::new();
    let lines = lines;
    let mut line_i_i = 0usize;
    while line_i_i < lines.lines.len() {
        'continue_lineloop : loop {
            let segs =
                lines
                    .owned_lines
                    .get(lines.lines.get(line_i_i).unwrap().0)
                    .unwrap()
                    .segs
                    .iter()
                    .filter_map(|seg_i| {
                        if {
                            let seg = out.segs.get(seg_i.0).unwrap();
                            let node = out.nodes.get(seg.node.0).unwrap();
                            match (&seg.mode, node.split) {
                                (SegmentMode::All, _) => true,
                                (SegmentMode::Unsplit, true) => false,
                                (SegmentMode::Unsplit, false) => true,
                                (SegmentMode::Split, true) => true,
                                (SegmentMode::Split, false) => false,
                            }
                        } {
                            Some(*seg_i)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<SegmentIdx>>();
            if segs.is_empty() {
                break 'continue_lineloop;
            }
            for seg_i in &segs {
                let seg = out.segs.get(seg_i.0).unwrap();
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
                                .get(line_i_i + 1)
                                .map(|i| lines.owned_lines.get(i.0).unwrap())
                                .and_then(|l| l.segs.iter().next())
                                .map(|seg_i| {
                                    let seg = out.segs.get(seg_i.0).unwrap();
                                    match &seg.content {
                                        SegmentContent::Comment(_) => true,
                                        _ => false,
                                    }
                                })
                                .unwrap_or(false) {
                            break 'continue_lineloop;
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
                                    rendered.push_str(format!("{}{}", prefix, line).trim());
                                    rendered.push_str("\n");
                                }
                            }
                        }
                    },
                }
            }
            rendered.push_str("\n");
            break;
        }
        line_i_i += 1;
    }
    Ok(FormatRes{
        rendered: rendered,
        lost_comments: out.comments,
    })
}
