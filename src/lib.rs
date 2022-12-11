use anyhow::Result;
use proc_macro2::{Ident, LineColumn};
use std::{borrow::Borrow, cell::RefCell, fmt::Write, rc::Rc};
use structre::UnicodeRegex;
use syn::File;

use crate::sg_general::new_sg_attrs;
// TODO
// line + block comments
//  block comments -> counted nested /* */, don't go by lines
//  grab before every token
//  add parts
// finish remaining elements
// wrapping (start from root, wrap any line any node seg is on)
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
    // Comment((Alignment, Comment)),
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

fn render_line_str(line: &RefCell<Line>) -> String {
    let mut rendered = String::new();
    render_line(&mut rendered, line);
    rendered
}

fn render_line(rendered: &mut String, line: &RefCell<Line>) {
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

pub(crate) fn new_sg_lit(out: &mut MakeSegsState, text: impl ToString) -> Rc<RefCell<SplitGroup>> {
    let mut node = new_sg();
    node.seg(out, text.to_string());
    node.build()
}

#[derive(PartialEq, Clone, Copy)]
pub(crate) enum CommentMode {
    Normal,
    DocInner,
    DocOuter,
}
pub(crate) struct Comment {
    pub(crate) mode: CommentMode,
    pub(crate) lines: Vec<String>,
}

pub(crate) struct MakeSegsState {
    pub(crate) source: String,
    pub(crate) line: Vec<Rc<RefCell<Segment>>>,
    pub(crate) last_loc: LineColumn,
    pub(crate) last_offset: usize,
}

impl MakeSegsState {
    pub(crate) fn get_comments(&mut self, loc: LineColumn) -> Vec<Comment> {
        let input = self.source.as_str();
        let mut offset = self.last_offset;
        let mut col = self.last_loc.column;
        for _ in self.last_loc.line..loc.line {
            offset += input[offset..].find('\n').unwrap() + 1;
            col = 0;
        }
        offset += input[offset..]
            .chars()
            .take(loc.column - col)
            .map(char::len_utf8)
            .sum::<usize>();
        let out = &input[self.last_offset..offset];
        self.last_offset = offset;
        self.last_loc = loc;

        let start_re = UnicodeRegex::new(r#"(//(:P/|!)|/\*(:P*|!))"#).unwrap();
        let block_event_re = UnicodeRegex::new(r#"(/\*|\*/)"#).unwrap();
        struct CommentBuffer {
            out: Vec<Comment>,
            mode: CommentMode,
            lines: Vec<String>,
        }

        impl CommentBuffer {
            fn flush(&mut self) {
                self.out.push(Comment {
                    mode: self.mode,
                    lines: self.lines.split_off(0),
                });
            }

            fn add(&mut self, mode: CommentMode, line: &str) {
                if self.mode != mode && !self.lines.is_empty() {
                    self.flush();
                }
                self.mode = mode;
                self.lines.push(line.trim().to_string());
            }

            fn add_continue(&mut self, line: &str) {
                self.lines.push(line.trim().to_string());
            }
        }

        let mut buffer = CommentBuffer {
            out: vec![],
            mode: CommentMode::Normal,
            lines: vec![],
        };

        let mut search_start_at = 0usize;
        'comment_loop: loop {
            match start_re.captures(&out[search_start_at..]) {
                Some(found_start) => {
                    let start_prefix_match = found_start.get(1).unwrap();
                    match start_prefix_match.as_str() {
                        "//" => {
                            let start_suffix_match = found_start.get(2);
                            let (mode, match_end) = match start_suffix_match {
                                Some(start_suffix_match) => match start_suffix_match.as_str() {
                                    "/" => (
                                        CommentMode::DocOuter,
                                        search_start_at + start_suffix_match.end(),
                                    ),
                                    "!" => (
                                        CommentMode::DocInner,
                                        search_start_at + start_suffix_match.end(),
                                    ),
                                    _ => unreachable!(),
                                },
                                None => (
                                    CommentMode::Normal,
                                    search_start_at + start_prefix_match.end(),
                                ),
                            };

                            let (line, next_start) = match out[match_end..].find('\n') {
                                Some(line_end) => (&out[match_end..line_end], line_end + 1),
                                None => (&out[match_end..], out.len()),
                            };

                            buffer.add(mode, line);
                            search_start_at = next_start;
                        }
                        "/*" => {
                            let (mode, match_end) = match found_start.get(2) {
                                Some(suffix_match) => match suffix_match.as_str() {
                                    "*" => (
                                        CommentMode::DocOuter,
                                        search_start_at + suffix_match.end(),
                                    ),
                                    "!" => (
                                        CommentMode::DocInner,
                                        search_start_at + suffix_match.end(),
                                    ),
                                    _ => unreachable!(),
                                },
                                None => (
                                    CommentMode::Normal,
                                    search_start_at + start_prefix_match.end(),
                                ),
                            };

                            let mut nesting = 1;
                            let mut search_end_at = match_end;
                            let (lines, next_start) = loop {
                                let found_event = block_event_re
                                    .captures(&out[search_end_at..])
                                    .unwrap()
                                    .get(1)
                                    .unwrap();
                                match found_event.as_str() {
                                    "/*" => {
                                        nesting += 1;
                                        search_end_at = search_end_at + found_event.end();
                                    }
                                    "*/" => {
                                        nesting -= 1;
                                        if nesting == 0 {
                                            break (
                                                &out[match_end..found_event.start()],
                                                search_end_at + found_event.end(),
                                            );
                                        } else {
                                            search_end_at = search_end_at + found_event.end();
                                        }
                                    }
                                    _ => unreachable!(),
                                }
                            };

                            for line in lines.lines() {
                                buffer.add(mode, line.trim());
                            }

                            search_start_at = next_start;
                        }
                        _ => unreachable!(),
                    }
                }
                None => {
                    break 'comment_loop;
                }
            }
        }

        buffer.flush();
        buffer.out
    }
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
        _base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        new_sg_lit(out, self)
    }
}

impl Formattable for &Ident {
    fn make_segs(
        &self,
        out: &mut MakeSegsState,
        _base_indent: &Alignment,
    ) -> Rc<RefCell<SplitGroup>> {
        new_sg_lit(out, self)
    }
}

pub fn format_str(code: &str, max_len: usize) -> Result<String> {
    format_ast(code, syn::parse_file(code)?, max_len)
}

pub fn format_ast(source: &str, ast: File, max_len: usize) -> Result<String> {
    // Build text
    let mut out = MakeSegsState {
        source: source.to_string(),
        line: vec![],
        last_loc: LineColumn { line: 1, column: 0 },
        last_offset: 0,
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
                        (SegmentContent::Break(_, _), SegmentMode::All) => {
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
        println!("{}", render_line_str(line));
        //render_line(&mut rendered, line);
        //rendered.push_str("\n").unwrap();
    }
    Ok(rendered)
}
