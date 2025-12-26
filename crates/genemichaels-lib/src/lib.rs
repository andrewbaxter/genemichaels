use std::cell::Cell;
use {
    loga::{
        ea,
        Error,
    },
    proc_macro2::{
        Ident,
        LineColumn,
    },
    quote::ToTokens,
    serde::{
        Serialize,
        Deserialize,
    },
    sg_general::append_whitespace,
    std::{
        collections::BTreeMap,
        cell::RefCell,
        rc::Rc,
    },
    syn::File,
};
pub use whitespace::{
    format_md,
    HashLineColumn,
};

pub(crate) mod whitespace;
pub(crate) mod sg_expr;
pub(crate) mod sg_general;
pub(crate) mod sg_pat;
pub(crate) mod sg_statement;
pub(crate) mod sg_type;
pub(crate) mod sg_root;
pub(crate) mod sg_general_lists;

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum CommentMode {
    Normal,
    ExplicitNormal,
    DocInner,
    DocOuter,
    Verbatim,
}

#[derive(Debug)]
pub struct Comment {
    pub mode: CommentMode,
    pub lines: String,
    /// The offset in the original source where the comment started (location of // or
    /// /* that initiated this specific comment mode)
    pub orig_start_offset: usize,
}

#[derive(Debug)]
pub enum WhitespaceMode {
    BlankLines(usize),
    Comment(Comment),
}

#[derive(Debug)]
pub struct Whitespace {
    // The loc of the AST node this whitespace is associated with. Special loc (0, 1)
    // == end of file.
    pub loc: LineColumn,
    pub mode: WhitespaceMode,
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
    Whitespace((Alignment, Vec<Whitespace>)),
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
    whitespaces: BTreeMap<HashLineColumn, Vec<Whitespace>>,
    config: FormatConfig,
    // MakeSegsState should be cloned at each call level with flags overwritable, but
    // it's a huge amount of work and only applies to this field for now. So instead
    // manage externally (for now).
    /// Positive if processing within a macro, used to control macro tweaks.
    macro_depth: Rc<Cell<usize>>,
}

pub struct IncMacroDepth(Rc<Cell<usize>>);

impl IncMacroDepth {
    fn new(s: &MakeSegsState) -> Self {
        s.macro_depth.update(|x| x + 1);
        return Self(s.macro_depth.clone());
    }
}

impl Drop for IncMacroDepth {
    fn drop(&mut self) {
        self.0.update(|x| x - 1);
    }
}

pub(crate) fn check_split_brace_threshold(out: &MakeSegsState, count: usize) -> bool {
    out.config.split_brace_threshold.map(|t| count >= t).unwrap_or(false)
}

pub(crate) fn line_length(out: &MakeSegsState, lines: &Lines, line_i: LineIdx) -> usize {
    let mut len = 0;
    for seg_i in &lines.owned_lines.get(line_i.0).unwrap().segs {
        let seg = out.segs.get(seg_i.0).unwrap();
        match &seg.content {
            SegmentContent::Text(t) => len += t.chars().count(),
            SegmentContent::Break(b, _) => {
                if out.nodes.get(seg.node.0).unwrap().split {
                    len += out.config.indent_spaces * b.get().0;
                }
            },
            SegmentContent::Whitespace(_) => { },
        };
    }
    len
}

pub(crate) fn split_group(out: &mut MakeSegsState, lines: &mut Lines, sg_i: SplitGroupIdx) {
    let sg = out.nodes.get_mut(sg_i.0).unwrap();
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
        if let Some((line_i, off)) = res {
            split_line_at(out, lines, line_i, off, None);
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
            SegmentContent::Whitespace((a, _)) => {
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
    lines.owned_lines.push(Line {
        index: at,
        segs,
    });
    for (i, seg_i) in lines.owned_lines.get(line_i.0).unwrap().segs.iter().enumerate() {
        let seg = out.segs.get_mut(seg_i.0).unwrap();
        match seg.line.as_mut() {
            Some(l) => {
                l.line = line_i;
                l.seg_index = i;
            },
            None => {
                seg.line = Some(SegmentLine {
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

struct IndentLevel(usize);

#[derive(Clone)]
pub struct Alignment(Rc<RefCell<Alignment_>>);

impl std::fmt::Debug for Alignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "(align)".fmt(f)
    }
}

impl Alignment {
    pub(crate) fn indent(&self) -> Alignment {
        Alignment(Rc::new(RefCell::new(Alignment_ {
            parent: Some(self.clone()),
            active: false,
        })))
    }

    pub(crate) fn activate(&self) {
        self.0.borrow_mut().active = true;
    }

    pub(crate) fn get(&self) -> IndentLevel {
        let parent = match &self.0.as_ref().borrow().parent {
            Some(p) => p.get(),
            None => {
                return IndentLevel(0usize);
            },
        };
        if self.0.as_ref().borrow_mut().active {
            IndentLevel(parent.0 + 1)
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
        let sg = out.nodes.get_mut(self.node.0).unwrap();
        sg.split = self.initial_split;
        sg.children = self.children;
        if self.reverse_children {
            sg.children.reverse();
        }
        sg.segments = self.segs;
        for seg in &sg.segments {
            out.segs.get_mut(seg.0).unwrap().node = self.node;
        }
        self.node
    }

    pub(crate) fn seg(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Segment {
            node: self.node,
            line: None,
            mode: SegmentMode::All,
            content: SegmentContent::Text(text.to_string()),
        });
    }

    pub(crate) fn seg_split(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Segment {
            node: self.node,
            line: None,
            mode: SegmentMode::Split,
            content: SegmentContent::Text(text.to_string()),
        });
    }

    pub(crate) fn seg_unsplit(&mut self, out: &mut MakeSegsState, text: impl ToString) {
        self.add(out, Segment {
            node: self.node,
            line: None,
            mode: SegmentMode::Unsplit,
            content: SegmentContent::Text(text.to_string()),
        });
    }

    pub(crate) fn split_if(&mut self, out: &mut MakeSegsState, alignment: Alignment, always: bool, activate: bool) {
        self.add(out, Segment {
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
        self.add(out, Segment {
            node: self.node,
            line: None,
            mode: SegmentMode::Split,
            content: SegmentContent::Break(alignment, activate),
        });
    }

    pub(crate) fn split_always(&mut self, out: &mut MakeSegsState, alignment: Alignment, activate: bool) {
        self.add(out, Segment {
            node: self.node,
            line: None,
            mode: SegmentMode::All,
            content: SegmentContent::Break(alignment, activate),
        });
    }
}

pub(crate) fn new_sg(out: &mut MakeSegsState) -> SplitGroupBuilder {
    let idx = SplitGroupIdx(out.nodes.len());
    out.nodes.push(SplitGroup {
        split: false,
        segments: vec![],
        children: vec![],
    });
    SplitGroupBuilder {
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
        append_whitespace(out, loc.0, &mut sg, loc.1);
    }
    sg.seg(out, text.to_string());
    sg.build(out)
}

#[derive(PartialEq, Debug)]
pub(crate) enum MarginGroup {
    Attr,
    BlockDef,
    Import,
    None,
}

pub(crate) trait FormattablePunct {
    fn span_start(&self) -> LineColumn;
}

pub(crate) trait FormattableStmt: ToTokens + Formattable {
    fn want_margin(&self) -> (MarginGroup, bool);
}

pub trait Formattable {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx;
    fn has_attrs(&self) -> bool;
}

impl<F: Fn(&mut MakeSegsState, &Alignment) -> SplitGroupIdx> Formattable for F {
    fn make_segs(&self, line: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        self(line, base_indent)
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for Ident {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_lit(out, Some((base_indent, self.span().start())), self)
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

impl Formattable for &Ident {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        (*self).make_segs(out, base_indent)
    }

    fn has_attrs(&self) -> bool {
        false
    }
}

#[derive(Debug, Copy, Clone, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum IndentUnit {
    Spaces,
    Tabs,
}

fn render_indent(config: &FormatConfig, current_indent: IndentLevel) -> String {
    match config.indent_unit {
        IndentUnit::Spaces => return " ".repeat(config.indent_spaces * current_indent.0),
        IndentUnit::Tabs => return "\t".repeat(current_indent.0),
    }
}

#[derive(Debug, Copy, Clone, Deserialize, Serialize)]
#[serde(default)]
pub struct FormatConfig {
    pub max_width: usize,
    pub root_splits: bool,
    pub split_brace_threshold: Option<usize>,
    pub split_attributes: bool,
    pub split_where: bool,
    pub comment_width: Option<usize>,
    pub comment_errors_fatal: bool,
    pub keep_max_blank_lines: usize,
    pub indent_spaces: usize,
    /// Indent with spaces or tabs.
    pub indent_unit: IndentUnit,
    pub explicit_markdown_comments: bool,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            max_width: 120,
            root_splits: false,
            split_brace_threshold: Some(1usize),
            split_attributes: true,
            split_where: true,
            comment_width: Some(80usize),
            comment_errors_fatal: false,
            keep_max_blank_lines: 0,
            indent_spaces: 4,
            indent_unit: IndentUnit::Spaces,
            explicit_markdown_comments: false,
        }
    }
}

pub struct FormatRes {
    pub rendered: String,
    pub lost_comments: BTreeMap<HashLineColumn, Vec<Whitespace>>,
    pub warnings: Vec<Error>,
}

pub use whitespace::extract_whitespaces;

pub fn format_str(source: &str, config: &FormatConfig) -> Result<FormatRes, loga::Error> {
    let shebang;
    let shebang_line_off;
    let source1;
    if source.starts_with("#!/") {
        let shebang_end = match source.find("\n") {
            Some(o) => o + 1,
            None => source.len(),
        };
        shebang = Some(&source[..shebang_end]);
        source1 = &source[shebang_end..];
        shebang_line_off = 1;
    } else {
        shebang = None;
        source1 = source;
        shebang_line_off = 0;
    }
    let source = source1;
    let (whitespaces, tokens) = extract_whitespaces(config.keep_max_blank_lines, source)?;
    let out =
        format_ast(
            syn::parse2::<File>(
                tokens,
            ).map_err(
                |e| loga::err_with(
                    "Syn error parsing Rust code",
                    ea!(line = e.span().start().line + shebang_line_off, column = e.span().start().column, err = e),
                ),
            )?,
            config,
            whitespaces,
        )?;
    if let Some(shebang) = shebang {
        return Ok(FormatRes {
            rendered: format!("{}{}", shebang, out.rendered),
            lost_comments: out.lost_comments,
            warnings: out.warnings,
        });
    } else {
        return Ok(out);
    }
}

pub fn format_ast(
    ast: impl Formattable,
    config: &FormatConfig,
    whitespaces: BTreeMap<HashLineColumn, Vec<Whitespace>>,
) -> Result<FormatRes, loga::Error> {
    // Build text
    let mut out = MakeSegsState {
        nodes: vec![],
        segs: vec![],
        whitespaces,
        config: config.clone(),
        macro_depth: Default::default(),
    };
    let base_indent = Alignment(Rc::new(RefCell::new(Alignment_ {
        parent: None,
        active: false,
    })));
    let root = ast.make_segs(&mut out, &base_indent);
    if out.whitespaces.contains_key(&HashLineColumn(LineColumn {
        line: 0,
        column: 1,
    })) {
        let mut sg = new_sg(&mut out);
        append_whitespace(&mut out, &base_indent, &mut sg, LineColumn {
            line: 0,
            column: 1,
        });
        sg.build(&mut out);
    }
    let mut lines = Lines {
        lines: vec![],
        owned_lines: vec![],
    };
    {
        let line_i = LineIdx(lines.owned_lines.len());
        lines.owned_lines.push(Line {
            index: 0,
            segs: out.segs.iter().enumerate().map(|(i, _)| SegmentIdx(i)).collect(),
        });
        lines.lines.push(line_i);
        for line_i in &lines.lines {
            for (j, seg_i) in lines.owned_lines.get(line_i.0).unwrap().segs.iter().enumerate() {
                out.segs.get_mut(seg_i.0).unwrap().line = Some(SegmentLine {
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
                                res = Some((*line_i, i, None));
                                prev_comment = None;
                                break 'segs;
                            },
                            (SegmentContent::Whitespace(c), _) => {
                                res = Some((*line_i, i, None));
                                prev_comment = Some(c.0.clone());
                                break 'segs;
                            },
                            (_, _) => {
                                if let Some(a) = prev_comment.take() {
                                    let seg_i = SegmentIdx(out.segs.len());
                                    out.segs.push(Segment {
                                        node: synth_seg_node,
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

    macro_rules! push{
        ($text: expr) => {
            rendered.push_str($text);
        };
    }

    let mut warnings = vec![];
    let lines = lines;
    let mut line_i = 0usize;
    while line_i < lines.lines.len() {
        'continue_lineloop : loop {
            let segs =
                lines.owned_lines.get(lines.lines.get(line_i).unwrap().0).unwrap().segs.iter().filter_map(|seg_i| {
                    let res = {
                        let seg = out.segs.get(seg_i.0).unwrap();
                        let node = out.nodes.get(seg.node.0).unwrap();
                        match (&seg.mode, node.split) {
                            (SegmentMode::All, _) => true,
                            (SegmentMode::Unsplit, true) => false,
                            (SegmentMode::Unsplit, false) => true,
                            (SegmentMode::Split, true) => true,
                            (SegmentMode::Split, false) => false,
                        }
                    };
                    if res {
                        Some(*seg_i)
                    } else {
                        None
                    }
                }).collect::<Vec<SegmentIdx>>();
            if segs.is_empty() {
                break 'continue_lineloop;
            }
            for (seg_i, seg_mem_i) in segs.iter().enumerate() {
                let seg = out.segs.get(seg_mem_i.0).unwrap();
                match &seg.content {
                    SegmentContent::Text(t) => {
                        let t = if seg_i == 1 && line_i > 0 {
                            // Work around comments splitting lines at weird places (seg_i_i 0 == break,
                            // except on first line)
                            t.trim_start()
                        } else {
                            t
                        };
                        push!(t);
                    },
                    SegmentContent::Break(b, activate) => {
                        let next_line_first_seg_comment =
                            lines
                                .lines
                                .get(line_i + 1)
                                .map(|i| lines.owned_lines.get(i.0).unwrap())
                                .and_then(|l| l.segs.first())
                                .map(|seg_i| {
                                    let seg = out.segs.get(seg_i.0).unwrap();
                                    matches!(&seg.content, SegmentContent::Whitespace(_))
                                })
                                .unwrap_or(false);

                        // since comments are always new lines we end up with duped newlines sometimes if
                        // there's a (break), (comment) on consec lines. skip the break
                        if segs.len() == 1 && next_line_first_seg_comment {
                            break 'continue_lineloop;
                        }
                        if *activate {
                            b.activate();
                        }
                        if segs.len() > 1 {
                            // if empty line (=just break), don't write indent
                            push!(&render_indent(config, b.get()));
                        }
                    },
                    SegmentContent::Whitespace((b, whitespaces)) => {
                        for (comment_i, whitespace) in whitespaces.iter().enumerate() {
                            match &whitespace.mode {
                                WhitespaceMode::BlankLines(count) => {
                                    if *count > 0 {
                                        for _ in 0 .. *count {
                                            push!("\n");
                                        }
                                        continue;
                                    }
                                },
                                WhitespaceMode::Comment(comment) => {
                                    if comment_i > 0 {
                                        push!("\n");
                                    }
                                    let prefix = format!(
                                        //. .
                                        "{}//{} ",
                                        render_indent(config, b.get()),
                                        match comment.mode {
                                            CommentMode::Normal => "",
                                            CommentMode::ExplicitNormal => "?",
                                            CommentMode::DocInner => "!",
                                            CommentMode::DocOuter => "/",
                                            CommentMode::Verbatim => ".",
                                        }
                                    );
                                    let verbatim = match comment.mode {
                                        CommentMode::Verbatim => {
                                            true
                                        },
                                        CommentMode::Normal if config.explicit_markdown_comments => {
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
                                                    let err =
                                                        loga::err_with(
                                                            "Error formatting comments",
                                                            ea!(
                                                                line = whitespace.loc.line,
                                                                column = whitespace.loc.column,
                                                                comments = comment.lines
                                                            ),
                                                        );
                                                    if config.comment_errors_fatal {
                                                        return Err(err);
                                                    } else {
                                                        warnings.push(e);
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
                                        for (i, line) in comment.lines.lines().enumerate() {
                                            if i > 0 {
                                                push!("\n");
                                            }
                                            let line = line.strip_prefix(' ').unwrap_or(line);
                                            push!(&format!("{}{}", prefix, line.trim_end()));
                                        }
                                    }
                                },
                            }
                        }
                    },
                }
            }
            push!("\n");
            break;
        }
        line_i += 1;
    }
    Ok(FormatRes {
        rendered: rendered,
        lost_comments: out.whitespaces,
        warnings: warnings,
    })
}
