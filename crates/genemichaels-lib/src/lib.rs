pub(crate) mod normalize_declarations;
pub(crate) mod normalize_imports;
pub(crate) mod sg_expr;
pub(crate) mod sg_general;
pub(crate) mod sg_general_lists;
pub(crate) mod sg_pat;
pub(crate) mod sg_root;
pub(crate) mod sg_statement;
pub(crate) mod sg_type;
pub(crate) mod whitespace;

pub use whitespace::{
    HashLineColumn,
    extract_whitespaces,
    format_md,
};
use {
    loga::{
        Error,
        ResultContext,
        ea,
    },
    proc_macro2::{
        Ident,
        LineColumn,
    },
    quote::ToTokens,
    serde::{
        Deserialize,
        Serialize,
    },
    sg_general::append_whitespace,
    std::{
        cell::{
            Cell,
            RefCell,
        },
        collections::BTreeMap,
        fs,
        io::Write,
        process::{
            Command,
            Stdio,
        },
        rc::Rc,
    },
    syn::File,
    tempfile::NamedTempFile,
};

pub trait Formattable {
    fn has_attrs(&self) -> bool;
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx;

    fn normalize_declarations(
        &mut self,
        _config: &FormatConfig,
        _whitespaces: &mut BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
    ) {

    }

    fn normalize_imports(
        &mut self,
        _config: &FormatConfig,
        _whitespaces: &mut BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
    ) {

    }
}

pub(crate) trait FormattablePunct {
    fn span_start(&self) -> LineColumn;
}

pub(crate) trait FormattableStmt: ToTokens + Formattable {
    fn want_margin(&self) -> (MarginGroup, bool);
}

#[derive(Clone)]
pub struct Alignment(Rc<RefCell<Alignment_>>);

impl Alignment {
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

    pub(crate) fn indent(&self) -> Alignment {
        Alignment(Rc::new(RefCell::new(Alignment_ {
            parent: Some(self.clone()),
            active: false,
        })))
    }
}

impl std::fmt::Debug for Alignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "(align)".fmt(f)
    }
}

pub(crate) struct Alignment_ {
    pub(crate) active: bool,
    pub(crate) parent: Option<Alignment>,
}

pub(crate) fn check_split_brace_threshold(out: &MakeSegsState, count: usize) -> bool {
    out.config.split_brace_threshold.map(|t| count >= t).unwrap_or(false)
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub lines: String,
    pub mode: CommentMode,
    /// The offset in the original source where the comment started (location of // or
    /// /* that initiated this specific comment mode)
    pub orig_start_offset: usize,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum CommentMode {
    Directive,
    DocInner,
    DocOuter,
    ExplicitNormal,
    Normal,
    Verbatim,
}

#[derive(Debug, Copy, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum DeclarationNormalizationCategory {
    Concrete,
    Const,
    Macro,
    MacroCall,
    Mod,
    Trait,
    Use,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Default)]
#[serde(rename_all = "snake_case")]
pub enum DeclarationNormalizationMode {
    Auto,
    ByCategory(Vec<DeclarationNormalizationCategory>),
    ByName,
    #[default]
    None,
}

fn default_true() -> bool {
    true
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExternalFormatterConfig {
    #[serde(default = "default_true")]
    pub adjust_indent: bool,
    pub commandline: Vec<String>,
}

pub fn format_ast(
    mut ast: impl Formattable,
    config: &FormatConfig,
    whitespaces: BTreeMap<HashLineColumn, Vec<Whitespace>>,
) -> Result<FormatRes, loga::Error> {
    let mut whitespaces: BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)> =
        whitespaces.into_iter().map(|(k, v)| (k, (1, v))).collect();
    ast.normalize_imports(config, &mut whitespaces);
    ast.normalize_declarations(config, &mut whitespaces);

    // Ensure any orphaned whitespace from import normalization (e.g. removed commas)
    // is preserved in order to alert for formatting bugs (i.e. import normalization
    // removed a symbol and didn't transpose the associated whitespace).
    for (_, (count, _)) in whitespaces.iter_mut() {
        if *count == 0 {
            *count = 1;
        }
    }

    // Build text
    let mut out = MakeSegsState {
        nodes: vec![],
        segs: vec![],
        whitespaces,
        config: config.clone(),
        macro_depth: Default::default(),
        warnings: vec![],
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

    struct Rendered {
        col: usize,
        text: String,
    }

    impl Rendered {
        fn push(&mut self, c: char) {
            self.text.push(c);
            if c == '\n' {
                self.col = 0;
            } else {
                self.col += 1;
            }
        }

        fn push_str(&mut self, s: &str) {
            self.text.push_str(s);
            if let Some(nl) = s.rfind('\n') {
                self.col = s[nl + 1..].chars().count();
            } else {
                self.col += s.chars().count();
            }
        }
    }

    // Render
    let mut rendered = Rendered {
        text: String::new(),
        col: 0,
    };
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
                        rendered.push_str(t);
                    },
                    SegmentContent::RawTextAdjustIndent(t) => {
                        // Find the prefix length (chars up to and including the opening `"`) so lines 1+
                        // align with the first line's content position.
                        let prefix_len = t.find('"').map(|i| i + 1).unwrap_or(0);
                        let indent_col = rendered.col + prefix_len;
                        let mut text_lines = t.split('\n');
                        if let Some(first_line) = text_lines.next() {
                            rendered.push_str(first_line);
                        }
                        for line in text_lines {
                            rendered.push('\n');
                            let indent_str = " ".repeat(indent_col);
                            rendered.push_str(&indent_str);
                            rendered.push_str(line);
                        }
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
                            rendered.push_str(&render_indent(config, b.get()));
                        }
                    },
                    SegmentContent::Whitespace((b, whitespaces)) => {
                        for (comment_i, whitespace) in whitespaces.iter().enumerate() {
                            match &whitespace.mode {
                                WhitespaceMode::BlankLines(count) => {
                                    if *count > 0 {
                                        for _ in 0 .. *count {
                                            rendered.push('\n');
                                        }
                                        continue;
                                    }
                                },
                                WhitespaceMode::Comment(comment) => {
                                    if comment_i > 0 {
                                        rendered.push('\n');
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
                                            CommentMode::Directive => "#",
                                        }
                                    );
                                    let verbatim;
                                    match comment.mode {
                                        CommentMode::Directive => {
                                            for (i, line) in comment.lines.lines().enumerate() {
                                                if i > 0 {
                                                    rendered.push('\n');
                                                }
                                                if let Some((k, v)) = line.split_once(":") {
                                                    rendered.push_str(
                                                        &format!("{}{}: {}", prefix, k.trim(), v.trim()),
                                                    );
                                                } else {
                                                    rendered.push_str(&format!("{}{}", prefix, line.trim()));
                                                }
                                            }
                                            continue;
                                        },
                                        CommentMode::Verbatim => {
                                            verbatim = true;
                                        },
                                        CommentMode::Normal if config.explicit_markdown_comments => {
                                            verbatim = true;
                                        },
                                        _ => {
                                            match format_md(&mut rendered.text, config, &prefix, &comment.lines) {
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
                                                    verbatim = true;
                                                },
                                                Ok(_) => {
                                                    let text = &rendered.text;
                                                    rendered.col = if let Some(nl_pos) = text.rfind('\n') {
                                                        text[nl_pos + 1..].chars().count()
                                                    } else {
                                                        text.chars().count()
                                                    };
                                                    verbatim = false;
                                                },
                                            }
                                        },
                                    };
                                    if verbatim {
                                        for (i, line) in comment.lines.lines().enumerate() {
                                            if i > 0 {
                                                rendered.push('\n');
                                            }
                                            let line = line.strip_prefix(' ').unwrap_or(line);
                                            rendered.push_str(&format!("{}{}", prefix, line.trim_end()));
                                        }
                                    }
                                },
                            }
                        }
                    },
                }
            }
            rendered.push('\n');
            break;
        }
        line_i += 1;
    }
    let mk_warnings = std::mem::take(&mut out.warnings);
    Ok(FormatRes {
        rendered: rendered.text,
        lost_comments: out.whitespaces.into_iter().map(|(k, (_, v))| (k, v)).collect(),
        warnings: {
            let mut all_warnings = mk_warnings;
            all_warnings.extend(warnings);
            all_warnings
        },
    })
}

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
    let stripped_source = source1;
    let (whitespaces, tokens) = extract_whitespaces(config.keep_max_blank_lines, stripped_source)?;
    for ws_list in whitespaces.values() {
        for ws in ws_list {
            if let WhitespaceMode::Comment(comment) = &ws.mode {
                if comment.mode == CommentMode::Directive {
                    for line in comment.lines.lines() {
                        if line.trim() == "genemichaels-file-skip" {
                            return Ok(FormatRes {
                                rendered: source.to_string(),
                                lost_comments: BTreeMap::new(),
                                warnings: vec![],
                            });
                        }
                    }
                }
            }
        }
    }
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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(default)]
pub struct FormatConfig {
    pub comment_errors_fatal: bool,
    pub comment_width: Option<usize>,
    pub declaration_normalization: DeclarationNormalizationMode,
    pub explicit_markdown_comments: bool,
    pub external_formatters: BTreeMap<String, ExternalFormatterConfig>,
    pub import_normalization: ImportNormalizationMode,
    pub indent_spaces: usize,
    /// Indent with spaces or tabs.
    pub indent_unit: IndentUnit,
    pub keep_max_blank_lines: usize,
    pub max_width: usize,
    pub root_splits: bool,
    pub split_attributes: bool,
    pub split_brace_threshold: Option<usize>,
    pub split_where: bool,
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
            import_normalization: ImportNormalizationMode::None,
            declaration_normalization: DeclarationNormalizationMode::None,
            external_formatters: BTreeMap::new(),
        }
    }
}

pub struct FormatRes {
    pub lost_comments: BTreeMap<HashLineColumn, Vec<Whitespace>>,
    pub rendered: String,
    pub warnings: Vec<Error>,
}

#[derive(Debug, Copy, Clone, Deserialize, Serialize, PartialEq, Default)]
#[serde(rename_all = "snake_case")]
pub enum ImportNormalizationMode {
    Combine,
    #[default]
    None,
    Split,
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

struct IndentLevel(usize);

#[derive(Debug, Copy, Clone, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum IndentUnit {
    Spaces,
    Tabs,
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

pub(crate) struct Line {
    index: usize,
    segs: Vec<SegmentIdx>,
}

pub(crate) fn line_length(out: &MakeSegsState, lines: &Lines, line_i: LineIdx) -> usize {
    let mut max_len = 0;
    let mut len = 0;
    for seg_i in &lines.owned_lines.get(line_i.0).unwrap().segs {
        let seg = out.segs.get(seg_i.0).unwrap();
        match &seg.content {
            SegmentContent::Text(t) => {
                let mut first = true;
                for line in t.split('\n') {
                    if !first {
                        max_len = max_len.max(len);
                        len = line.chars().count();
                    } else {
                        len += line.chars().count();
                        first = false;
                    }
                }
            },
            SegmentContent::Break(b, _) => {
                if out.nodes.get(seg.node.0).unwrap().split {
                    len += out.config.indent_spaces * b.get().0;
                }
            },
            SegmentContent::Whitespace(_) => { },
            SegmentContent::RawTextAdjustIndent(t) => {
                let prefix_len = t.find('"').map(|i| i + 1).unwrap_or(0);
                let start_len = len;
                let mut first = true;
                let mut local_max = 0;
                for line in t.split('\n') {
                    if !first {
                        len = start_len + prefix_len + line.chars().count();
                        local_max = local_max.max(len);
                    } else {
                        len += line.chars().count();
                        local_max = local_max.max(len);
                        first = false;
                    }
                }
                len = local_max;
            },
        };
    }
    max_len.max(len)
}

#[derive(Clone, Copy)]
pub(crate) struct LineIdx(usize);

struct Lines {
    lines: Vec<LineIdx>,
    owned_lines: Vec<Line>,
}

pub struct MakeSegsState {
    config: FormatConfig,
    // MakeSegsState should be cloned at each call level with flags overwritable, but
    // it's a huge amount of work and only applies to this field for now. So instead
    // manage externally (for now).
    /// Positive if processing within a macro, used to control macro tweaks.
    macro_depth: Rc<Cell<usize>>,
    nodes: Vec<SplitGroup>,
    segs: Vec<Segment>,
    pub(crate) warnings: Vec<Error>,
    whitespaces: BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
}

#[derive(PartialEq, Debug)]
pub(crate) enum MarginGroup {
    Attr,
    BlockDef,
    Import,
    None,
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
    start: Option<(&Alignment, Vec<LineColumn>)>,
    text: impl ToString,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    if let Some((base_indent, starts)) = start {
        for loc in starts {
            append_whitespace(out, base_indent, &mut sg, loc);
        }
    }
    sg.seg(out, text.to_string());
    sg.build(out)
}

fn render_indent(config: &FormatConfig, current_indent: IndentLevel) -> String {
    match config.indent_unit {
        IndentUnit::Spaces => return " ".repeat(config.indent_spaces * current_indent.0),
        IndentUnit::Tabs => return "\t".repeat(current_indent.0),
    }
}

pub(crate) fn run_external_formatter(command: &[String], content: &str) -> Result<String, loga::Error> {
    let mut tmp_file: Option<NamedTempFile> = None;
    let args = command[1..].iter().map(|a| -> Result<String, loga::Error> {
        if a == "{}" {
            let f = match tmp_file {
                Some(ref f) => f,
                None => {
                    let mut f = NamedTempFile::new().context("Failed to create external formatter temp file")?;
                    f.write_all(content.as_bytes()).context("Failed to write external formatter temp file")?;
                    f.flush().context("Failed to flush external formatter temp file")?;
                    tmp_file.insert(f)
                },
            };
            return Ok(f.path().to_string_lossy().to_string());
        } else {
            return Ok(a.clone());
        }
    }).collect::<Result<Vec<_>, _>>()?;
    if let Some(ref tmp_file) = tmp_file {
        let status =
            Command::new(&command[0])
                .args(&args)
                .status()
                .context_with("Failed to run external formatter", ea!(cmd = command[0].as_str()))?;
        if !status.success() {
            return Err(loga::err_with("External formatter exited with error", ea!(cmd = &command[0])));
        }
        return fs::read_to_string(tmp_file.path()).context("Failed to read external formatter output");
    } else {
        let mut child =
            Command::new(&command[0])
                .args(&args)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .context_with("Failed to spawn external formatter", ea!(cmd = command[0].as_str()))?;
        child
            .stdin
            .take()
            .unwrap()
            .write_all(content.as_bytes())
            .context("Failed to write to external formatter stdin")?;
        let output = child.wait_with_output().context("Failed to get external formatter output")?;
        if !output.status.success() {
            return Err(loga::err_with("External formatter exited with error", ea!(cmd = &command[0])));
        }
        return String::from_utf8(output.stdout)
            .map_err(loga::err)
            .context("External formatter output is not valid UTF-8");
    }
}

pub(crate) struct Segment {
    pub(crate) content: SegmentContent,
    pub(crate) line: Option<SegmentLine>,
    pub(crate) mode: SegmentMode,
    pub(crate) node: SplitGroupIdx,
}

#[derive(Debug)]
pub(crate) enum SegmentContent {
    Break(Alignment, bool),
    RawTextAdjustIndent(String),
    Text(String),
    Whitespace((Alignment, Vec<Whitespace>)),
}

#[derive(Clone, Copy)]
pub struct SegmentIdx(usize);

pub(crate) struct SegmentLine {
    pub(crate) line: LineIdx,
    pub(crate) seg_index: usize,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum SegmentMode {
    All,
    Split,
    Unsplit,
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

pub struct SplitGroup {
    pub(crate) children: Vec<SplitGroupIdx>,
    pub(crate) segments: Vec<SegmentIdx>,
    pub(crate) split: bool,
}

pub(crate) struct SplitGroupBuilder {
    children: Vec<SplitGroupIdx>,
    initial_split: bool,
    pub(crate) node: SplitGroupIdx,
    reverse_children: bool,
    segs: Vec<SegmentIdx>,
}

impl SplitGroupBuilder {
    pub(crate) fn add(&mut self, out: &mut MakeSegsState, seg: Segment) {
        let idx = SegmentIdx(out.segs.len());
        out.segs.push(seg);
        self.segs.push(idx);
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

    pub(crate) fn child(&mut self, child: SplitGroupIdx) {
        self.children.push(child);
    }

    pub(crate) fn initial_split(&mut self) {
        self.initial_split = true;
    }

    pub(crate) fn reverse_children(&mut self) {
        self.reverse_children = true;
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
}

#[derive(Clone, Copy)]
pub struct SplitGroupIdx(usize);

#[derive(Debug, Clone)]
pub struct Whitespace {
    // The loc of the AST node this whitespace is associated with. Special loc (0, 1)
    // == end of file.
    pub loc: LineColumn,
    pub mode: WhitespaceMode,
}

#[derive(Debug, Clone)]
pub enum WhitespaceMode {
    BlankLines(usize),
    Comment(Comment),
}

impl Formattable for &Ident {
    fn has_attrs(&self) -> bool {
        false
    }

    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        (*self).make_segs(out, base_indent)
    }
}

impl<F: Fn(&mut MakeSegsState, &Alignment) -> SplitGroupIdx> Formattable for F {
    fn has_attrs(&self) -> bool {
        false
    }

    fn make_segs(&self, line: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        self(line, base_indent)
    }
}

impl Formattable for Ident {
    fn has_attrs(&self) -> bool {
        false
    }

    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        new_sg_lit(out, Some((base_indent, vec![self.span().start()])), self)
    }
}
