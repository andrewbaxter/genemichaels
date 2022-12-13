use std::{cell::RefCell, rc::Rc};
use syn::File;
use crate::{
    new_sg,
    sg_general::{has_comments, new_sg_attrs},
    Alignment,
    Formattable,
    FormattableStmt,
    MakeSegsState,
    SplitGroup,
};

impl Formattable for File {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        fn build_inner(out: &mut MakeSegsState, base_indent: &Alignment, ast: &File) -> Rc<RefCell<SplitGroup>> {
            new_sg_attrs(
                out,
                base_indent,
                &ast.attrs,
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg();
                    let mut previous_margin_group = crate::MarginGroup::None;
                    for (i, el) in ast.items.iter().enumerate() {
                        let (new_margin_group, want_margin) = el.want_margin();
                        if i > 0 && (previous_margin_group != new_margin_group || want_margin || has_comments(out, el)) {
                            sg.split_always(out, base_indent.clone(), true);
                        }
                        sg.split_always(out, base_indent.clone(), true);
                        sg.child((&el).make_segs(out, &base_indent));
                        previous_margin_group = new_margin_group;
                    }
                    sg.build()
                },
            )
        }

        if let Some(shebang) = &self.shebang {
            let mut sg = new_sg();
            sg.seg(out, shebang);
            sg.split_always(out, base_indent.clone(), true);
            sg.split_always(out, base_indent.clone(), true);
            sg.child(build_inner(out, base_indent, self));
            sg.build()
        } else { build_inner(out, base_indent, self) }
    }
}
