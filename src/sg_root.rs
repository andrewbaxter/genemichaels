use std::{cell::RefCell, rc::Rc};
use syn::File;
use crate::{
    new_sg,
    sg_general::{new_sg_outer_attrs, append_statement_list_raw},
    Alignment,
    Formattable,
    MakeSegsState,
    SplitGroup,
};

impl Formattable for File {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> Rc<RefCell<SplitGroup>> {
        fn build_inner(out: &mut MakeSegsState, base_indent: &Alignment, ast: &File) -> Rc<RefCell<SplitGroup>> {
            new_sg_outer_attrs(out, base_indent, &ast.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
                let mut sg = new_sg();
                append_statement_list_raw(out, base_indent, &mut sg, Some(&ast.attrs), &ast.items);
                sg.build()
            })
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
