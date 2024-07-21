use syn::File;
use crate::{
    new_sg,
    sg_general::{
        new_sg_outer_attrs,
        append_statement_list_raw,
    },
    Alignment,
    Formattable,
    MakeSegsState,
    SplitGroupIdx,
};

impl Formattable for File {
    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        fn build_inner(out: &mut MakeSegsState, base_indent: &Alignment, ast: &File) -> SplitGroupIdx {
            new_sg_outer_attrs(out, base_indent, &ast.attrs, |out: &mut MakeSegsState, base_indent: &Alignment| {
                let mut sg = new_sg(out);
                append_statement_list_raw(out, base_indent, &mut sg, Some(&ast.attrs), &ast.items);
                sg.build(out)
            })
        }

        if let Some(shebang) = &self.shebang {
            let mut sg = new_sg(out);
            sg.seg(out, shebang);
            sg.split_always(out, base_indent.clone(), true);
            sg.split_always(out, base_indent.clone(), true);
            sg.child(build_inner(out, base_indent, self));
            sg.build(out)
        } else {
            build_inner(out, base_indent, self)
        }
    }

    fn has_attrs(&self) -> bool {
        !self.attrs.is_empty()
    }
}
