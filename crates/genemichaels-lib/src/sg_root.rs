use {
    crate::{
        Alignment,
        Formattable,
        MakeSegsState,
        SplitGroupIdx,
        new_sg,
        sg_general::{
            append_statement_list_raw,
            has_genem_skip_comment,
            new_sg_outer_attrs,
        },
    },
    quote::ToTokens,
    syn::{
        AttrStyle,
        File,
        spanned::Spanned,
    },
};

impl Formattable for File {
    fn has_attrs(&self) -> bool {
        !self.attrs.is_empty()
    }

    fn make_segs(&self, out: &mut MakeSegsState, base_indent: &Alignment) -> SplitGroupIdx {
        fn build_inner(out: &mut MakeSegsState, base_indent: &Alignment, ast: &File) -> SplitGroupIdx {
            new_sg_outer_attrs(
                out,
                base_indent,
                &ast.attrs,
                ast.span(),
                |out: &mut MakeSegsState, base_indent: &Alignment| {
                    let mut sg = new_sg(out);
                    append_statement_list_raw(out, base_indent, &mut sg, Some(&ast.attrs), &ast.items);
                    sg.build(out)
                },
            )
        }

        if let Some(shebang) = &self.shebang {
            let mut sg = new_sg(out);
            sg.seg(out, shebang);
            sg.split_always(out, base_indent.clone(), true);
            sg.split_always(out, base_indent.clone(), true);
            sg.child(build_inner(out, base_indent, self));
            sg.build(out)
        } else if let Some(text) = 'res_rustfmt_skip: {
            for attr in &self.attrs {
                if matches!(&attr.style, AttrStyle::Outer) {
                    continue;
                }
                if attr.meta.to_token_stream().to_string().contains("rustfmt :: skip") {
                    break 'res_rustfmt_skip self.span().source_text();
                }
            }
            if has_genem_skip_comment(out, self.span().start()) {
                break 'res_rustfmt_skip self.span().source_text();
            }
            break 'res_rustfmt_skip None;
        } {
            out.whitespaces.clear();
            let mut sg = new_sg(out);
            sg.seg(out, text);
            sg.build(out)
        } else {
            build_inner(out, base_indent, self)
        }
    }

    fn normalize_declarations(
        &mut self,
        config: &crate::FormatConfig,
        whitespaces:
            &mut std::collections::BTreeMap<crate::whitespace::HashLineColumn, (usize, Vec<crate::Whitespace>)>,
    ) {
        if config.declaration_normalization != crate::DeclarationNormalizationMode::None {
            let mut normalizer = crate::normalize_declarations::DeclarationNormalizer {
                config: config,
                whitespaces: whitespaces,
            };
            syn::visit_mut::VisitMut::visit_file_mut(&mut normalizer, self);
        }
    }

    fn normalize_imports(
        &mut self,
        config: &crate::FormatConfig,
        whitespaces: &mut std::collections::BTreeMap<crate::HashLineColumn, (usize, Vec<crate::Whitespace>)>,
    ) {
        if config.import_normalization != crate::ImportNormalizationMode::None {
            let mut normalizer = crate::normalize_imports::ImportNormalizer {
                config,
                whitespaces,
            };
            syn::visit_mut::VisitMut::visit_file_mut(&mut normalizer, self);
        }
    }
}
