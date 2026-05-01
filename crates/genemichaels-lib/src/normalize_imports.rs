use std::collections::BTreeMap;
use crate::{
    FormatConfig,
    ImportNormalizationMode,
};
use crate::{
    Whitespace,
    WhitespaceMode,
};
use crate::whitespace::HashLineColumn;
use syn::{
    Item,
    ItemUse,
    Stmt,
    UseTree,
    UsePath,
    UseName,
    UseRename,
    UseGlob,
    File,
    ItemMod,
    Block,
};
use syn::visit_mut::VisitMut;
use quote::ToTokens;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum CmpTree {
    Name(syn::Ident),
    Rename(syn::Ident, syn::Ident),
    Glob(String),
}

#[derive(Clone)]
struct FlatUseCombine {
    path: Vec<syn::UsePath>,
    leaf: syn::UseTree,
}

fn flatten_tree_combine(tree: UseTree, current_path: &mut Vec<syn::UsePath>, out: &mut Vec<FlatUseCombine>) {
    match tree {
        UseTree::Path(p) => {
            let inner = *p.tree.clone();
            let mut p_shallow = p.clone();
            p_shallow.tree =
                Box::new(
                    UseTree::Name(UseName { ident: syn::Ident::new("dummy", proc_macro2::Span::call_site()) }),
                );
            current_path.push(p_shallow);
            flatten_tree_combine(inner, current_path, out);
            current_path.pop();
        },
        UseTree::Group(g) => {
            for item in g.items {
                flatten_tree_combine(item, current_path, out);
            }
        },
        _ => {
            out.push(FlatUseCombine {
                path: current_path.clone(),
                leaf: tree,
            });
        },
    }
}

fn cmp_tree_node(tree: &syn::UseTree) -> CmpTree {
    match tree {
        syn::UseTree::Name(n) => CmpTree::Name(n.ident.clone()),
        syn::UseTree::Rename(r) => CmpTree::Rename(r.ident.clone(), r.rename.clone()),
        syn::UseTree::Glob(_) => CmpTree::Glob("*".to_string()),
        syn::UseTree::Path(p) => CmpTree::Name(p.ident.clone()),
        syn::UseTree::Group(_) => CmpTree::Glob("".to_string()),
    }
}

fn group_flat_uses(
    flats: Vec<FlatUseCombine>,
    depth: usize,
    whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
) -> Vec<syn::UseTree> {
    let mut leaves = Vec::new();
    let mut mergeable: BTreeMap<String, (syn::UsePath, Vec<FlatUseCombine>)> = BTreeMap::new();
    let mut unmergeable: BTreeMap<HashLineColumn, (syn::UsePath, Vec<FlatUseCombine>)> = BTreeMap::new();
    for flat in flats {
        if depth < flat.path.len() {
            let p = &flat.path[depth];

            fn has_comments(
                span: proc_macro2::Span,
                whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
            ) -> bool {
                if let Some((_, ws)) = whitespaces.get(&HashLineColumn(span.start())) {
                    for w in ws.iter() {
                        if matches!(w.mode, WhitespaceMode::Comment(_)) {
                            return true;
                        }
                    }
                }
                false
            }

            let has_comment =
                has_comments(p.ident.span(), whitespaces) || has_comments(p.colon2_token.spans[0], whitespaces);
            if has_comment {
                unmergeable
                    .entry(HashLineColumn(p.ident.span().start()))
                    .or_insert_with(|| (p.clone(), Vec::new()))
                    .1
                    .push(flat);
            } else {
                let key = p.ident.to_string();
                mergeable.entry(key).or_insert_with(|| (p.clone(), Vec::new())).1.push(flat);
            }
        } else {
            leaves.push(flat);
        }
    }
    let mut items = Vec::new();
    leaves.sort_by(|a, b| cmp_tree_node(&a.leaf).cmp(&cmp_tree_node(&b.leaf)));
    for leaf in leaves {
        items.push(leaf.leaf);
    }
    let mut children_trees = Vec::new();
    let mut groups = Vec::new();
    for (_, group) in unmergeable {
        groups.push(group);
    }
    for (_, group) in mergeable {
        groups.push(group);
    }
    for (mut path_node, group_flats) in groups {
        let mut child_trees = group_flat_uses(group_flats, depth + 1, whitespaces);
        if child_trees.len() == 1 {
            path_node.tree = Box::new(child_trees.pop().unwrap());
            children_trees.push(syn::UseTree::Path(path_node));
        } else {
            let group = syn::UseGroup {
                brace_token: Default::default(),
                items: child_trees.into_iter().collect(),
            };
            path_node.tree = Box::new(syn::UseTree::Group(group));
            children_trees.push(syn::UseTree::Path(path_node));
        }
    }
    children_trees.sort_by(|a, b| cmp_tree_node(a).cmp(&cmp_tree_node(b)));
    items.extend(children_trees);
    items
}

fn attrs_and_comments_key(
    item: &ItemUse,
    whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
) -> String {
    let mut key = String::new();
    for attr in &item.attrs {
        key.push_str(&attr.to_token_stream().to_string());
    }
    let get_ws = |span: proc_macro2::Span| {
        let mut s = String::new();
        if let Some((_, ws)) = whitespaces.get(&HashLineColumn(span.start())) {
            for w in ws.iter() {
                if let WhitespaceMode::Comment(c) = &w.mode {
                    s.push_str(&c.lines);
                }
            }
        }
        s
    };
    key.push_str(&get_ws(item.use_token.span));
    key.push_str(&get_ws(item.semi_token.span));
    key
}

fn process_item_uses(
    imports: &mut Vec<ItemUse>,
    new_items: &mut Vec<ItemUse>,
    config: &FormatConfig,
    whitespaces: &mut BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
) {
    if imports.is_empty() {
        return;
    }
    match config.import_normalization {
        ImportNormalizationMode::None => {
            new_items.extend(imports.drain(..));
        },
        ImportNormalizationMode::Combine => {
            let mut subgroups: Vec<Vec<ItemUse>> = Vec::new();
            for item in imports.drain(..) {
                if let Some(last_subgroup) = subgroups.last_mut() {
                    let key1 = attrs_and_comments_key(&last_subgroup[0], whitespaces);
                    let key2 = attrs_and_comments_key(&item, whitespaces);
                    if key1 == key2 {
                        last_subgroup.push(item);
                    } else {
                        subgroups.push(vec![item]);
                    }
                } else {
                    subgroups.push(vec![item]);
                }
            }
            for mut subgroup in subgroups {
                if subgroup.len() == 1 {
                    new_items.push(subgroup.pop().unwrap());
                    continue;
                }
                let template = subgroup[0].clone();

                fn collect_spans(ts: proc_macro2::TokenStream, out: &mut std::collections::HashSet<HashLineColumn>) {
                    for tt in ts {
                        out.insert(HashLineColumn(tt.span().start()));
                        if let proc_macro2::TokenTree::Group(g) = tt {
                            collect_spans(g.stream(), out);
                        }
                    }
                }

                let mut original_tokens = std::collections::HashSet::new();
                for item in &subgroup {
                    collect_spans(item.tree.to_token_stream(), &mut original_tokens);
                }
                let mut all_flats = Vec::new();
                for item in &subgroup {
                    flatten_tree_combine(item.tree.clone(), &mut Vec::new(), &mut all_flats);
                }
                let new_trees = group_flat_uses(all_flats, 0, whitespaces);
                let mut new_tokens = std::collections::HashSet::new();
                for tree in &new_trees {
                    collect_spans(tree.to_token_stream(), &mut new_tokens);
                }
                for span_start in original_tokens {
                    if !new_tokens.contains(&span_start) {
                        whitespaces.remove(&span_start);
                    }
                }
                for item in subgroup.iter().skip(1) {
                    whitespaces.remove(&HashLineColumn(item.use_token.span.start()));
                    whitespaces.remove(&HashLineColumn(item.semi_token.span.start()));
                }
                let r = new_trees.len();
                if r > 1 {
                    whitespaces
                        .entry(HashLineColumn(template.use_token.span.start()))
                        .and_modify(|e| e.0 += r - 1);
                    whitespaces
                        .entry(HashLineColumn(template.semi_token.span.start()))
                        .and_modify(|e| e.0 += r - 1);
                }
                for tree in new_trees {
                    let mut new_item = template.clone();
                    new_item.tree = tree;
                    new_items.push(new_item);
                }
            }
        },
        ImportNormalizationMode::Split => {
            #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
            struct FlatUse {
                path: Vec<syn::Ident>,
                leaf: CmpTree,
            }

            fn flatten_tree(tree: UseTree, current_path: &mut Vec<syn::Ident>, out: &mut Vec<FlatUse>) {
                match tree {
                    UseTree::Path(p) => {
                        current_path.push(p.ident);
                        flatten_tree(*p.tree, current_path, out);
                        current_path.pop();
                    },
                    UseTree::Name(n) => {
                        out.push(FlatUse {
                            path: current_path.clone(),
                            leaf: CmpTree::Name(n.ident),
                        });
                    },
                    UseTree::Rename(r) => {
                        out.push(FlatUse {
                            path: current_path.clone(),
                            leaf: CmpTree::Rename(r.ident, r.rename),
                        });
                    },
                    UseTree::Glob(_) => {
                        out.push(FlatUse {
                            path: current_path.clone(),
                            leaf: CmpTree::Glob("*".to_string()),
                        });
                    },
                    UseTree::Group(g) => {
                        for item in g.items {
                            flatten_tree(item, current_path, out);
                        }
                    },
                }
            }

            for item in imports.drain(..) {
                // Split the import leaves into individual imports.
                let mut flat = Vec::new();
                flatten_tree(item.tree.clone(), &mut Vec::new(), &mut flat);
                let n = flat.len();
                if n > 1 {
                    whitespaces.entry(HashLineColumn(item.use_token.span.start())).and_modify(|e| e.0 += n - 1);
                    whitespaces.entry(HashLineColumn(item.semi_token.span.start())).and_modify(|e| e.0 += n - 1);
                }
                {
                    let tree: &UseTree = &item.tree;
                    for token in tree.to_token_stream() {
                        whitespaces.remove(&HashLineColumn(token.span().start()));
                    }
                };

                // Sort
                flat.sort();

                // Re-constitute the split items into actual syn ast nodes
                for f in flat {
                    let mut tree = match f.leaf {
                        CmpTree::Name(ident) => UseTree::Name(UseName { ident }),
                        CmpTree::Rename(ident, rename) => UseTree::Rename(UseRename {
                            ident,
                            rename,
                            as_token: Default::default(),
                        }),
                        CmpTree::Glob(_) => UseTree::Glob(UseGlob { star_token: Default::default() }),
                    };
                    for ident in f.path.into_iter().rev() {
                        tree = UseTree::Path(UsePath {
                            ident,
                            colon2_token: Default::default(),
                            tree: Box::new(tree),
                        });
                    }
                    let mut new_item = item.clone();
                    new_item.tree = tree;
                    new_items.push(new_item);
                }
            }
        },
    }
}

pub(crate) struct ImportNormalizer<'a> {
    pub(crate) config: &'a FormatConfig,
    pub(crate) whitespaces: &'a mut BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
}

impl<'a> ImportNormalizer<'a> {
    fn process_items(&mut self, items: &mut Vec<Item>) {
        let mut new_items = Vec::new();
        let mut consecutive_imports = Vec::new();
        for item in items.drain(..) {
            if let Item::Use(u) = item {
                consecutive_imports.push(u);
            } else {
                // Process and flush the current use group
                let mut uses = Vec::new();
                process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
                for u in uses {
                    new_items.push(Item::Use(u));
                }

                // Now handle the non-import item itself
                new_items.push(item);
            }
        }

        // Flush the final group
        let mut uses = Vec::new();
        process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
        for u in uses {
            new_items.push(Item::Use(u));
        }
        *items = new_items;
    }

    fn process_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut new_stmts = Vec::new();
        let mut consecutive_imports = Vec::new();
        for stmt in stmts.drain(..) {
            if let Stmt::Item(Item::Use(u)) = stmt {
                consecutive_imports.push(u);
            } else {
                // Process and flush the current use group
                let mut uses = Vec::new();
                process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
                for u in uses {
                    new_stmts.push(Stmt::Item(Item::Use(u)));
                }

                // Now handle the non-import item itself
                new_stmts.push(stmt);
            }
        }

        // Flush the final group
        let mut uses = Vec::new();
        process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
        for u in uses {
            new_stmts.push(Stmt::Item(Item::Use(u)));
        }
        *stmts = new_stmts;
    }
}

impl<'a> VisitMut for ImportNormalizer<'a> {
    fn visit_file_mut(&mut self, i: &mut File) {
        self.process_items(&mut i.items);
        syn::visit_mut::visit_file_mut(self, i);
    }

    fn visit_item_mod_mut(&mut self, i: &mut ItemMod) {
        if let Some((_, items)) = &mut i.content {
            self.process_items(items);
        }
        syn::visit_mut::visit_item_mod_mut(self, i);
    }

    fn visit_block_mut(&mut self, i: &mut Block) {
        self.process_stmts(&mut i.stmts);
        syn::visit_mut::visit_block_mut(self, i);
    }
}
