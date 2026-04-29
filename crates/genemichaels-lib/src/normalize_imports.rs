use std::collections::{BTreeMap, BTreeSet};
use crate::{FormatConfig, ImportNormalizationMode};
use crate::{Whitespace, WhitespaceMode};
use crate::whitespace::HashLineColumn;
use syn::{Item, ItemUse, Stmt, UseTree, UsePath, UseName, UseRename, UseGlob, UseGroup, File, ItemMod, Block};
use syn::visit_mut::VisitMut;
use quote::ToTokens;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum CmpTree {
    Name(syn::Ident),
    Rename(syn::Ident, syn::Ident),
    Glob(String),
}

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
        }
        UseTree::Name(n) => {
            out.push(FlatUse { path: current_path.clone(), leaf: CmpTree::Name(n.ident) });
        }
        UseTree::Rename(r) => {
            out.push(FlatUse { path: current_path.clone(), leaf: CmpTree::Rename(r.ident, r.rename) });
        }
        UseTree::Glob(_) => {
            out.push(FlatUse { path: current_path.clone(), leaf: CmpTree::Glob("*".to_string()) });
        }
        UseTree::Group(g) => {
            for item in g.items {
                flatten_tree(item, current_path, out);
            }
        }
    }
}

fn flat_to_tree(flat: FlatUse) -> UseTree {
    let mut tree = match flat.leaf {
        CmpTree::Name(ident) => UseTree::Name(UseName { ident }),
        CmpTree::Rename(ident, rename) => UseTree::Rename(UseRename { ident, rename, as_token: Default::default() }),
        CmpTree::Glob(_) => UseTree::Glob(UseGlob { star_token: Default::default() }),
    };
    for ident in flat.path.into_iter().rev() {
        tree = UseTree::Path(UsePath {
            ident,
            colon2_token: Default::default(),
            tree: Box::new(tree),
        });
    }
    tree
}

#[derive(Default)]
struct BuildNode {
    children: BTreeMap<String, (syn::Ident, BuildNode)>,
    leaves: Vec<CmpTree>,
}

fn insert_flat(node: &mut BuildNode, mut path: std::vec::IntoIter<syn::Ident>, leaf: CmpTree) {
    if let Some(first) = path.next() {
        let entry = node.children.entry(first.to_string()).or_insert_with(|| (first.clone(), BuildNode::default()));
        insert_flat(&mut entry.1, path, leaf);
    } else {
        node.leaves.push(leaf);
    }
}

fn build_tree(mut node: BuildNode) -> Vec<UseTree> {
    let mut items = Vec::new();
    node.leaves.sort();
    for leaf in node.leaves {
        items.push(match leaf {
            CmpTree::Name(ident) => UseTree::Name(UseName { ident }),
            CmpTree::Rename(ident, rename) => UseTree::Rename(UseRename { ident, rename, as_token: Default::default() }),
            CmpTree::Glob(_) => UseTree::Glob(UseGlob { star_token: Default::default() }),
        });
    }
    for (_, (ident, child)) in node.children {
        let mut child_trees = build_tree(child);
        if child_trees.len() == 1 {
            items.push(UseTree::Path(UsePath {
                ident,
                colon2_token: Default::default(),
                tree: Box::new(child_trees.pop().unwrap()),
            }));
        } else {
            let group = UseGroup {
                brace_token: Default::default(),
                items: child_trees.into_iter().collect(),
            };
            items.push(UseTree::Path(UsePath {
                ident,
                colon2_token: Default::default(),
                tree: Box::new(UseTree::Group(group)),
            }));
        }
    }
    items
}

fn attrs_and_comments_key(item: &ItemUse, whitespaces: &BTreeMap<HashLineColumn, Vec<Whitespace>>) -> String {
    let mut key = String::new();
    for attr in &item.attrs {
        key.push_str(&attr.to_token_stream().to_string());
    }
    let get_ws = |span: proc_macro2::Span| {
        let mut s = String::new();
        if let Some(ws) = whitespaces.get(&HashLineColumn(span.start())) {
            for w in ws {
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

fn drop_tree_whitespaces(tree: &UseTree, whitespaces: &mut BTreeMap<HashLineColumn, Vec<Whitespace>>) {
    for token in tree.to_token_stream() {
        whitespaces.remove(&HashLineColumn(token.span().start()));
    }
}

fn process_item_uses(
    group: &mut Vec<ItemUse>,
    new_items: &mut Vec<ItemUse>,
    config: &FormatConfig,
    whitespaces: &mut BTreeMap<HashLineColumn, Vec<Whitespace>>,
    cloned: &mut BTreeMap<HashLineColumn, usize>,
) {
    if group.is_empty() {
        return;
    }
    if config.import_normalization == ImportNormalizationMode::Split {
        for item in group.drain(..) {
            let mut flat = Vec::new();
            flatten_tree(item.tree.clone(), &mut Vec::new(), &mut flat);
            flat.sort();
            let n = flat.len();
            if n > 1 {
                cloned.entry(HashLineColumn(item.use_token.span.start())).and_modify(|c| *c += n - 1).or_insert(n - 1);
                cloned.entry(HashLineColumn(item.semi_token.span.start())).and_modify(|c| *c += n - 1).or_insert(n - 1);
            }
            drop_tree_whitespaces(&item.tree, whitespaces);
            for f in flat {
                let mut new_item = item.clone();
                new_item.tree = flat_to_tree(f);
                new_items.push(new_item);
            }
        }
    } else if config.import_normalization == ImportNormalizationMode::Combine {
        // Group contiguous by attrs and comments
        let mut subgroups: Vec<Vec<ItemUse>> = Vec::new();
        for item in group.drain(..) {
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
            let template = subgroup[0].clone();
            let mut root = BuildNode::default();
            for item in &subgroup {
                let mut flat = Vec::new();
                flatten_tree(item.tree.clone(), &mut Vec::new(), &mut flat);
                for f in flat {
                    insert_flat(&mut root, f.path.into_iter(), f.leaf);
                }
                drop_tree_whitespaces(&item.tree, whitespaces);
            }
            // drop everything for others
            for item in subgroup.iter().skip(1) {
                whitespaces.remove(&HashLineColumn(item.use_token.span.start()));
                whitespaces.remove(&HashLineColumn(item.semi_token.span.start()));
            }

            let mut new_trees = build_tree(root);
            let r = new_trees.len();
            if r > 1 {
                cloned.entry(HashLineColumn(template.use_token.span.start())).and_modify(|c| *c += r - 1).or_insert(r - 1);
                cloned.entry(HashLineColumn(template.semi_token.span.start())).and_modify(|c| *c += r - 1).or_insert(r - 1);
            }
            for tree in new_trees {
                let mut new_item = template.clone();
                new_item.tree = tree;
                new_items.push(new_item);
            }
        }
    } else {
        new_items.extend(group.drain(..));
    }
}

pub(crate) struct ImportNormalizer<'a> {
    pub(crate) config: &'a FormatConfig,
    pub(crate) whitespaces: &'a mut BTreeMap<HashLineColumn, Vec<Whitespace>>,
    pub(crate) cloned: &'a mut BTreeMap<HashLineColumn, usize>,
}

impl<'a> ImportNormalizer<'a> {
    fn process_items(&mut self, items: &mut Vec<Item>) {
        let mut new_items = Vec::new();
        let mut current_group = Vec::new();

        for item in items.drain(..) {
            if let Item::Use(u) = item {
                current_group.push(u);
            } else {
                let mut uses = Vec::new();
                process_item_uses(&mut current_group, &mut uses, self.config, self.whitespaces, self.cloned);
                for u in uses {
                    new_items.push(Item::Use(u));
                }
                new_items.push(item);
            }
        }
        let mut uses = Vec::new();
        process_item_uses(&mut current_group, &mut uses, self.config, self.whitespaces, self.cloned);
        for u in uses {
            new_items.push(Item::Use(u));
        }
        *items = new_items;
    }

    fn process_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut new_stmts = Vec::new();
        let mut current_group = Vec::new();

        for stmt in stmts.drain(..) {
            if let Stmt::Item(Item::Use(u)) = stmt {
                current_group.push(u);
            } else {
                let mut new_items = Vec::new();
                process_item_uses(&mut current_group, &mut new_items, self.config, self.whitespaces, self.cloned);
                for u in new_items {
                    new_stmts.push(Stmt::Item(Item::Use(u)));
                }
                new_stmts.push(stmt);
            }
        }
        let mut new_items = Vec::new();
        process_item_uses(&mut current_group, &mut new_items, self.config, self.whitespaces, self.cloned);
        for u in new_items {
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
