use std::collections::HashMap;

use convert_case::{Case, Casing};
use syn::{punctuated::Punctuated, spanned::Spanned, Ident, PathSegment};
use syn_prelude::{JoinSynErrors, ToErr, ToIdent, ToIdentWithCase, ToSynError};

use crate::{
    dep::Deps,
    model::{FieldType, Import, Message, MessageElement, Package, Type},
};

#[derive(Debug)]
pub struct ResolveContext<'a> {
    pub package: &'a Option<Package>,
    pub types: HashMap<Ident, InsideType>,
    pub deps: &'a Deps,
    pub imports: &'a Vec<Import>,
}

#[derive(Debug)]
pub enum InsideType {
    Message(MessageHierarchy),
    Enum(Ident),
}

#[derive(Debug)]
pub struct MessageHierarchy {
    pub name: Vec<Ident>,
    pub inner_messages: Option<Vec<MessageHierarchy>>,
    pub inner_enums: Option<Vec<Ident>>,
}

impl Message {
    pub fn resolve(&mut self, ctx: &ResolveContext) -> syn::Result<()> {
        if let Some(InsideType::Message(hierarchy)) = ctx.types.get(&self.name) {
            self.resolve_field_types(ctx, hierarchy)
        } else {
            Ok(())
        }
    }
    fn resolve_field_types(
        &mut self,
        ctx: &ResolveContext,
        hierarchy: &MessageHierarchy,
    ) -> syn::Result<()> {
        let result = self
            .messages
            .iter_mut()
            .enumerate()
            .filter_map(|(index, inner_message)| {
                hierarchy.inner_messages.as_ref().map(|messages| {
                    inner_message.resolve_field_types(ctx, messages.get(index).unwrap())
                })
            })
            .collect::<Vec<_>>()
            .join_errors();

        let result2 = self
            .fields
            .iter_mut()
            .map(|f| match f {
                MessageElement::Field(f) => ctx.resolve_field_type(Some(hierarchy), &mut f.typ),
                MessageElement::OneOf(oneof) => oneof
                    .fields
                    .iter_mut()
                    .map(|f| ctx.resolve_field_type(Some(hierarchy), &mut f.typ))
                    .collect::<Vec<syn::Result<_>>>()
                    .join_errors(),
            })
            .collect::<Vec<syn::Result<_>>>()
            .join_errors();

        (result, result2).join_errors()
    }
}

impl From<(&Vec<Ident>, &Message)> for MessageHierarchy {
    fn from((path, value): (&Vec<Ident>, &Message)) -> Self {
        let mut current_path = Vec::with_capacity(path.len() + 1);
        current_path.clone_from(path);
        current_path.push(value.name.clone());
        let inner_messages = if value.messages.is_empty() {
            None
        } else {
            Some(
                value
                    .messages
                    .iter()
                    .map(|m| Self::from((&current_path, m)))
                    .collect::<Vec<_>>(),
            )
        };
        let inner_enums = if value.enums.is_empty() {
            None
        } else {
            Some(
                value
                    .enums
                    .iter()
                    .map(|e| e.name.clone())
                    .collect::<Vec<_>>(),
            )
        };
        Self {
            name: current_path,
            inner_messages,
            inner_enums,
        }
    }
}

impl MessageHierarchy {
    fn find_message(&self, name: &Ident) -> Option<&Self> {
        if let Some(inner_messages) = &self.inner_messages {
            inner_messages
                .iter()
                .find(|m| m.name.last().map(|n| n.eq(name)).unwrap_or_default())
        } else {
            None
        }
    }

    fn find_enum(&self, name: &Ident) -> Option<&Ident> {
        if let Some(inner_enums) = &self.inner_enums {
            inner_enums.iter().find(|e| name.eq(*e))
        } else {
            None
        }
    }
}

impl ResolveContext<'_> {
    pub fn resolve_type(
        &self,
        parent: Option<&MessageHierarchy>,
        typ: &mut Type,
    ) -> syn::Result<()> {
        let import0 = self.imports.get(0).unwrap();
        let type_path_seg_first = typ
            .type_path
            .segments
            .first()
            .ok_or(typ.type_path.span().to_syn_error("missing type name"))?;

        // lookup inner types
        if let Some(container) = parent {
            let mut type_name_iter = typ.type_path.segments.iter();
            if let Some(is_message) = Self::match_with_message_inner_type(
                import0,
                self.package.as_ref().map(|p| &p.package),
                container,
                &mut type_name_iter,
                &mut typ.complete_path,
            )? {
                typ.target_is_message = is_message;
                return Ok(());
            }
        }
        // search in proto
        if let Some(t) = self.types.get(type_path_seg_first) {
            match t {
                InsideType::Message(hierarchy) => {
                    if typ.type_path.segments.len() > 1 {
                        if let Some(is_message) = Self::match_with_message_inner_type(
                            import0,
                            self.package.as_ref().map(|p| &p.package),
                            hierarchy,
                            &mut typ.type_path.segments.iter().skip(1),
                            &mut typ.complete_path,
                        )? {
                            typ.target_is_message = is_message;
                            return Ok(());
                        }
                    } else {
                        typ.complete_path.push_import_with_scope(
                            import0,
                            self.package.as_ref().map(|p| &p.package),
                        );
                        typ.complete_path.push(type_path_seg_first);
                        typ.target_is_message = true;
                        return Ok(());
                    }
                }
                InsideType::Enum(e) => {
                    if typ.type_path.segments.len() > 1 {
                        typ.type_path
                            .span()
                            .to_syn_error("cannot find this type in inner enumeration")
                            .to_err()?;
                    } else {
                        typ.complete_path.push_import_with_scope(
                            import0,
                            self.package.as_ref().map(|p| &p.package),
                        );
                        typ.complete_path.push(e);
                        typ.target_is_message = false;
                        return Ok(());
                    }
                }
            }
        }
        // search in import-wide
        let type_path_first_str = type_path_seg_first.to_string();
        let try_package_name = type_path_seg_first.clone();
        if if type_path_first_str.is_case(Case::UpperCamel) {
            // try search default scope first
            self.match_with_external_type(None, typ)?
                || self.match_with_external_type(Some(&try_package_name), typ)?
        } else {
            self.match_with_external_type(Some(&try_package_name), typ)?
                || self.match_with_external_type(None, typ)?
        } {
            return Ok(());
        }

        typ.type_path
            .span()
            .to_syn_error(&format!(
                "no such type '{}'",
                typ.type_path
                    .segments
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ))
            .to_err()
    }

    pub fn resolve_field_type(
        &self,
        parent: Option<&MessageHierarchy>,
        typ: &mut FieldType,
    ) -> syn::Result<()> {
        match typ {
            FieldType::MessageOrEnum(typ) => self.resolve_type(parent, typ),
            FieldType::Map(map) => (
                self.resolve_field_type(parent, map.key.as_mut()),
                self.resolve_field_type(parent, map.value.as_mut()),
            )
                .join_errors(),
            _ => Ok(()),
        }
    }

    fn match_with_message_inner_type<'a>(
        import0: &Import,
        package: Option<&Ident>,
        hierarchy: &'a MessageHierarchy,
        type_name_iter: &mut impl Iterator<Item = &'a Ident>,
        full_type_path: &mut syn::Path,
    ) -> syn::Result<Option<bool>> {
        if let Some(type_name_seg) = type_name_iter.next() {
            if let Some(msg) = hierarchy.find_message(type_name_seg) {
                if type_name_iter.count() == 0 {
                    full_type_path.push_import_with_scope(import0, package);
                    full_type_path.push_type_vec(&msg.name, false);
                    return Ok(Some(true));
                }
                Self::match_with_message_inner_type(
                    import0,
                    package,
                    msg,
                    type_name_iter,
                    full_type_path,
                )
            } else if let Some(enum_name) = hierarchy.find_enum(type_name_seg) {
                if type_name_iter.count() == 0 {
                    full_type_path.push_import_with_scope(import0, package);
                    full_type_path.push_type_vec(&hierarchy.name, true);
                    full_type_path.push(enum_name);
                    Ok(Some(false))
                } else {
                    // cannot find more types in enumeration
                    type_name_seg
                        .to_syn_error("cannot find this type in inner enumeration")
                        .to_err()
                }
            } else {
                // cannot find this inner type in message
                Ok(None)
            }
        } else {
            // unreachable
            Ok(None)
        }
    }

    fn match_with_external_type(
        &self,
        package: Option<&Ident>,
        typ: &mut Type,
    ) -> syn::Result<bool> {
        let skip = if package.is_some() { 1 } else { 0 };
        if let Some(scope) = self
            .deps
            .scopes
            .get(&package.map(|p| p.to_string()).unwrap_or_default())
        {
            let type_path_str = typ
                .type_path
                .segments
                .iter()
                .skip(skip)
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(".");
            if let Some(x) = scope.get(&type_path_str) {
                if x.prost_type {
                    let mut type_name_iter = typ.type_path.segments.iter().skip(2);
                    let type_name = typ.type_path.segments.last().unwrap();
                    typ.complete_path
                        .push(&("prost", type_name.span()).to_ident());
                    typ.complete_path.push_type_path(&mut type_name_iter, false);
                } else {
                    typ.complete_path
                        .push_import_with_scope(self.imports.get(x.import_index).unwrap(), package)
                        .push_type_path(&mut typ.type_path.segments.iter().skip(skip), false);
                }
                return Ok(true);
            }
        }
        Ok(false)
    }
}

pub trait PathMod {
    fn new() -> Self;
    fn from_idents(idents: impl Iterator<Item = Ident>) -> Self;
    fn push(&mut self, type_name_seg: &Ident) -> &mut Self;
    fn push_ident(&mut self, type_name_seg: Ident) -> &mut Self;
    fn push_import_with_scope(&mut self, import: &Import, package: Option<&Ident>) -> &mut Self;
    fn push_type_path<'a>(
        &mut self,
        iter: &mut impl Iterator<Item = &'a Ident>,
        tailing: bool,
    ) -> &mut Self;
    fn push_type_vec(&mut self, name: &Vec<Ident>, tailing: bool) -> &mut Self;
}

impl PathMod for syn::Path {
    fn new() -> Self {
        Self {
            leading_colon: None,
            segments: Punctuated::new(),
        }
    }

    fn from_idents(mut idents: impl Iterator<Item = Ident>) -> Self {
        let mut new = Self::new();
        while let Some(seg) = idents.next() {
            if new.segments.is_empty() {
                new.push_ident(("crate", seg.span()).to_ident());
            }
            new.push_ident(seg);
        }
        new
    }

    fn push_ident(&mut self, type_name_seg: Ident) -> &mut Self {
        self.segments.push(PathSegment {
            ident: type_name_seg,
            arguments: syn::PathArguments::None,
        });
        self
    }

    fn push(&mut self, type_name_seg: &Ident) -> &mut Self {
        self.segments.push(PathSegment {
            ident: type_name_seg.clone(),
            arguments: syn::PathArguments::None,
        });
        self
    }

    fn push_import_with_scope(&mut self, import: &Import, package: Option<&Ident>) -> &mut Self {
        if let Some(file_path) = &import.file_path {
            for seg in file_path.mod_path.segments.iter() {
                self.segments.push(seg.clone());
            }
        }
        if let Some(package) = package {
            self.push(package);
        }
        self
    }

    fn push_type_path<'a>(
        &mut self,
        iter: &mut impl Iterator<Item = &'a Ident>,
        tailing: bool,
    ) -> &mut Self {
        while let Some(seg) = iter.next() {
            if tailing || iter.count() > 0 {
                self.push(&seg.to_ident_with_case(Case::Snake));
            } else {
                self.push(seg);
            }
        }
        self
    }
    fn push_type_vec(&mut self, name: &Vec<Ident>, tailing: bool) -> &mut Self {
        self.push_type_path(&mut name.iter(), tailing)
    }
}
