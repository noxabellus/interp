//! Analysis system

#![allow(dead_code)]

use std::{
  hash::{ Hash, Hasher },
  collections::hash_map::{
    DefaultHasher,
    HashMap,
    // RawEntryMut
  }
};

use super::{
  common::*,
  ast::{ Item, ItemData, TyExpr, TyExprData, ElementDecl },
};

use crate::{
  macros::unchecked_destructure,
  vm::{
    Context, Module, module::ModuleBinding,
    ModuleID, TypeID, GlobalID, typeinfo::PrimitiveType
  }
};


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SymbolData {
  Module(ModuleID),
  Type(TypeRef),
  Global(GlobalID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Symbol {
  data: SymbolData,
  loc: Loc
}


mod type_map {
  use super::*;

  #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
  pub struct TypeRef(u16);

  impl TypeRef {
    pub const MAX_TYPE_REFS: u16 = u16::MAX;
  }

  #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
  pub enum Ty {
    Array(TypeRef),
    Map(TypeRef, TypeRef),
    Record(Vec<String>, Vec<TypeRef>),
    Function(Vec<TypeRef>, Option<TypeRef>),
  }

  pub enum TypeEntry {
    Existing(TypeID),
    New(Ty),
    Redirect(TypeRef),
    Undefined,
  }

  #[derive(Default)]
  pub struct TypeMap {
    types: Vec<TypeEntry>,
    new_hashes: Vec<u64>,
    news: Vec<TypeRef>,
    existings: HashMap<TypeID, TypeRef>,
  }

  impl TypeMap {
    pub fn create_entry (&mut self, entry: TypeEntry) -> Result<TypeRef, String> {
      if self.types.len() < TypeRef::MAX_TYPE_REFS as usize {
        let new_ref = TypeRef(self.types.len() as _);
        
        self.types.push(entry);

        Ok(new_ref)
      } else {
        Err(format!("Cannot bind more than {} types in a module", TypeRef::MAX_TYPE_REFS))
      }
    }

    fn hash_ty (ty: &Ty) -> u64 {
      let mut hasher = DefaultHasher::default();

      ty.hash(&mut hasher);

      hasher.finish()
    }

    pub fn new_ty (&mut self, ty: Ty) -> Result<TypeRef, String> {
      let hash = Self::hash_ty(&ty);

      for &tref in self.news.iter() {
        // Safety: the only way to create a TypeRef is thru TypeMap's interface, so indices inside are always valid
        let (&existing_hash, existing_ty) = unsafe {(
          self.new_hashes.get_unchecked(tref.0 as usize),
          self.types.get_unchecked(tref.0 as usize)
        )};

        // Safety: the typeref we used to find this entry came from self.news, which only contains refs to New entries
        unsafe { unchecked_destructure! {
          existing_ty,
          TypeEntry::New(existing_ty) => {
            if hash == existing_hash
            && existing_ty == &ty {
              return Ok(tref)
            }
          }
        } }
      }

      let tref = self.create_entry(TypeEntry::New(ty))?;
      self.new_hashes.push(hash);
      Ok(tref)
    }

    pub fn existing_ty (&mut self, id: TypeID) -> Result<TypeRef, String> {
      Ok(if let Some(&tref) = self.existings.get(&id) { tref } else {
        let tref = self.create_entry(TypeEntry::Existing(id))?;
        self.existings.insert(id, tref);
        tref
      })
    }

    pub fn undefined_ty (&mut self) -> Result<TypeRef, String> {
      self.create_entry(TypeEntry::Undefined)
    }

    pub fn get (&self, tref: TypeRef) -> &TypeEntry {
      // Safety: the only way to create a TypeRef is thru TypeMap's interface, so indices inside are always valid
      unsafe { self.types.get_unchecked(tref.0 as usize) }
    }

    pub fn get_mut (&mut self, tref: TypeRef) -> &mut TypeEntry {
      // Safety: the only way to create a TypeRef is thru TypeMap's interface, so indices inside are always valid
      unsafe { self.types.get_unchecked_mut(tref.0 as usize) }
    }
    
  }
}

use type_map::*;


struct Analyzer<'c> {
  ctx: &'c mut Context,
  module: Module,
  top_syms: HashMap<String, Symbol>,
  type_map: TypeMap,
}



macro_rules! err {
  ($loc:expr, $fmt:literal $(, $($params:expr),* $(,)?)?) => {
    Err(err_data!($loc, $fmt $(, $($params),*)?))
  };
}

macro_rules! err_data {
  ($loc:expr, $fmt:literal $(, $($params:expr),* $(,)?)?) => {
    AnalysisErr { info: format!($fmt $(, $($params),*)?), loc: $loc }
  };
}

impl<'c> Analyzer<'c> {
  fn new (ctx: &'c mut Context) -> Self {
    Self {
      ctx,
      module: Module::default(),
      top_syms: HashMap::default(),
      type_map: TypeMap::default()
    }
  }

  fn bind (&mut self, key: &str, data: SymbolData, loc: Loc) -> Result<(), AnalysisErr> {
    if let Some(existing_sym) = self.top_syms.get(key) {
      err!(loc, "`{}` shadows existing symbol (originally bound at [{}])", key, existing_sym.loc)
    } else {
      self.top_syms.insert(key.to_owned(), Symbol { data, loc });
      Ok(())
    }
  }
}



/// An error that resulted from analysis of a source module
#[derive(Debug)]
#[allow(missing_docs)]
pub struct AnalysisErr {
  pub info: String,
  pub loc: Loc
}

fn add_err_loc (loc: Loc) -> impl FnOnce (String) -> AnalysisErr {
  move |info| AnalysisErr { info, loc }
}



fn analyze<'c, 'i> (ctx: &'c mut Context, items: &mut [Item<'i>]) -> Result<Module, AnalysisErr> {
  let mut az = Analyzer::new(ctx);

  bind_top_level(&mut az, items)?;
  make_typedefs(&mut az, items)?;

  Ok(az.module)
}



fn build_ty<'c, 't> (az: &mut Analyzer<'c>, texpr: &TyExpr<'t>) -> Result<TypeRef, AnalysisErr> {
  match &texpr.data {
    TyExprData::Nil => {
      Ok(
        az.type_map
          .existing_ty(TypeID::from_primitive(PrimitiveType::Nil))
          .map_err(add_err_loc(texpr.loc))?
      )
    }

    TyExprData::Identifier(name) => {
      let sym = if let Some(sym) = az.top_syms.get(*name) { sym } else {
        return err!(texpr.loc, "Undefined symbol `{}` in type expression", name)
      };
      
      if let SymbolData::Type(tref) = sym.data {
        Ok(tref)
      } else {
        err!(texpr.loc, "`{}` does not name a type (Item is defined at [{}])", name, sym.loc)
      }
    }

    TyExprData::Path(root, sub) => {
      let sym = if let Some(sym) = az.top_syms.get(*root) { sym } else {
        return err!(texpr.loc, "Module `{}` has not been imported", root)
      };

      let mid = if let SymbolData::Module(mid) = sym.data { mid } else {
        return err!(texpr.loc, "`{}` does not name a module (Item is defined at [{}])", root, sym.loc)
      };

      // Safety: We imported this module in order to have a mid, so it must still be valid
      let module = unsafe { az.ctx.modules.get_module_unchecked(mid) };

      let binding = if let Some(binding) = module.find_binding(*sub) { binding } else {
        return err!(texpr.loc, "Module `{}` does not export an item named `{}`", root, sub)
      };

      match binding {
        ModuleBinding::Global(_) => err!(texpr.loc, "`{}::{}` does not name a type", root, sub),
        ModuleBinding::Type(tid) => {
          az.type_map
            .existing_ty(tid)
            .map_err(add_err_loc(texpr.loc))
        }
      }
    }

    TyExprData::Record(fields) => {
      // TODO: cache/temp storage?
      let mut locs: HashMap<&str, Loc> = HashMap::default();
      let mut names = vec![];
      let mut tys = vec![];

      for &ElementDecl { data: (name, ref texpr), loc } in fields.iter() {
        if let Some(existing_loc) = locs.get(name) {
          return err!(loc, "Redundant field name: `{}` is already defined at [{}]", name, existing_loc)
        } else {
          locs.insert(name, loc);
          names.push(name.to_owned());
          tys.push(build_ty(az, texpr)?);
        }
      }

      let ty = Ty::Record(names, tys);

      az.type_map
        .new_ty(ty)
        .map_err(add_err_loc(texpr.loc))
    }

    TyExprData::Array(elem_texpr) => {
      let ty = Ty::Array(build_ty(az, elem_texpr)?);

      az.type_map
        .new_ty(ty)
        .map_err(add_err_loc(texpr.loc))
    }

    TyExprData::Map(key_texpr, val_texpr) => {
      let ty = Ty::Map(
        build_ty(az, key_texpr)?,
        build_ty(az, val_texpr)?
      );

      az.type_map
        .new_ty(ty)
        .map_err(add_err_loc(texpr.loc))
    }

    TyExprData::Function(params, result) => {
      let mut param_tys = vec![];

      for texpr in params {
        param_tys.push(build_ty(az, texpr)?);
      }

      let result_ty = if let Some(result) = result.as_deref() {
        Some(build_ty(az, result)?)
      } else {
        None
      };

      let ty = Ty::Function(param_tys, result_ty);

      az.type_map
        .new_ty(ty)
        .map_err(add_err_loc(texpr.loc))
    }
  }
}


macro_rules! passes {
  ($pass_name:ident ($az:ident, $item:ident @ $loc:ident) => $body:block $($rest:tt)*) => {
    fn $pass_name<'c, 'i> (az: &mut Analyzer<'c>, items: &mut [Item<'i>]) -> Result<(), AnalysisErr> {
      #[allow(unused_variables)]
      fn this<'c, 'i> ($az: &mut Analyzer<'c>, $item: &mut Item<'i>) -> Result<(), AnalysisErr> {
        let $loc = $item.loc;

        $body;

        Ok(())
      }

      for item in items.iter_mut() {
        this(az, item)?;
      }

      Ok(())
    }

    passes!($($rest)*);
  };
  ($(,)?) => {};
}

passes! {
  bind_top_level (az, item @ loc) => {
    match &mut item.data {
      ItemData::Export(inner) => return this(az, inner),

      ItemData::Import(root, sub) => {
        let mid = if let Some(mid) = az.ctx.modules.find_module(root) { mid } else {
          return err!(loc, "No module named `{}` is available", root)
        };

        let (key, data) = if let Some(sub) = sub.as_deref() {
          // Safety: We imported this module in order to have a mid, so it must still be valid
          let module = unsafe { az.ctx.modules.get_module_unchecked(mid) };

          let binding = if let Some(binding) = module.find_binding(sub) { binding } else {
            return err!(loc, "Module `{}` does not export an item named `{}`", root, sub)
          };

          let data = match binding {
            ModuleBinding::Global(gid) => SymbolData::Global(gid),
            ModuleBinding::Type(tid) => {
              let tref = {
                az.type_map
                  .existing_ty(tid)
                  .map_err(add_err_loc(loc))?
              };

              SymbolData::Type(tref)
            }
          };

          (sub, data)
        } else {
          (*root, SymbolData::Module(mid))
        };

        az.bind(key, data, loc)?;
      }
      
      ItemData::Type(name, _) => {
        let tref = {
          az.type_map
            .undefined_ty()
            .map_err(add_err_loc(loc))?
        };

        az.bind(name, SymbolData::Type(tref), loc)?;
      }

      | ItemData::Global(name, ..)
      | ItemData::Function(name, ..)
      => {
        let (gid, _) = {
          az.module
            .create_global()
            .ok_or_else(|| err_data!(loc,
              "Cannot bind more than {} functions and globals in a module", GlobalID::MAX_GLOBALS
            ))?
        };

        az.bind(name, SymbolData::Global(gid), loc)?;
      }
    }
  }


  make_typedefs(az, item @ loc) => {
    match &mut item.data {
      ItemData::Export(inner) => return this(az, inner),

      ItemData::Type(name, texpr) => {
        let tref = build_ty(az, texpr)?;

        let sym = az.top_syms.get(*name).unwrap();

        if let SymbolData::Type(existing_tref) = sym.data {
          let entry = az.type_map.get_mut(existing_tref);
          debug_assert!(matches!(entry, TypeEntry::Undefined));
          *entry = TypeEntry::Redirect(tref)
        } else {
          unreachable!()
        }
      }

      // Only processing type defs here??
      | ItemData::Import { .. }
      | ItemData::Global { .. }
      | ItemData::Function { .. }
      => { }
    }
  }
}