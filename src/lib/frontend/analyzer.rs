//! Analysis system

#![allow(dead_code, missing_docs)]

use std::{
	collections::{
		hash_map::{
			// DefaultHasher,
			HashMap,
			// RawEntryMut
		}
	}, fmt, hash::{
		Hash,
		// Hasher
	},
	mem,
};

use super::{
	common::*,
	ast::{ Item, ItemData, TyExpr, TyExprData, ElementDecl },
};

use crate::{
	utils::TryCollectVec,
	vm::{
		Context, GlobalID, Module, ModuleID, TypeID, TypeInfo, TypeRegistry,
		context::BUILTIN_TYPE_ENTRIES,
		module::ModuleBinding,
		typeinfo::{
			FunctionKind,
			PrimitiveType
		}
	}
};


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolData {
	Module(ModuleID),
	Type(TypeRef),
	Global(GlobalID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
	pub data: SymbolData,
	pub loc: Option<Loc>
}


mod type_map {
	use super::*;

	#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
	pub struct TypeRef(pub(crate) u16);

	impl TypeRef {
		pub const MAX_TYPE_REFS: u16 = u16::MAX;
	}

	#[derive(Debug, Clone)]
	pub enum Ty {
		Array(TypeRef),
		Map(TypeRef, TypeRef),
		Record(Vec<String>, Vec<TypeRef>),
		Function(Vec<TypeRef>, Option<TypeRef>),
	}

	#[derive(Debug)]
	pub enum TypeEntry {
		Existing(TypeID),
		New(Ty),
		Redirect(TypeRef),
		Undefined,
	}

	impl TypeEntry {
		pub fn resolve_redirects<'a> (&'a self, az: &'a Analyzer<'a>) -> &'a TypeEntry {
			if let &TypeEntry::Redirect(next_ref) = self {
				az.type_map.get(next_ref).resolve_redirects(az)
			} else {
				self
			}
		}
	}


	#[derive(Default, Debug)]
	pub struct TypeMap {
		pub types: Vec<TypeEntry>,
	}

	impl TypeMap {
		pub fn new () -> Self {
			let mut s = Self::default();
			s.load_builtins();
			s
		}
		
		pub fn create_entry (&mut self, entry: TypeEntry) -> Result<TypeRef, String> {
			if self.types.len() < TypeRef::MAX_TYPE_REFS as usize {
				let new_ref = TypeRef(self.types.len() as _);
				
				self.types.push(entry);

				Ok(new_ref)
			} else {
				Err(format!("Cannot bind more than {} types in a module", TypeRef::MAX_TYPE_REFS))
			}
		}
		
		fn load_builtins (&mut self) {
			for &(_, id) in BUILTIN_TYPE_ENTRIES {
				self.types.push(TypeEntry::Existing(id));
			}
		}

		pub(super) fn clear (&mut self) {
			self.types.clear();
			self.load_builtins();
		}

		
		pub fn new_ty (&mut self, ty: Ty) -> Result<TypeRef, String> {
			let tref = self.create_entry(TypeEntry::New(ty))?;
			Ok(tref)
		}

		pub fn existing_ty (&mut self, id: TypeID) -> Result<TypeRef, String> {
			self.create_entry(TypeEntry::Existing(id))
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

pub use type_map::*;


/// Stores contextual information specific to a semantic analysis run
#[derive(Debug)]
pub struct Analyzer<'c> {
	pub ctx: &'c mut Context,
	pub module: Module,
	pub top_syms: HashMap<String, Symbol>,
	pub type_map: TypeMap,
}



macro_rules! err {
	($loc:expr, $fmt:literal $(, $($params:expr),* $(,)?)?) => {
		Err(err_data!($loc, $fmt $(, $($params),*)?))
	};

	($fmt:literal $(, $($params:expr),* $(,)?)?) => {
		Err(err_data!($fmt $(, $($params),*)?))
	};
}

macro_rules! err_data {
	($loc:expr, $fmt:literal $(, $($params:expr),* $(,)?)?) => {
		AnalysisErr { data: format!($fmt $(, $($params),*)?), loc: Some($loc) }
	};

	($fmt:literal $(, $($params:expr),* $(,)?)?) => {
		AnalysisErr { data: format!($fmt $(, $($params),*)?), loc: None }
	};
}

impl<'c> Analyzer<'c> {
	/// Create a new semantic analyzer
	pub fn new (ctx: &'c mut Context) -> Self {
		let mut out = Self {
			ctx,
			module: Module::default(),
			top_syms: HashMap::default(),
			type_map: TypeMap::default()
		};
		out.load_builtins();
		out
	}

	fn load_builtins (&mut self) {
		for (i, &(name, _)) in BUILTIN_TYPE_ENTRIES.iter().enumerate() {
			self.top_syms.insert(name.to_owned(), Symbol { data: SymbolData::Type(TypeRef(i as _)), loc: None });
		}
	}

	fn clear (&mut self) {
		self.top_syms.clear();
		self.type_map.clear();
		self.load_builtins()
	}

	/// Run a semantic analyzer over a given ast
	pub fn analyze (&mut self, items: &mut [Item<'_>]) -> AnalysisResult<Module> {
		self.clear();

		bind_top_level(self, items)?;

		make_typedefs(self, items)?;


		finalize_types(self)?;
		
		Ok(mem::take(&mut self.module))
	}

	fn bind (&mut self, key: &str, data: SymbolData, loc: Loc) -> AnalysisResult {
		if let Some(existing_sym) = self.top_syms.get(key) {
			if let Some(existing_loc) = existing_sym.loc {
				err!(loc, "`{}` shadows existing symbol (originally bound at [{}])", key, existing_loc)
			} else {
				err!(loc, "`{}` shadows existing symbol", key)
			}
		} else {
			self.top_syms.insert(key.to_owned(), Symbol { data, loc: Some(loc) });
			Ok(())
		}
	}
}



/// An error that resulted from analysis of a source module
#[derive(Debug)]
#[allow(missing_docs)]
pub struct AnalysisErr {
	pub data: String,
	pub loc: Option<Loc>
}

/// The result of analyzing a source module
pub type AnalysisResult<T = ()> = Result<T, AnalysisErr>;

fn add_err_loc (loc: Loc) -> impl FnOnce (String) -> AnalysisErr {
	move |data| AnalysisErr { data, loc: Some(loc) }
}


/// Display wrapper for formatting analyzer errors, produced by `AnalysisErr::display`
pub struct AnalysisErrDisplay<'f>(&'f str, AnalysisErr);

impl<'f> fmt::Display for AnalysisErrDisplay<'f> {
	fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self(file_name, AnalysisErr { data, loc: Some(Loc { line, column, .. }) }) 
			=> write!(f, "Error at [{}:{}:{}]: {}", file_name, line + 1, column + 1, data),

			Self(file_name, AnalysisErr { data, loc: None }) 
			=> write!(f, "Error in [{} (Location not provided, possibly at EOF)]: {}", file_name, data)
		}
	}
}

impl AnalysisErr {
	/// Formats an AnalysisErr for user-facing display
	pub fn display (self, file_name: &str) -> AnalysisErrDisplay {
		AnalysisErrDisplay(file_name, self)
	}
}


/// Perform semantic analysis on top level ast items, producing a Module
pub fn analyze (ctx: &mut Context, items: &mut [Item<'_>]) -> AnalysisResult<Module> {
	let mut az = Analyzer::new(ctx);

	az.analyze(items)
}





macro_rules! passes {
	($pass_name:ident ($az:ident, $item:ident @ $loc:ident) => $body:block $($rest:tt)*) => {
		fn $pass_name (az: &mut Analyzer, items: &mut [Item<'_>]) -> AnalysisResult {
			#[allow(unused_variables)]
			fn this ($az: &mut Analyzer, $item: &mut Item<'_>) -> AnalysisResult {
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
					// unreachable because shadowing would have already produced an error,
					// and the previous pass has produced the Undefined value already
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




fn build_ty (az: &mut Analyzer, texpr: &TyExpr<'_>) -> AnalysisResult<TypeRef> {
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
				err!(texpr.loc, "`{}` does not name a type", name)
			}
		}

		TyExprData::Path(root, sub) => {
			let sym = if let Some(sym) = az.top_syms.get(*root) { sym } else {
				return err!(texpr.loc, "Module `{}` has not been imported", root)
			};

			let mid = if let SymbolData::Module(mid) = sym.data { mid } else {
				return err!(texpr.loc, "`{}` does not name a module", root)
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





fn reduce_redirects (az: &Analyzer, base: TypeRef) -> TypeRef {
	if let &TypeEntry::Redirect(next_ref) = az.type_map.get(base) {
		reduce_redirects(az, next_ref)
	} else {
		base
	}
}

fn type_eq_impl<'a> (az: &'a Analyzer<'a>, stack: &mut Vec<(TypeRef, TypeRef)>, a: TypeRef, b: TypeRef) -> bool {
	fn type_eq_impl_n (az: &Analyzer, stack: &mut Vec<(TypeRef, TypeRef)>, a: &[TypeRef], b: &[TypeRef]) -> bool {
		for (a, b) in a.iter().zip(b.iter()) {
			if !type_eq_impl(az, stack, *a, *b) { return false }
		}
	
		true
	}

	fn compare_ty_to_ty (az: &Analyzer, stack: &mut Vec<(TypeRef, TypeRef)>, a: &Ty, b: &Ty) -> bool {
		match (a, b) {
			(Ty::Array(a), Ty::Array(b))
			=> type_eq_impl(az, stack, *a, *b),

			(Ty::Map(x, y), Ty::Map(i, j))
			=> type_eq_impl(az, stack, *x, *i) && type_eq_impl(az, stack, *y, *j),

			(Ty::Record(a_names, a_refs), Ty::Record(b_names, b_refs))
			=> a_names == b_names && type_eq_impl_n(az, stack, a_refs, b_refs),
			
			(Ty::Function(a_params, a_result), Ty::Function(b_params, b_result))
			=> {
				(match (a_result, b_result) {
					(Some(a), Some(b)) => type_eq_impl(az, stack, *a, *b),
					_ => false
				}) && type_eq_impl_n(az, stack, a_params, b_params)
			}

			_ => false
		}
	}

	fn compare_ty_to_typeinfo (az: &Analyzer, stack: &mut Vec<(TypeRef, TypeRef)>, ty: &Ty, info: &TypeInfo) -> bool {
		match (ty, info) {
			(Ty::Array(a), TypeInfo::Array(b))
			=> compare_type_ref_to_id(az, stack, *a, *b),
	
			(Ty::Map(x, y), TypeInfo::Map(i, j))
			=> compare_type_ref_to_id(az, stack, *x, *i) && compare_type_ref_to_id(az, stack, *y, *j),
	
			(Ty::Record(names, refs), TypeInfo::Record { field_names, field_types })
			=> names == field_names && compare_type_ref_to_id_n(az, stack, refs, field_types),
	
			(Ty::Function(param_refs, result_ref), TypeInfo::Function { kind, parameter_types, return_type })
			=> {
				*kind == FunctionKind::Free && match (result_ref, return_type) {
					(Some(a), Some(b)) => compare_type_ref_to_id(az, stack, *a, *b),
					_ => false
				} && compare_type_ref_to_id_n(az, stack, param_refs, parameter_types)
			}
	
			_ => false
		}
	}
	
	fn compare_type_ref_to_id (az: &Analyzer, stack: &mut Vec<(TypeRef, TypeRef)>, a: TypeRef, b: TypeID) -> bool {
		match az.type_map.get(a).resolve_redirects(az) {
			TypeEntry::Existing(a) => *a == b,
			TypeEntry::New(a) => compare_ty_to_typeinfo(az, stack, a, az.ctx.types.get_type(b).unwrap()),
	
			| TypeEntry::Redirect(_)
			| TypeEntry::Undefined
			=> unreachable!()
		}
	}
	
	fn compare_type_ref_to_id_n (az: &Analyzer, stack: &mut Vec<(TypeRef, TypeRef)>, a: &[TypeRef], b: &[TypeID]) -> bool {
		for (a, b) in a.iter().zip(b.iter()) {
			if !compare_type_ref_to_id(az, stack, *a, *b) { return false }
		}
	
		true
	}

	
	let a = reduce_redirects(az, a);
	let b = reduce_redirects(az, b);

	if a == b { return true }

	if stack.contains(&(a, b)) { return true }
	else { stack.push((a, b)) }
	
	let a = az.type_map.get(a);
	let b = az.type_map.get(b);

	let res = match (a, b) {
		(TypeEntry::Existing(a), TypeEntry::Existing(b)) => a == b,
		(TypeEntry::New(a), TypeEntry::New(b)) => compare_ty_to_ty(az, stack, a, b),

		| (TypeEntry::New(x), TypeEntry::Existing(y))
		| (TypeEntry::Existing(y), TypeEntry::New(x))
		=> compare_ty_to_typeinfo(az, stack, x, az.ctx.types.get_type(*y).unwrap()),

		| (TypeEntry::Undefined, _) | (_, TypeEntry::Undefined)
		| (TypeEntry::Redirect(_), _) | (_, TypeEntry::Redirect(_))
		=> unreachable!(),
	};

	stack.pop();

	res
}

fn type_eq_n (az: &Analyzer, a: &[TypeRef], b: &[TypeRef]) -> bool {
	let mut stack = vec![];

	for (a, b) in a.iter().zip(b.iter()) {
		if !type_eq_impl(az, &mut stack, *a, *b) { return false }
	}

	true
}

fn type_eq (az: &Analyzer, a: TypeRef, b: TypeRef) -> bool {
	let mut stack = vec![];
	type_eq_impl(az, &mut stack, a, b)
}






fn finalize_types (az: &mut Analyzer) -> AnalysisResult {
	struct IdentSys<'i> {
		type_map: &'i TypeMap,
		ctx_registry: &'i TypeRegistry,
		new_registry: TypeRegistry,
		ref_lookup_table: HashMap<TypeRef, TypeID>,
		id_lookup_table: HashMap<TypeID, TypeID>,
	}

	macro_rules! id_or_err {
		($expr:expr) => { if let Some(id) = $expr { id } else { return err!("Cannot register more than {} types in a Context", TypeID::MAX_TYPES) } }
	}

	fn copy_tinfo (
		in_registry: &TypeRegistry,
		out_registry: &mut TypeRegistry,
		id_lookup_table: &mut HashMap<TypeID, TypeID>,
		i: TypeID
	) -> AnalysisResult<TypeID> {
		if let Some(ni) = id_lookup_table.get(&i) {
			return Ok(*ni)
		} 

		let tinfo = in_registry.get_type(i).unwrap();

		if matches!(tinfo, TypeInfo::Primitive(_)) {
			id_lookup_table.insert(i, i);
			return Ok(i)
		}

		let ni = id_or_err!(out_registry.pre_register_type());

		id_lookup_table.insert(i, ni);

		match tinfo {
			TypeInfo::Array(ei) => {
				let nei = copy_tinfo(in_registry, out_registry, id_lookup_table, *ei)?;

				out_registry.define_type(ni, TypeInfo::Array(nei));
			}

			TypeInfo::Map(ki, vi) => {
				let nki = copy_tinfo(in_registry, out_registry, id_lookup_table, *ki)?;
				let nvi = copy_tinfo(in_registry, out_registry, id_lookup_table, *vi)?;

				out_registry.define_type(ni, TypeInfo::Map(nki, nvi));
			}

			TypeInfo::Userdata(name) => {
				out_registry.define_type(ni, TypeInfo::Userdata(name.to_owned()));
			}

			TypeInfo::Function { kind, parameter_types, return_type } => {
				let kind = *kind;
				let return_type = if let Some(i) = return_type { Some(copy_tinfo(in_registry, out_registry, id_lookup_table, *i)?) } else { None };
				let parameter_types = parameter_types.iter().map(|i| copy_tinfo(in_registry, out_registry, id_lookup_table, *i)).try_collect_vec()?;

				out_registry.define_type(ni, TypeInfo::Function { kind, parameter_types, return_type });
			}
			
			TypeInfo::Record { field_names, field_types } => {
				let field_names = field_names.clone();
				let field_types = field_types.iter().map(|i| copy_tinfo(in_registry, out_registry, id_lookup_table, *i)).try_collect_vec()?;
				
				out_registry.define_type(ni, TypeInfo::Record {
					field_names,
					field_types
				});
			}

			TypeInfo::Primitive(_) => unreachable!()
		};

		Ok(ni)
	}

	fn identify_types (az: &mut Analyzer) -> AnalysisResult<(TypeRegistry, HashMap<TypeRef, TypeID>)> {
		impl<'i> IdentSys<'i> {
			fn new (az: &'i mut Analyzer) -> Self {
				Self {
					type_map: &az.type_map,
					ctx_registry: &az.ctx.types,
					new_registry: TypeRegistry::default(),
					ref_lookup_table: HashMap::default(),
					id_lookup_table: HashMap::default(),
				}
			}

			fn copy_existing (&mut self, i: TypeID) -> AnalysisResult<TypeID> {
				copy_tinfo(&self.ctx_registry, &mut self.new_registry, &mut self.id_lookup_table, i)
			}
			

			fn identify_type (&mut self, r: TypeRef) -> AnalysisResult<TypeID> {
				if let Some(i) = self.ref_lookup_table.get(&r) {
					return Ok(*i)
				}

				let t = self.type_map.get(r);

				let i = match t {
					TypeEntry::Existing(i) => self.copy_existing(*i)?,

					TypeEntry::New(ty) => {
						let ni = id_or_err!(self.new_registry.pre_register_type());

						// CLEANUP this is a duplicate insert for the outer
						self.ref_lookup_table.insert(r, ni);

						match ty {
							Ty::Array(er) => {
								let ei = self.identify_type(*er)?;

								unsafe { self.new_registry.define_type_unchecked(ni, TypeInfo::Array(ei)) }
							}

							Ty::Map(kr, vr) => {
								let ki = self.identify_type(*kr)?;
								let vi = self.identify_type(*vr)?;

								unsafe { self.new_registry.define_type_unchecked(ni, TypeInfo::Map(ki, vi)) }
							}

							Ty::Function(param_rs, result_r) => {
								let parameter_types = param_rs.iter().map(|r| self.identify_type(*r)).try_collect_vec()?;
								let return_type = if let Some(r) = result_r { Some(self.identify_type(*r)?) } else { None };

								unsafe { self.new_registry.define_type_unchecked(ni, TypeInfo::Function {
									kind: FunctionKind::Free,
									return_type,
									parameter_types
								}) }
							}

							Ty::Record(field_names, field_rs) => {
								let field_names = field_names.clone();
								let field_types = field_rs.iter().map(|r| self.identify_type(*r)).try_collect_vec()?;
								
								unsafe { self.new_registry.define_type_unchecked (ni, TypeInfo::Record {
									field_names,
									field_types
								}) }
							}
						}

						ni
					}

					TypeEntry::Redirect(inner) => self.identify_type(*inner)?,

					TypeEntry::Undefined => unreachable!()
				};

				self.ref_lookup_table.insert(r, i);

				Ok(i)
			}
		}


		let range = 0..az.type_map.types.len() as u16;
		let mut id_sys = IdentSys::new(az);
		
		for i in range {
			let ty_ref = TypeRef(i);

			id_sys.identify_type(ty_ref)?;
		}

		Ok((
			id_sys.new_registry,
			id_sys.ref_lookup_table
		))
	}


	fn compare_tinfo (
		a_registry: &TypeRegistry,
		b_registry: &TypeRegistry,
		comparison_stack: &mut Vec<(TypeID, TypeID)>,
		a: TypeID, b: TypeID
	) -> bool {
		if comparison_stack.contains(&(a, b)) { return true }

		comparison_stack.push((a, b));

		let a = a_registry.get_type(a).unwrap();
		let b = b_registry.get_type(b).unwrap();

		let res = match (a, b) {
			(TypeInfo::Primitive(a), TypeInfo::Primitive(b))
			=> a == b,

			(TypeInfo::Array(a), TypeInfo::Array(b))
			=> compare_tinfo(a_registry, b_registry, comparison_stack, *a, *b),

			(TypeInfo::Map(ak, av), TypeInfo::Map(bk, bv))
			=> compare_tinfo(a_registry, b_registry, comparison_stack, *ak, *bk)
			&& compare_tinfo(a_registry, b_registry, comparison_stack, *av, *bv),

			(TypeInfo::Userdata(a), TypeInfo::Userdata(b))
			=> a == b,

			(TypeInfo::Record { field_types: a_field_types, field_names: a_field_names }
			,TypeInfo::Record { field_types: b_field_types, field_names: b_field_names })
			=> a_field_names == b_field_names
			&& compare_tinfo_n(a_registry, b_registry, comparison_stack, a_field_types, b_field_types),
			
			(TypeInfo::Function { kind: a_kind, return_type: a_return_type, parameter_types: a_parameter_types }
			,TypeInfo::Function { kind: b_kind, return_type: b_return_type, parameter_types: b_parameter_types })
			=> a_kind == b_kind
			&& match (a_return_type, b_return_type) {
				(Some(a), Some(b)) => compare_tinfo(a_registry, b_registry, comparison_stack, *a, *b),
				_ => false
			}
			&& compare_tinfo_n(a_registry, b_registry, comparison_stack, a_parameter_types, b_parameter_types),
			
			_ => false
		};

		comparison_stack.pop();

		res
	}

	fn compare_tinfo_n (
		a_registry: &TypeRegistry,
		b_registry: &TypeRegistry,
		comparison_stack: &mut Vec<(TypeID, TypeID)>,
		a: &[TypeID], b: &[TypeID]
	) -> bool {
		for (a, b) in a.iter().zip(b.iter()) {
			if !compare_tinfo(a_registry, b_registry, comparison_stack, *a, *b) { return false }
		}

		true
	}


	fn fold_identities (in_registry: TypeRegistry, out_registry: &mut TypeRegistry, map: &mut HashMap<TypeRef, TypeID>) -> AnalysisResult {
		let mut comparison_stack = Vec::<(TypeID, TypeID)>::default();
		let mut copied = HashMap::default();

		'map: for a in map.values_mut() {
			for b in out_registry.id_iter() {
				if compare_tinfo(&in_registry, &out_registry, &mut comparison_stack, *a, b) {
					copied.insert(*a, b);

					*a = b;
					
					continue 'map
				}
			}

			*a = copy_tinfo(&in_registry, out_registry, &mut copied, *a)?;
		}

		Ok(())
	}


	let (intermediate_registry, mut map) = identify_types(az)?;
	
	fold_identities(intermediate_registry, &mut az.ctx.types, &mut map)?;

	for (rf, id) in map.into_iter() {
		*az.type_map.get_mut(rf) = TypeEntry::Existing(id)
	}

	Ok(())
}