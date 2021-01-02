//! Contains shared contextual information for all VM Fibers

use super::{
  TypeID,
  TypeRegistry,
  ModuleRegistry,
  typeinfo
};

/// Contains shared contextual information for all VM Fibers
#[derive(Debug)]
pub struct Context {
	/// Storage for all types in the VM
	pub types: TypeRegistry,
	/// Storage for all modules in the VM
	pub modules: ModuleRegistry,
}



pub(crate) const BUILTIN_TYPE_ENTRIES: &[(&str, TypeID)] = {
	use TypeID as ID;

	{
		use typeinfo::PrimitiveType::*;

		&[
			("nil", ID::from_primitive(Nil)),
			("float", ID::from_primitive(Real)),
			("int", ID::from_primitive(Integer)),
			("char", ID::from_primitive(Character)),
			("bool", ID::from_primitive(Boolean)),
			("typeid", ID::from_primitive(TypeID)),
			("string", ID::from_primitive(String)),
		]
	}
};


impl Context {
	/// Create a new Context
	pub fn new () -> Self {
		let mut out = Self {
			types: TypeRegistry::default(),
			modules: ModuleRegistry::default()
		};
		
		let (_, core_mod) = out.modules.create_module("[core]").unwrap();

		for &(name, id) in BUILTIN_TYPE_ENTRIES {
			let _ok = core_mod.export_type(name, id);
			debug_assert!(_ok);
		}

		out
	}
}

impl Default for Context { fn default () -> Self { Self::new() } }