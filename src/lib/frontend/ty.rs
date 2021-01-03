//! Contains the frontend's representation of types

use crate::vm::TypeID;

/// Similar to a TypeID, but represents a temporary binding in the frontend
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRef(pub(crate) u16);

impl TypeRef {
	/// The maximum number of TypeRefs storable by a TypeMap
	pub const MAX_TYPE_REFS: u16 = u16::MAX;
}

/// Similar to TypeInfo but stores user types temporarily in the frontend
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum Ty {
	Array(TypeRef),
	Map(TypeRef, TypeRef),
	Record(Vec<String>, Vec<TypeRef>),
	Function(Vec<TypeRef>, Option<TypeRef>),
}

/// An entry in a TypeMap, allowing bindings to other TypeEntrys, existing TypeIDs, or new Tys
#[derive(Debug)]
pub enum TypeEntry {
	/// Binds a type in the Context
	Existing(TypeID),
	/// Binds a newly analyzed type in the frontend
	New(Ty),
	/// Binds another type in its TypeMap
	Redirect(TypeRef),
	/// Represents an empty slot where a type will be filled in later
	Undefined,
}

impl TypeEntry {
	/// Traverse any interior TypeEntry::Redirects to resolve the actual data
	pub fn resolve_redirects<'a> (&'a self, type_map: &'a TypeMap) -> &'a TypeEntry {
		if let &TypeEntry::Redirect(next_ref) = self {
			type_map.get(next_ref).resolve_redirects(type_map)
		} else {
			self
		}
	}
}


/// Similar to TypeRegistry, but for temporary storage in the frontend
#[derive(Default, Debug)]
pub struct TypeMap {
	pub(crate) types: Vec<TypeEntry>,
}

impl TypeMap {
	/// Create a new TypeMap
	pub fn new () -> Self { Self::default() }

	/// Iterator over all valid references into a TypeMap
	pub fn ref_iter (&self) -> std::iter::Map<std::ops::Range<u16>, impl FnMut (u16) -> TypeRef> {
		(0..self.types.len() as u16).map(TypeRef)
	}
	
	/// Bind a new TypeEntry to a TypeRef
	pub fn create_entry (&mut self, entry: TypeEntry) -> Result<TypeRef, String> {
		if self.types.len() < TypeRef::MAX_TYPE_REFS as usize {
			let new_ref = TypeRef(self.types.len() as _);
			
			self.types.push(entry);

			Ok(new_ref)
		} else {
			Err(format!("Cannot bind more that {} types in a Module", TypeRef::MAX_TYPE_REFS))
		}
	}
	
	pub(crate) fn clear (&mut self) {
		self.types.clear();
	}

	
	/// Create a binding for a new Ty and get a TypeRef to it
	pub fn new_ty (&mut self, ty: Ty) -> Result<TypeRef, String> {
		let tref = self.create_entry(TypeEntry::New(ty))?;
		Ok(tref)
	}

	/// Create a new binding for an existing TypeID and get a TypeRef to it
	pub fn existing_ty (&mut self, id: TypeID) -> Result<TypeRef, String> {
		self.create_entry(TypeEntry::Existing(id))
	}

	/// Create a binding to an empty TypeEntry and get a TypeRef to it
	pub fn undefined_ty (&mut self) -> Result<TypeRef, String> {
		self.create_entry(TypeEntry::Undefined)
	}

	/// Convert a TypeRef to an immutable reference to its TypeEntry
	pub fn get (&self, tref: TypeRef) -> &TypeEntry {
		// Safety: the only safe way to create a TypeRef is thru TypeMap's interface, so indices inside are presumed always valid
		unsafe { self.types.get_unchecked(tref.0 as usize) }
	}

	/// Convert a TypeRef to a mutable reference to its TypeEntry
	pub fn get_mut (&mut self, tref: TypeRef) -> &mut TypeEntry {
		// Safety: the only safe way to create a TypeRef is thru TypeMap's interface, so indices inside are presumed always valid
		unsafe { self.types.get_unchecked_mut(tref.0 as usize) }
	}
}