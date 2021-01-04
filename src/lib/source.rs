//! Contains source tracking structures

use std::{
  rc::Rc,
  cmp::Ordering,
  collections::HashMap,
  path::{ Path, PathBuf },
  fmt, fs, io,
};


/// A unique identifier for a Source loaded in a SourceRegistry
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct SourceID(u32);

impl SourceID {
  /// The maximum number of Sources that can be stored in a SourceRegistry
  pub const MAX_SOURCES: u32 = u32::MAX;
}

/// A unit of source code, corresponding to a file
#[derive(Debug)]
pub struct Source {
  id: SourceID,
  path: Rc<Path>,
  content: String,
  lines: Vec<Pos>
}

/// A cache of Sources
#[derive(Debug, Default)]
pub struct SourceRegistry {
  sources: HashMap<SourceID, Source>,
  path_to_id: HashMap<Rc<Path>, SourceID>,
  id_counter: SourceID
}

/// A byte offset in a Source
pub type Pos = u32;

/// A byte offset in a specific Source
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Loc(pub SourceID, pub Pos);

/// A start and end byte offset in a specific Source
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Range(pub SourceID, pub (Pos, Pos));

/// A generic line and column number
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LineCol(pub Pos, pub Pos);

/// A line and column number and the path of the source it originated from
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributionLoc<'a> (&'a Path, LineCol);

/// Start and end line and column numbers and the path of the source they originated from
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AttributionRange<'a> (&'a Path, (LineCol, LineCol));


impl Source {
  fn new (id: SourceID, path: Rc<Path>, content: String) -> Self {
    let mut lines = vec![0];
    
    let mut iter = content.char_indices();
    if let Some((_, mut last_ch)) = iter.next() {
      for (i, ch) in iter {
        if last_ch == '\n' { lines.push(i as Pos); }
        last_ch = ch;
      }
    }

    Self {
      id,
      path,
      content,
      lines
    }
  }

  /// Get the unique SourceID of a Source
  pub fn id (&self) -> SourceID {
    self.id
  }

  /// Get the unique Path a Source originated from
  pub fn path (&self) -> &Path {
    &self.path
  }

  /// Get the string content of a Source
  pub fn content (&self) -> &str {
    &self.content
  }

  /// Get the list of byte indices each line starts at in a Source
  pub fn line_indices (&self) -> &[Pos] {
    &self.lines
  }

  /// Get the length of the string content of a Source
  pub fn len (&self) -> usize {
    self.content.len()
  }

  /// Determine if a Source has any string content
  pub fn is_empty (&self) -> bool {
    self.content.is_empty()
  }

  /// Get an iterator over the `char`s in the string content of a Source
  pub fn chars (&self) -> std::str::Chars<'_> {
    self.content.chars()
  }

  /// Get an iterator over the `char`s and their associated byte indices in the string content of a Source
  pub fn char_indices (&self) -> std::str::CharIndices<'_> {
    self.content.char_indices()
  }

  /// Convert a byte index to a line number and get the byte index of the start of the line
  ///
  /// Note that this does not validate byte indices:
  /// indices past the end of the buffer will be attributed to the last line
  pub fn get_line_and_idx (&self, pos: Pos) -> (Pos, Pos) {
    fn bin_search (slice: &[Pos], value: &Pos) -> usize {
      fn search (slice: &[Pos], l: usize, r: usize, x: &Pos) -> usize {
        if r > l {
          let mid = l + (r - l) / 2;
    
          match slice[mid].cmp(x) {
            Ordering::Equal => mid,
            Ordering::Greater => search(slice, l, mid - 1, x),
            Ordering::Less => search(slice, mid + 1, r, x)
          }
        } else {
          r
        }
      };
    
      search(slice, 0, slice.len() - 1, value)
    }

    let line = bin_search(&self.lines, &pos);

    (line as Pos, self.lines[line])
  }

  /// Convert a byte index to a line and column number
  ///
  /// Note that this does not validate byte indices:
  /// indices past the end of the buffer will be attributed to the last line
  pub fn get_line_col (&self, pos: Pos) -> LineCol {
    let (line, line_start) = self.get_line_and_idx(pos);
    let column = pos - line_start;
  
    LineCol(line, column)
  }
}


impl SourceRegistry {
  /// Create a new SourceRegistry
  pub fn new () -> Self { Self::default() }


  /// Load a Source from file
  ///
  /// Returns `io::ErrorKind::AlreadyExists` if the given path is already loaded
  pub fn load_src (&mut self, path: PathBuf) -> io::Result<&Source> {
    if let Some(existing_id) = self.path_to_id.get(path.as_path()) {
      return Ok(self.sources.get(existing_id).unwrap())
    }

    if self.id_counter.0 == SourceID::MAX_SOURCES {
      return Err(io::Error::from(io::ErrorKind::AlreadyExists))
    }
    
    let content = fs::read_to_string(path.as_path())?;

    let id = self.id_counter;
    self.id_counter.0 += 1;

    let path: Rc<Path> = path.into();

    self.path_to_id.insert(path.clone(), id);

    Ok(
      self.sources
        .entry(id)
        .or_insert_with(|| Source::new(id, path, content))
    )
  }

  /// Load a Source from memory
  ///
  /// Returns `None` if the give path is already loaded
  pub fn add_src (&mut self, path: PathBuf, content: String) -> Option<&Source> {
    if let Some(existing_id) = self.path_to_id.get(path.as_path()) {
      return Some(self.sources.get(existing_id).unwrap())
    }

    if self.id_counter.0 == SourceID::MAX_SOURCES {
      return None
    }

    let id = self.id_counter;
    self.id_counter.0 += 1;

    let path: Rc<Path> = path.into();

    self.path_to_id.insert(path.clone(), id);

    Some(
      self.sources
        .entry(id)
        .or_insert_with(|| Source::new(id, path, content))
    )
  }


  /// Add a Source from memory
  ///
  /// If the given path is already loaded, this will unload it first,
  /// invalidating any existing references with the old SourceID
  pub fn force_add_src (&mut self, path: PathBuf, content: String) -> Option<&Source> {
    if let Some(&existing_id) = self.path_to_id.get(path.as_path()) {
      self.remove_src(existing_id)
    }

    self.add_src(path, content)
  }

  /// Load a Source from file
  ///
  /// If the given path is already loaded, this will unload it first,
  /// invalidating any existing references with the old SourceID
  pub fn force_load_src (&mut self, path: PathBuf) -> io::Result<&Source> {
    if let Some(&existing_id) = self.path_to_id.get(path.as_path()) {
      self.remove_src(existing_id)
    }

    self.load_src(path)
  }


  /// Unload a Source, invalidating any existing references with the SourceID
  pub fn remove_src (&mut self, id: SourceID) {
    if let Some(src) = self.sources.remove(&id) {
      self.path_to_id.remove(src.path.as_ref());
    }
  }

  /// Get the number of Sources currently loaded
  pub fn len (&self) -> usize {
    self.sources.len()
  }

  /// Determine if there are any Sources currently loaded
  pub fn is_empty (&self) -> bool {
    self.sources.is_empty()
  }


  /// Find an existing Source from its path
  pub fn find_src_path<P: AsRef<Path>> (&self, path: P) -> Option<&Source> {
    self.get_src(*self.path_to_id.get(path.as_ref())?)
  }

  /// Determine if a given path is currently loaded as a Source
  pub fn has_src_path<P: AsRef<Path>> (&self, path: P) -> bool {
    self.find_src_path(path).is_some()
  }


  /// Get a Source by ID
  pub fn get_src (&self, id: SourceID) -> Option<&Source> {
    self.sources.get(&id)
  }


  /// Convert a Loc into an AttributionLoc for user-facing display
  pub fn attribute_loc (&self, loc: Loc) -> Option<AttributionLoc<'_>> {
    self.get_src(loc.0).map(|src| AttributionLoc(src.path.as_ref(), src.get_line_col(loc.1)))
  }

  /// Convert a Range into an AttributionRange for user-facing display
  pub fn attribute_range (&self, range: Range) -> Option<AttributionRange<'_>> {
    self.get_src(range.0).map(|src| AttributionRange(src.path.as_ref(), (src.get_line_col(range.1.0), src.get_line_col(range.1.1))))
  }
}


impl fmt::Display for LineCol {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}", self.0 + 1, self.1 + 1)
  }
}

impl<'a> fmt::Display for AttributionLoc<'a> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}", self.0.display(), self.1)
  }
}

impl<'a> fmt::Display for AttributionRange<'a> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{} to {}", self.0.display(), self.1.0, self.1.1)
  }
}