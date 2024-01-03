import purse/core

pub type Visibility {
  // Read available to all processes, write available to owner - default
  Protected

  /// Read/Write available to all processes
  Public

  /// Read/Write available to owner ONLY
  Private
}

pub type TableType {
  // One value per key, keys are unique - default
  Set

  /// One value per key, similar to `Set` but items are ordered by term (not insertion order, e.g :a before :b).
  /// Keys also do need need to match for retrieval or other ops as long as they compare equally e.g 1 == 1.0
  OrderedSet

  /// Many objects per key but only one instance of each object per key
  Bag

  /// Similar to `Bag` but duplicates are allowed
  DuplicateBag
}

/// Creates a new named table with the given name, visibility and type.
///
/// If you want more control over the table options, use `core.new` directly (re-exported as `purse.new`)
pub fn new(
  name name: a,
  visibility visibility: Visibility,
  table_type type_: TableType,
) -> a {
  core.new(name, [
    core.NamedTable,
    to_visibility(visibility),
    to_table_type(type_),
  ])
}

fn to_visibility(visibility: Visibility) -> core.TableOptions(_) {
  case visibility {
    Protected -> core.Protected
    Public -> core.Public
    Private -> core.Private
  }
}

fn to_table_type(type_: TableType) -> core.TableOptions(_) {
  case type_ {
    Set -> core.Set
    OrderedSet -> core.OrderedSet
    Bag -> core.Bag
    DuplicateBag -> core.DuplicateBag
  }
}
