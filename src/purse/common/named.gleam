import gleam/erlang/atom.{type Atom}
import gleam/dynamic.{type Decoder}
import purse/core
import purse/ets

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
  name name: Atom,
  visibility visibility: Visibility,
  table_type type_: TableType,
  accepts decoder: Decoder(b),
) -> Result(core.Table(b), core.PurseError) {
  ets.new(
    name: name,
    options: [ets.NamedTable, to_visibility(visibility), to_table_type(type_)],
    accepts: decoder,
  )
}

fn to_visibility(visibility: Visibility) -> ets.TableOptions {
  case visibility {
    Protected -> ets.Protected
    Public -> ets.Public
    Private -> ets.Private
  }
}

fn to_table_type(type_: TableType) -> ets.TableOptions {
  case type_ {
    Set -> ets.Set
    OrderedSet -> ets.OrderedSet
    Bag -> ets.Bag
    DuplicateBag -> ets.DuplicateBag
  }
}
