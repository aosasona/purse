import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}

pub type TableName {
  /// A table identifier
  Tid(Dynamic)

  // this is not a great name but I do not want it to clash with the erlang/atom module's type
  /// An atom representing a (named) table's name
  Named(Atom)
}
