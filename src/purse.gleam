import gleam/dynamic
import gleam/erlang/atom
import gleam/io
import purse/core.{type Key, type PurseError, type Table, type TableName}
import purse/ets
import purse/dets
import purse/common/named

// I am going to go with a builder patterns like so
// new_builder(Name)
// |> set_access(SomeAccessThing)
// |> set_type(SomeTypeThing)
// ...
// |> create
// for this, I would need to also filter out the options in each category to make sure that even if you set the same option twice, it only gets set once by picking the last one
//
//  but also with an option to do it all in one go if you want i.e. purse.new(name, [options...])

// TODO: DELETE
pub type Data {
  Data(key: String, value: String, point: Int)
}

// TODO: DELETE
pub fn main() {
  let decoder =
    dynamic.decode3(
      Data,
      dynamic.element(at: 1, of: dynamic.string),
      dynamic.element(at: 2, of: dynamic.string),
      dynamic.element(at: 3, of: dynamic.int),
    )

  let table_name = atom.create_from_string("test")

  let assert Ok(t) =
    named.new(
      name: table_name,
      visibility: named.Public,
      table_type: named.Bag,
      accepts: decoder,
    )

  let _ = insert(t, string("foo"), Data("bar", "baz", 1))
  let _ = insert(t, string("foo"), Data("ayy", "bee", 2))
  let _ = insert(t, string("foo"), Data("cee", "dee", 3))

  lookup(t, string("foo"))
  |> io.debug

  Nil
}

pub const new_ets_table = ets.new

pub const new_dets_table = dets.new

pub const string = core.string

pub const int = core.int

pub const term = core.term

pub const atom = core.atom

/// Inserts a value into a table
pub fn insert(
  table table: Table(model),
  key key: Key(_),
  value value: model,
) -> Result(model, PurseError) {
  todo
}

/// Looks up a value in a table by key
pub fn lookup(
  table: Table(model),
  key key: Key(_),
) -> Result(List(model), PurseError) {
  todo
}

/// Deletes all objects with the given key from the table
pub fn delete(table: Table(_), key key: Key(_)) -> Result(Nil, PurseError) {
  todo
}

/// Drops a table, deleting all of its contents - this is irreversible
pub fn drop_table(table: Table(_)) -> Result(Nil, PurseError) {
  todo
}

/// Returns a list of all tables in the current node. This function returns a dynamic list because a table name is not guaranteed to be an atom, it could be a `tid()`
pub fn list_tables() -> Result(List(TableName), PurseError) {
  todo
}
