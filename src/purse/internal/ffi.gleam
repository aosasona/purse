import gleam/erlang/atom.{type Atom}
import gleam/dynamic.{type Dynamic}
import purse/core.{type TableName}

// ---- Non-persistent functions

@external(erlang, "purse_ffi", "insert")
pub fn insert(table: a, data: #(BitArray, b)) -> Result(b, Dynamic)

@external(erlang, "purse_ffi", "insert")
pub fn insert_many(
  table: a,
  data: #(BitArray, List(b)),
) -> Result(Dynamic, Dynamic)

@external(erlang, "purse_ffi", "lookup")
pub fn lookup(table: Atom, key: BitArray) -> Result(Dynamic, Dynamic)

@external(erlang, "purse_ffi", "delete")
pub fn delete(table: Atom, key: BitArray) -> Result(Nil, Dynamic)

// TODO: implement this - should be dynamic decoded into a list of _
@external(erlang, "purse_ffi", "all")
pub fn all(table: Atom) -> Result(List(Dynamic), Dynamic)

// TODO: implement this
@external(erlang, "purse_ffi", "rename_table")
pub fn rename_table(table: Atom, name: Atom) -> Result(Atom, Dynamic)

// TODO: implement this
@external(erlang, "purse_ffi", "purge_table")
pub fn purge_table(table: Atom) -> Result(Nil, Dynamic)

@external(erlang, "purse_ffi", "list_tables")
pub fn list_tables() -> Result(List(TableName), Dynamic)

@external(erlang, "purse_ffi", "drop_table")
pub fn drop_table(table: Atom) -> Result(Nil, Dynamic)
