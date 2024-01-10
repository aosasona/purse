import gleam/erlang/atom.{type Atom}
import gleam/dynamic.{type Dynamic}

// Non-persistent functions
@external(erlang, "purse_ffi", "insert")
pub fn do_insert(table: a, data: #(BitArray, b)) -> Result(b, Dynamic)

@external(erlang, "purse_ffi", "lookup")
pub fn do_lookup(
  table: Atom,
  key: BitArray,
) -> Result(List(#(BitArray, Dynamic)), Dynamic)

// TODO: implement this
@external(erlang, "purse_ffi", "delete")
pub fn do_delete(table: Atom, key: BitArray) -> Result(Nil, Dynamic)

@external(erlang, "purse_ffi", "drop_table")
pub fn do_drop_table(table: Atom) -> Result(Nil, Dynamic)
