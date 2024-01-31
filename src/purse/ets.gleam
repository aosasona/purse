import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/result
import purse/core.{
  type Key, type PurseError, type Table, type TableName, SystemException, Table,
  key_to_bit_array,
}
import purse/internal/ffi

pub type TableOptions(a) {
  NamedTable

  Set

  OrderedSet

  Bag

  DuplicateBag

  Public

  Private

  Protected

  Heir(Pid, Atom)

  WriteConcurrency(a)

  ReadConcurrency(Bool)

  DecentralizedCounters(Bool)

  Compressed
}

/// Creates a new ETS table with the given name and options. You may want to look at `named.new`, it has lesser options but all you would usually need for a named table.
///
/// # Example
///
/// ```gleam
/// import purse
/// import purse/ets
///
///
/// ```
pub fn new(
  name name: Atom,
  accepts decoder: Decoder(b),
  options options: List(TableOptions(c)),
) -> Result(Table(b), PurseError) {
  use table_name <- result.try(
    do_new(name, options)
    |> result.map_error(fn(e) { SystemException(e) }),
  )

  Ok(Table(table_name, decoder, False, ""))
}

@external(erlang, "purse_ffi", "new")
pub fn do_new(
  name: Atom,
  options: List(TableOptions(b)),
) -> Result(Atom, Dynamic)

/// Inserts a value into a table
pub fn insert(
  table table: Table(model),
  key key: Key(_),
  value value: model,
) -> Result(model, PurseError) {
  key
  |> key_to_bit_array
  |> fn(k) { #(k, value) }
  |> ffi.insert(table.name, _)
  |> result.map_error(fn(e) { SystemException(e) })
}

/// Looks up a value in a table by key
pub fn lookup(
  table: Table(model),
  key key: Key(_),
) -> Result(List(model), PurseError) {
  use kv_pairs <- result.try(
    key
    |> key_to_bit_array
    |> ffi.lookup(table.name, _)
    |> result.map_error(fn(e) { SystemException(e) }),
  )

  let values =
    kv_pairs
    |> list.map(fn(result) { result.1 })

  use decoded_values <- core.decode_recursively(values, table.decoder, [])

  Ok(decoded_values)
}

/// Deletes all objects with the given key from the table
pub fn delete(table: Table(_), key key: Key(_)) -> Result(Nil, PurseError) {
  key
  |> key_to_bit_array
  |> ffi.delete(table.name, _)
  |> result.map_error(fn(e) { SystemException(e) })
}

/// Drops a table, deleting all of its contents - this is irreversible
pub fn drop_table(table: Table(_)) -> Result(Nil, PurseError) {
  ffi.drop_table(table.name)
  |> result.map_error(fn(e) { SystemException(e) })
}

/// Returns a list of all tables in the current node. This function returns a dynamic list because a table name is not guaranteed to be an atom, it could be a `tid()`
pub fn list_tables() -> Result(List(TableName), PurseError) {
  ffi.list_tables()
  |> result.map_error(fn(e) { SystemException(e) })
}
