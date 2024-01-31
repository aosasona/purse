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

pub type WriteConcurrencyAlternative {
  Auto
}

pub type TableOptions {
  /// When present, the table is registered under the given name and can be referenced with that name instead of a `tid()`.
  NamedTable

  /// The table is a set table: one key, one object, unordered. This is the default.
  Set

  /// The table is an ordered set table: one key, one object, ordere in Erlang term order.
  OrderedSet

  /// The table is a bag table: many objects, but only one copy/instance of each object per key.
  Bag

  /// The table is a duplicate bag table: many objects, and multiple copies of the same object per key.
  DuplicateBag

  /// Any process can read and write to this table.
  Public

  /// Only the owner process can read and write to this table.
  Private

  /// Only the owner process can both read and write to this table, other processes can ONLY read from it. This is the default.
  Protected

  /// Set a process as heir to the table. If the owner process dies, the heir process will become the new owner. The heir must be a **LOCAL** process.
  /// This library has no way to set heir to none (for now, due to naming conflicts), but it is the default and you don't need to worry about it unless you want to actually set an heir.
  Heir(Pid, Atom)

  /// Performance tuning. See https://www.erlang.org/doc/man/ets for more information. This library has no way to provide other values other than `auto`, the default is false.
  WriteConcurrency(WriteConcurrencyAlternative)

  /// Performance tuning. Defaults to false. When set to true, the table is optimized for concurrent read operations. When this option is enabled read operations become much cheaper; especially on systems with multiple physical processors. However, switching between read and write operations becomes more expensive.
  /// See https://www.erlang.org/doc/man/ets
  ReadConcurrency(Bool)

  /// Performance tuning. Defaults to true for all tables with the `write_concurrency` option set to auto. For tables of type `ordered_set` the option also defaults to true when the `write_concurrency` option is set to true. The option defaults to false for all other configurations. This option has no effect if the `write_concurrency` option is set to false.
  /// See https://www.erlang.org/doc/man/ets
  DecentralizedCounters(Bool)

  /// If this option is present, the table data is stored in a more compact format to consume less memory. However, it will make table operations slower. Especially operations that need to inspect entire objects, such as match and select, get much slower. The key element is not compressed.
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
  options options: List(TableOptions),
) -> Result(Table(b), PurseError) {
  use table_name <- result.try(
    do_new(name, options)
    |> result.map_error(fn(e) { SystemException(e) }),
  )

  Ok(Table(table_name, decoder, False, ""))
}

@external(erlang, "purse_ffi", "new")
pub fn do_new(name: Atom, options: List(TableOptions)) -> Result(Atom, Dynamic)

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
