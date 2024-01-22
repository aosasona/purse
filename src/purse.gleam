import gleam/bit_array
import gleam/dynamic
import gleam/erlang
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/io
import gleam/int
import gleam/result
import purse/core.{type PurseError, type Table, SystemException}
import purse/named
import purse/internal/ffi

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

// TODO: add delete and `dets` support

pub const new = core.new

pub opaque type Key(t) {
  StringKey(String)
  IntKey(Int)
  TermKey(t)
  AtomKey(Atom)
}

/// Creates a key from an integer
/// # Example:
/// ```gleam
/// let key = purse.int(1)
/// ```
pub fn int(value: Int) -> Key(Int) {
  IntKey(value)
}

/// Creates a key from a string
///
/// # Example:
/// ```gleam
/// let key = purse.string("foo")
/// ```
pub fn string(value: String) -> Key(String) {
  StringKey(value)
}

/// Creates a key from an atom.
///
/// Atoms are never garbage collected, so you need to be careful with this one
///
/// # Example:
/// ```gleam
/// let assert Ok(key) = atom.from_string("foo")
/// let key = purse.atom(key)
/// ```
pub fn atom(value: Atom) -> Key(Atom) {
  AtomKey(value)
}

/// Creates a key from a term - you need to be VERY careful with this one, use it only if you know what you are doing
///
/// Example:
/// ```gleam
/// type Foo {
///   Foo
/// }
///
/// let key = purse.term(Foo)
/// ```
///
pub fn term(value: t) -> Key(t) {
  TermKey(value)
}

fn key_to_bit_array(key: Key(_)) -> BitArray {
  case key {
    IntKey(i) -> int.to_string(i)
    StringKey(s) -> s
    TermKey(t) -> erlang.format(t)
    AtomKey(a) -> atom.to_string(a)
  }
  |> bit_array.from_string
}

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

/// Drops a table, deleting all of its contents - this is irreversible
pub fn drop_table(table: Table(_)) -> Result(Nil, PurseError) {
  ffi.drop_table(table.name)
  |> result.map_error(fn(e) { SystemException(e) })
}
