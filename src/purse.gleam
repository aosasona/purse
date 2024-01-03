import gleam/bit_array
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/erlang
import gleam/list
import gleam/io
import gleam/int
import purse/core
import purse/named

// I am going to go with a builder patterns like so
// new_builder(Name)
// |> set_access(SomeAccessThing)
// |> set_type(SomeTypeThing)
// ...
// |> create
// for this, I would need to also filter out the options in each category to make sure that even if you set the same option twice, it only gets set once by picking the last one
//
//  but also with an option to do it all in one go if you want without

pub type Test {
  Test
}

pub type Data {
  Data(key: String, value: String, point: Int)
}

pub fn main() {
  let decoder =
    dynamic.decode3(
      Data,
      dynamic.element(at: 1, of: dynamic.string),
      dynamic.element(at: 2, of: dynamic.string),
      dynamic.element(at: 3, of: dynamic.int),
    )

  let t =
    named.new(
      Test,
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

pub const new = core.new

pub opaque type Key(t) {
  StringKey(String)
  IntKey(Int)
  TermKey(t)
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

/// Creates a key from a term AKA any type
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
  }
  |> bit_array.from_string
}

@external(erlang, "purse_ffi", "insert")
fn do_insert(table: a, data: #(BitArray, b)) -> Result(b, Nil)

pub fn insert(
  table table: core.Table(tname, model),
  key key: Key(_),
  value value: model,
) -> Result(model, Nil) {
  key
  |> key_to_bit_array
  |> fn(k) { #(k, value) }
  |> do_insert(table.name, _)
}

// TODO: rewrite to be exception-safe in FFI
@external(erlang, "ets", "lookup")
fn do_lookup(table: a, key: BitArray) -> List(#(BitArray, Dynamic))

pub fn lookup(
  table: core.Table(_, model),
  key key: Key(_),
) -> Result(List(model), List(DecodeError)) {
  let values =
    key
    |> key_to_bit_array
    |> do_lookup(table.name, _)
    |> list.map(fn(result) { result.1 })

  use r <- core.decode_recursively(values, table.decoder, [])

  Ok(r)
}
