import gleam/bit_array
import gleam/erlang
import gleam/erlang/atom.{type Atom}
import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic}
import gleam/int
import gleam/list

pub type Table(model) {
  Table(name: Atom, decoder: Decoder(model), is_persisted: Bool, path: String)
}

pub type TableName {
  /// A table identifier
  Tid(Dynamic)

  // this is not a great name but I do not want it to clash with the erlang/atom module's type
  /// An atom representing a (named) table's name
  Named(Atom)
}

pub type PurseError {
  SystemException(Dynamic)
  DecodeError(List(DecodeError))
}

pub type Key(t) {
  StringKey(String)
  IntKey(Int)
  TermKey(t)
  AtomKey(Atom)
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

pub fn key_to_bit_array(key: Key(_)) -> BitArray {
  case key {
    IntKey(i) -> int.to_string(i)
    StringKey(s) -> s
    TermKey(t) -> erlang.format(t)
    AtomKey(a) -> atom.to_string(a)
  }
  |> bit_array.from_string
}

pub fn decode_recursively(
  values: List(Dynamic),
  decode: Decoder(a),
  state: List(a),
  next: fn(List(a)) -> Result(List(a), PurseError),
) -> Result(List(a), PurseError) {
  case values {
    [] -> next(state)
    [raw_value, ..rest] ->
      case decode(raw_value) {
        Ok(value) -> {
          decode_recursively(rest, decode, list.concat([[value], state]), next)
        }
        Error(error) -> Error(DecodeError(error))
      }
  }
}
