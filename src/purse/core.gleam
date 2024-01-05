import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/result

pub type TableOptions(a) {
  NamedTable

  Set

  OrderedSet

  Bag

  DuplicateBag

  Public

  Private

  Protected

  Heir(Pid, a)
}

pub type Table(model) {
  Table(name: Atom, decoder: Decoder(model))
}

pub type LookupError {
  SystemException(Dynamic)
  DecodeError(List(DecodeError))
}

/// Creates a new ETS table with the given name and options. You may want to look at `named.new`, it has lesser options but all you would usually need for a named table.
///
/// # Example
///
/// ```gleam
/// import purse
/// import purse/core
///
///
/// ```
pub fn new(
  name name: Atom,
  accepts decoder: Decoder(b),
  options options: List(TableOptions(c)),
) -> Result(Table(b), Dynamic) {
  use table_name <- result.try(do_new(name, options))

  Ok(Table(table_name, decoder))
}

@external(erlang, "purse_ffi", "new")
pub fn do_new(
  name: Atom,
  options: List(TableOptions(b)),
) -> Result(Atom, Dynamic)

pub fn decode_recursively(
  values: List(Dynamic),
  decode: Decoder(a),
  state: List(a),
  next: fn(List(a)) -> Result(List(a), LookupError),
) -> Result(List(a), LookupError) {
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
