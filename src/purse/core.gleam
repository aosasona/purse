import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/result

// TODO: complete table options
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
  Table(name: Atom, decoder: Decoder(model), is_persisted: Bool)
}

pub type PurseError {
  SystemException(Dynamic)
  DecodeError(List(DecodeError))
}

@external(erlang, "purse_ffi", "new")
pub fn do_new(
  name: Atom,
  options: List(TableOptions(b)),
) -> Result(Atom, Dynamic)

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
) -> Result(Table(b), PurseError) {
  use table_name <- result.try(
    do_new(name, options)
    |> result.map_error(fn(e) { SystemException(e) }),
  )

  Ok(Table(table_name, decoder, False))
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
// TODO: move all persisted and non-persisted functions here
