import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic}
import gleam/erlang/process.{type Pid}
import gleam/list

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

pub type Table(name, model) =
  #(name, Decoder(model))

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
pub fn new(name name: a, options options: List(TableOptions(c))) {
  do_new(name, options)
}

// TODO: rewrite as an FFI for crash safety
@external(erlang, "ets", "new")
pub fn do_new(name: a, options: List(TableOptions(b))) -> a

pub fn decode_recursively(
  values: List(Dynamic),
  decode: Decoder(a),
  state: List(a),
  next: fn(List(a)) -> Result(List(a), List(DecodeError)),
) -> Result(List(a), List(DecodeError)) {
  case values {
    [] -> next(state)
    [raw_value, ..rest] ->
      case decode(raw_value) {
        Ok(value) -> {
          decode_recursively(rest, decode, list.concat([[value], state]), next)
        }
        Error(error) -> Error(error)
      }
  }
}
