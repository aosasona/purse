import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/result
import purse/core.{type PurseError, type Table, SystemException, Table}

pub type AccessOptions {
  Read
  ReadWrite
}

pub type TableOptions(model) {
  /// Existing tables can be opened in read-only mode. Default is read-write.
  Access(AccessOptions)
}

/// Creates a new DETS table with the given name and options.
pub fn new(
  name name: Atom,
  path path: String,
  accepts decoder: Decoder(b),
  options options: List(TableOptions(c)),
) -> Result(Table(b), PurseError) {
  use table_name <- result.try(
    do_new(name, options)
    |> result.map_error(fn(e) { SystemException(e) }),
  )

  Ok(Table(table_name, decoder, True, path))
}

@external(erlang, "dets", "open_file")
fn do_new(name: Atom, args: List(TableOptions(a))) -> Result(Atom, Dynamic)
