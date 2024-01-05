# purse

[![Package Version](https://img.shields.io/hexpm/v/purse)](https://hex.pm/packages/purse)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/purse/)

```gleam
import gleam/dynamic
import gleam/erlang/atom
import gleam/io
import purse
import purse/named

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
}
```

Running this produces the following output:

> Ok([Data("cee", "dee", 3), Data("ayy", "bee", 2), Data("bar", "baz", 1)])

## Quick start

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add purse
```

and its documentation can be found at <https://hexdocs.pm/purse>.
