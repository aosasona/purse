# purse

[![Package Version](https://img.shields.io/hexpm/v/purse)](https://hex.pm/packages/purse)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/purse/)

```gleam
import purse
import purse/named

pub fn main() {
  let decoder =
    dynamic.decode3(
      Data,
      dynamic.element(at: 1, of: dynamic.string),
      dynamic.element(at: 2, of: dynamic.string),
      dynamic.element(at: 3, of: dynamic.int),
    )

  let t = named.new(Test, visibility: named.Public, table_type: named.Set)

  let _ = purse.insert(t, string("foo"), Data("bar", "baz", 1))

  let assert Ok([data]) = lookup(t, string("foo"), decoder)

  io.debug(data)

  Nil
}
```

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
