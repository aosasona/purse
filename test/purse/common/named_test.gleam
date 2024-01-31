import gleam/dynamic
import gleam/erlang/atom
import gleeunit/should
import purse/common/named

pub type DummyTerm {
  DummyTerm
}

pub type Person {
  Person(first_name: String, last_name: String, age: Int)
}

fn decoder() {
  dynamic.decode3(
    Person,
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.int),
  )
}

pub fn new_test() {
  named.new(
    name: atom.create_from_string("users"),
    visibility: named.Public,
    table_type: named.Set,
    accepts: decoder(),
  )
  |> should.be_ok
}
