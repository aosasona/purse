import gleam/erlang/atom
import gleam/dynamic
import gleam/list
import gleeunit
import gleeunit/should
import purse
import purse/core

pub fn main() {
  gleeunit.main()
}

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
// pub fn unnamed_table_test() {
//   // Testing unnamed table
//   let other_table_name = atom.create_from_string("other_table")
//
//   let tb =
//     purse.new_ets_table(other_table_name, decoder(), [ets.Set])
//     |> should.be_ok
//
//   purse.insert(tb, purse.string("foo"), Person("John", "Doe", 19))
//   |> should.be_ok
//   |> should.equal(Person("John", "Doe", 19))
//
//   purse.lookup(tb, purse.string("foo"))
//   |> should.be_ok
//   |> should.equal([Person("John", "Doe", 19)])
//
//   purse.drop_table(tb)
//   |> should.be_ok
//   |> should.equal(Nil)
//
//   // should fail
//   purse.lookup(tb, purse.string("foo"))
//   |> should.be_error
// }
