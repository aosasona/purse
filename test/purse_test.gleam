import gleam/erlang/atom
import gleam/dynamic
import gleeunit
import gleeunit/should
import purse/named
import purse

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

pub fn new_test() {
  named.new(
    name: atom.create_from_string("users"),
    visibility: named.Public,
    table_type: named.Set,
    accepts: decoder(),
  )
  |> should.be_ok
}

pub fn insert_test() {
  let assert Ok(table) =
    named.new(
      name: atom.create_from_string("users_for_insert"),
      visibility: named.Public,
      table_type: named.Set,
      accepts: decoder(),
    )

  purse.insert(table, key: purse.int(1), value: Person("John", "Doe", 19))
  |> should.be_ok
  |> should.equal(Person("John", "Doe", 19))

  purse.insert(table, purse.term(DummyTerm), value: Person("Mary", "Smith", 21))
  |> should.be_ok
  |> should.equal(Person("Mary", "Smith", 21))

  purse.insert(table, purse.string("foo"), value: Person("Jane", "Doe", 20))
  |> should.be_ok
  |> should.equal(Person("Jane", "Doe", 20))
}

pub fn lookup_test() {
  let assert Ok(table) =
    named.new(
      name: atom.create_from_string("users_for_lookup"),
      visibility: named.Public,
      table_type: named.Set,
      accepts: decoder(),
    )

  let assert Ok(person1) =
    purse.insert(table, key: purse.int(1), value: Person("John", "Doe", 19))

  let assert Ok(person2) =
    purse.insert(
      table,
      purse.term(DummyTerm),
      value: Person("Mary", "Smith", 21),
    )

  let assert Ok(person3) =
    purse.insert(table, purse.string("foo"), value: Person("Jane", "Doe", 20))

  purse.lookup(table, key: purse.int(1))
  |> should.be_ok
  |> should.equal([person1])

  purse.lookup(table, key: purse.term(DummyTerm))
  |> should.be_ok
  |> should.equal([person2])

  purse.lookup(table, key: purse.string("foo"))
  |> should.be_ok
  |> should.equal([person3])
}
