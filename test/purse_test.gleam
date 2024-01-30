import gleam/erlang/atom
import gleam/dynamic
import gleam/list
import gleeunit
import gleeunit/should
import purse
import purse/core
import purse/named
import purse/types.{Named}

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

pub fn unnamed_table_test() {
  // Testing unnamed table
  let other_table_name = atom.create_from_string("other_table")

  let tb =
    purse.new(other_table_name, decoder(), [core.Set])
    |> should.be_ok

  purse.insert(tb, purse.string("foo"), Person("John", "Doe", 19))
  |> should.be_ok
  |> should.equal(Person("John", "Doe", 19))

  purse.lookup(tb, purse.string("foo"))
  |> should.be_ok
  |> should.equal([Person("John", "Doe", 19)])

  purse.drop_table(tb)
  |> should.be_ok
  |> should.equal(Nil)

  // should fail
  purse.lookup(tb, purse.string("foo"))
  |> should.be_error
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

pub fn list_tables_test() {
  let table_one = atom.create_from_string("table_one")
  let table_two = atom.create_from_string("table_two")

  named.new(
    name: table_one,
    visibility: named.Public,
    table_type: named.Set,
    accepts: decoder(),
  )
  |> should.be_ok

  named.new(
    name: table_two,
    visibility: named.Public,
    table_type: named.Set,
    accepts: decoder(),
  )
  |> should.be_ok

  // Filter out all other tables except the two we created and the final result MUST contain only those two tables proving they actually exist in the list
  purse.list_tables()
  |> should.be_ok
  |> list.filter(fn(table) {
    case table {
      Named(name) ->
        case name {
          x if x == table_one -> True
          x if x == table_two -> True
          _ -> False
        }
      _ -> False
    }
  })
  |> should.equal([Named(table_one), Named(table_two)])
}
