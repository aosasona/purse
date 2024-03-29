import gleam/dynamic
import gleam/erlang/atom
import gleam/list
import gleam/io
import gleeunit/should
import purse
import purse/core.{Named}
import purse/ets
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

pub fn insert_test() {
  let assert Ok(table) =
    ets.new(
      name: atom.create_from_string("users_for_insert"),
      accepts: decoder(),
      options: [ets.Set, ets.Protected, ets.NamedTable],
    )

  ets.insert(table, key: purse.int(1), value: Person("John", "Doe", 19))
  |> should.be_ok
  |> should.equal(Person("John", "Doe", 19))

  ets.insert(table, purse.term(DummyTerm), value: Person("Mary", "Smith", 21))
  |> should.be_ok
  |> should.equal(Person("Mary", "Smith", 21))

  ets.insert(table, purse.string("foo"), value: Person("Jane", "Doe", 20))
  |> should.be_ok
  |> should.equal(Person("Jane", "Doe", 20))
}

pub fn insert_many_test() {
  let assert Ok(table) =
    ets.new(
      name: atom.create_from_string("users_for_insert_many"),
      accepts: decoder(),
      options: [ets.Set, ets.Protected, ets.NamedTable],
    )

  let objects = [
    Person("John", "Doe", 19),
    Person("Mary", "Smith", 21),
    Person("Jane", "Doe", 20),
  ]

  ets.insert_many(table, key: purse.int(1), objects: objects)
  |> should.be_ok
  |> list.length
  |> should.equal(3)
}

pub fn lookup_test() {
  let assert Ok(table) =
    ets.new(
      name: atom.create_from_string("users_for_lookup"),
      accepts: decoder(),
      options: [ets.Set, ets.Protected, ets.NamedTable],
    )

  let assert Ok(person1) =
    ets.insert(table, key: purse.int(1), value: Person("John", "Doe", 19))

  let assert Ok(person2) =
    ets.insert(table, purse.term(DummyTerm), value: Person("Mary", "Smith", 21))

  let assert Ok(person3) =
    ets.insert(table, purse.string("foo"), value: Person("Jane", "Doe", 20))

  ets.lookup(table, key: purse.int(1))
  |> should.be_ok
  |> should.equal([person1])

  ets.lookup(table, key: purse.term(DummyTerm))
  |> should.be_ok
  |> should.equal([person2])

  ets.lookup(table, key: purse.string("foo"))
  |> should.be_ok
  |> should.equal([person3])
}

pub fn delete_test() {
  let assert Ok(table) =
    ets.new(
      name: atom.create_from_string("users_delete_test"),
      accepts: decoder(),
      options: [ets.DuplicateBag, ets.Protected, ets.NamedTable],
    )

  let assert Ok(_) =
    ets.insert(table, purse.string("human"), Person("John", "Doe", 19))

  let assert Ok(_) =
    ets.insert(table, purse.string("human"), Person("Bane", "Cheeks", 20))

  let assert Ok(_) =
    ets.insert(table, purse.string("human"), Person("Dean", "Smith", 19))

  // Check that all three entries were actually inserted
  ets.lookup(table, key: purse.string("human"))
  |> should.be_ok
  |> list.length
  |> should.equal(3)

  // Delete all entries with the key "human"
  ets.delete(table, key: purse.string("human"))
  |> should.be_ok

  ets.lookup(table, key: purse.string("human"))
  |> should.be_ok
  |> should.equal([])
}

/// Lookup should now return an empty list
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
  ets.list_tables()
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
