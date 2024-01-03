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

// We can't use the same type for both the insert and lookup tests because calling new with the same table name more than once will fail.
pub type Users {
  UsersForInsert
  UsersForLookup
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
  let table =
    named.new(
      name: UsersForInsert,
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
  let table =
    named.new(
      name: UsersForLookup,
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
