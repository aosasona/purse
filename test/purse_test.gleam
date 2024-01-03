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

pub type Users {
  Users
}

pub type Person {
  Person(first_name: String, last_name: String, age: Int)
}

pub fn insert_test() {
  let table = named.new(Users, visibility: named.Public, table_type: named.Set)

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
