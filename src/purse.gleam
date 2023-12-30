import gleam/io

// pub type Visibility {
//   PublicVisibility
//
//   PrivateVisibility
//
//   ProtectedVisibility
// }
//
// fn visibility_to_table_option(visibility: Visibility) -> TableOptions(_) {
//   case visibility {
//     PublicVisibility -> Public
//     PrivateVisibility -> Private
//     ProtectedVisibility -> Protected
//   }
// }

//
// pub type TableType {
//   Set
//
//   OrderedSet
//
//   Bag
//
//   DuplicateBag
// }
//
// pub opaque type EtsTable(a) {
//   EtsTable(name: a, visibility: Visibility, type_: TableType)
// }
//

// I am going to go with a builder patterns like so
// new_builder(Name)
// |> set_access(SomeAccessThing)
// |> set_type(SomeTypeThing)
// ...
// |> create
// for this, I would need to also filter out the options in each category to make sure that even if you set the same option twice, it only gets set once by picking the last one
//
//  but also with an option to do it all in one go if you want without
//
// also another option to quickly create a named table with common options e.g. new_named_table(name, visibility, type)

pub fn main() {
  do_new(CacheTable, [NamedTable, Set, Public])
  |> io.debug
}

// pub fn set_visibility(table, visibility) {
//
// }

pub fn new_named_table(name: a, visibility: Visibility) -> a {
  let options = [NamedTable, visibility_to_table_option(visibility)]
  do_new(name, options)
}
// TODO: better `new` function with support for all options in Gleam
