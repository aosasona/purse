pub type Pid

pub type TableOptions(a) {
  NamedTable

  Set

  OrderedSet

  Bag

  DuplicateBag

  Public

  Private

  Protected

  Heir(Pid, a)
}

@external(erlang, "ets", "new")
pub fn new(name: a, options: List(TableOptions(b))) -> a
