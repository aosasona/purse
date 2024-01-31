-module(purse_ffi).

-export([insert/2, lookup/2, delete/2, new/2, drop_table/1, list_tables/0]).

new(Name, Options) ->
  try
    {ok, ets:new(Name, Options)}
  catch
    _:Reason ->
      {error, Reason}
  end.

insert(Table, KvPair) ->
  try ets:insert(Table, KvPair) of
    true ->
      {_, Data} = KvPair,
      {ok, Data};
    _ ->
      {error, nil}
  catch
    _:Reason ->
      {error, Reason}
  end.

lookup(Table, Key) ->
  try
    {ok, ets:lookup(Table, Key)}
  catch
    _:Reason ->
      {error, Reason}
  end.

drop_table(Table) ->
  try ets:delete(Table) of
    true ->
      {ok, nil};
    _ ->
      {error, nil}
  catch
    _:Reason ->
      {error, Reason}
  end.

list_tables() ->
  try ets:all() of
    Tables ->
      {ok, lists:map(fun table_name_to_gleam_type/1, Tables)}
  catch
    _:Reason ->
      {error, Reason}
  end.

delete(Table, Key) ->
  try ets:delete(Table, Key) of
    true ->
      {ok, nil};
    _ ->
      {error, nil}
  catch
    _:Reason ->
      {error, Reason}
  end.

table_name_to_gleam_type(Table) ->
  case Table of
    T when is_atom(Table) ->
      {named, T};
    _ ->
      {tid, Table}
  end.
