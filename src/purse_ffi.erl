-module(purse_ffi).

-export([insert/2, lookup/2, new/2, drop_table/1]).

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
