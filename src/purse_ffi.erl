-module(purse_ffi).

-export([insert/2]).

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
