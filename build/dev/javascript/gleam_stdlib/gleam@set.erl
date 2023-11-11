-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([new/0, size/1, insert/2, contains/2, delete/2, to_list/1, from_list/1, fold/3, filter/2, drop/2, take/2, union/2, intersection/2]).
-export_type([set/1]).

-opaque set(DGH) :: {set, gleam@map:map_(DGH, list(nil))}.

-spec new() -> set(any()).
new() ->
    {set, gleam@map:new()}.

-spec size(set(any())) -> integer().
size(Set) ->
    gleam@map:size(erlang:element(2, Set)).

-spec insert(set(DGN), DGN) -> set(DGN).
insert(Set, Member) ->
    {set, gleam@map:insert(erlang:element(2, Set), Member, [])}.

-spec contains(set(DGQ), DGQ) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@map:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-spec delete(set(DGS), DGS) -> set(DGS).
delete(Set, Member) ->
    {set, gleam@map:delete(erlang:element(2, Set), Member)}.

-spec to_list(set(DGV)) -> list(DGV).
to_list(Set) ->
    gleam@map:keys(erlang:element(2, Set)).

-spec from_list(list(DGY)) -> set(DGY).
from_list(Members) ->
    Map = gleam@list:fold(
        Members,
        gleam@map:new(),
        fun(M, K) -> gleam@map:insert(M, K, []) end
    ),
    {set, Map}.

-spec fold(set(DHB), DHD, fun((DHD, DHB) -> DHD)) -> DHD.
fold(Set, Initial, Reducer) ->
    gleam@map:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-spec filter(set(DHE), fun((DHE) -> boolean())) -> set(DHE).
filter(Set, Predicate) ->
    {set,
        gleam@map:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-spec drop(set(DHH), list(DHH)) -> set(DHH).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-spec take(set(DHL), list(DHL)) -> set(DHL).
take(Set, Desired) ->
    {set, gleam@map:take(erlang:element(2, Set), Desired)}.

-spec order(set(DHP), set(DHP)) -> {set(DHP), set(DHP)}.
order(First, Second) ->
    case gleam@map:size(erlang:element(2, First)) > gleam@map:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-spec union(set(DHU), set(DHU)) -> set(DHU).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-spec intersection(set(DHY), set(DHY)) -> set(DHY).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).
