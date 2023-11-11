-module(gleam@map).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(KS, KT) :: any() | {gleam_phantom, KS, KT}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(LC, LD)) -> list({LC, LD}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({LM, LN})) -> map_(LM, LN).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(LW, any()), LW) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(MM, MN), MM) -> {ok, MN} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(MY, MZ), MY, MZ) -> map_(MY, MZ).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(NK, NL), fun((NK, NL) -> NO)) -> map_(NK, NO).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(NY, any())) -> list(NY).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), OJ)) -> list(OJ).
values(Map) ->
    maps:values(Map).

-spec filter(map_(OS, OT), fun((OS, OT) -> boolean())) -> map_(OS, OT).
filter(Map, Predicate) ->
    maps:filter(Predicate, Map).

-spec take(map_(PE, PF), list(PE)) -> map_(PE, PF).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(PS, PT), map_(PS, PT)) -> map_(PS, PT).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(QI, QJ), QI) -> map_(QI, QJ).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(QU, QV), list(QU)) -> map_(QU, QV).
drop(Map, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Map;

        [X | Xs] ->
            drop(delete(Map, X), Xs)
    end.

-spec update(map_(RB, RC), RB, fun((gleam@option:option(RC)) -> RC)) -> map_(RB, RC).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({RI, RJ}), RL, fun((RL, RI, RJ) -> RL)) -> RL.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(RM, RN), RQ, fun((RQ, RM, RN) -> RQ)) -> RQ.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
