-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BKI} | {error, BKJ}, fun((BKI) -> BKM)) -> {ok, BKM} |
    {error, BKJ}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BKP} | {error, BKQ}, fun((BKQ) -> BKT)) -> {ok, BKP} |
    {error, BKT}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BKW} | {error, BKX}} | {error, BKX}) -> {ok, BKW} |
    {error, BKX}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BLE} | {error, BLF}, fun((BLE) -> {ok, BLI} | {error, BLF})) -> {ok,
        BLI} |
    {error, BLF}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BLN} | {error, BLO}, fun((BLN) -> {ok, BLR} | {error, BLO})) -> {ok,
        BLR} |
    {error, BLO}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BLW} | {error, any()}, BLW) -> BLW.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BMA} | {error, any()}, fun(() -> BMA)) -> BMA.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BMF}, BMF) -> BMF.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BMI} | {error, BMI}) -> BMI.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BML} | {error, any()}) -> {ok, BML} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BMR} | {error, BMS}, {ok, BMR} | {error, BMS}) -> {ok, BMR} |
    {error, BMS}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BMZ} | {error, BNA}, fun(() -> {ok, BMZ} | {error, BNA})) -> {ok,
        BMZ} |
    {error, BNA}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BNH} | {error, BNI})) -> {ok, list(BNH)} | {error, BNI}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BNW} | {error, BNX}), list(BNW), list(BNX)) -> {list(BNW),
    list(BNX)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BNP} | {error, BNQ})) -> {list(BNP), list(BNQ)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BOF}, BOI) -> {ok, BOI} | {error, BOF}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BOL} | {error, any()}, BOP) -> {ok, BOL} | {error, BOP}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BOS} | {error, any()})) -> list(BOS).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BOY} | {error, BOZ},
    fun((BOZ) -> {ok, BOY} | {error, BPC})
) -> {ok, BOY} | {error, BPC}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
