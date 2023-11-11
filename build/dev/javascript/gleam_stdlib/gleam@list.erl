-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(UD) :: {continue, UD} | {stop, UD}.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(UI)) -> list(UI).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(UQ), UQ) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-spec first(list(US)) -> {ok, US} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-spec rest(list(UW)) -> {ok, list(UW)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

-spec do_filter(list(VP), fun((VP) -> boolean()), list(VP)) -> list(VP).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(VT), fun((VT) -> boolean())) -> list(VT).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(VW), fun((VW) -> {ok, VY} | {error, any()}), list(VY)) -> list(VY).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(WE), fun((WE) -> {ok, WG} | {error, any()})) -> list(WG).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(WL), fun((WL) -> WN), list(WN)) -> list(WN).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(WQ), fun((WQ) -> WS)) -> list(WS).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec update_group(fun((VB) -> VC)) -> fun((gleam@map:map_(VC, list(VB)), VB) -> gleam@map:map_(VC, list(VB))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@map:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@map:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@map:insert(Groups, F(Elem), [Elem])
        end end.

-spec do_map2(list(XA), list(XC), fun((XA, XC) -> XE), list(XE)) -> list(XE).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            reverse(Acc);

        {_, []} ->
            reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-spec map2(list(WU), list(WW), fun((WU, WW) -> WY)) -> list(WY).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-spec do_index_map(list(XM), fun((integer(), XM) -> XO), integer(), list(XO)) -> list(XO).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(XR), fun((integer(), XR) -> XT)) -> list(XT).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(XV), fun((XV) -> {ok, XX} | {error, XY}), list(XX)) -> {ok,
        list(XX)} |
    {error, XY}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(YF), fun((YF) -> {ok, YH} | {error, YI})) -> {ok, list(YH)} |
    {error, YI}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(YO), integer()) -> list(YO).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(YR), integer(), list(YR)) -> list(YR).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(YV), integer()) -> list(YV).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec append(list(AAA), list(AAA)) -> list(AAA).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(AAI), AAI) -> list(AAI).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(AAL), list(AAL)) -> list(AAL).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-spec do_concat(list(list(AAP)), list(AAP)) -> list(AAP).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec concat(list(list(AAU))) -> list(AAU).
concat(Lists) ->
    do_concat(Lists, []).

-spec flatten(list(list(AAY))) -> list(AAY).
flatten(Lists) ->
    do_concat(Lists, []).

-spec flat_map(list(ABC), fun((ABC) -> list(ABE))) -> list(ABE).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    concat(_pipe).

-spec fold(list(ABH), ABJ, fun((ABJ, ABH) -> ABJ)) -> ABJ.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec group(list(VJ), fun((VJ) -> VL)) -> gleam@map:map_(VL, list(VJ)).
group(List, Key) ->
    fold(List, gleam@map:new(), update_group(Key)).

-spec map_fold(list(XH), XJ, fun((XJ, XH) -> {XJ, XK})) -> {XJ, list(XK)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec fold_right(list(ABK), ABM, fun((ABM, ABK) -> ABM)) -> ABM.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(
    list(ABN),
    ABP,
    fun((ABP, ABN, integer()) -> ABP),
    integer()
) -> ABP.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(ABQ), ABS, fun((ABS, ABQ, integer()) -> ABS)) -> ABS.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(ABT), ABV, fun((ABV, ABT) -> {ok, ABV} | {error, ABW})) -> {ok,
        ABV} |
    {error, ABW}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-spec fold_until(list(ACB), ACD, fun((ACD, ACB) -> continue_or_stop(ACD))) -> ACD.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(ACF), fun((ACF) -> boolean())) -> {ok, ACF} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(ACJ), fun((ACJ) -> {ok, ACL} | {error, any()})) -> {ok, ACL} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(ACR), fun((ACR) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(ACT), fun((ACT) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-spec do_zip(list(ACV), list(ACX), list({ACV, ACX})) -> list({ACV, ACX}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_, _} ->
            reverse(Acc)
    end.

-spec zip(list(ADB), list(ADD)) -> list({ADB, ADD}).
zip(List, Other) ->
    do_zip(List, Other, []).

-spec strict_zip(list(ADG), list(ADI)) -> {ok, list({ADG, ADI})} |
    {error, length_mismatch()}.
strict_zip(List, Other) ->
    case length(List) =:= length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, length_mismatch}
    end.

-spec do_unzip(list({ATI, ATJ}), list(ATI), list(ATJ)) -> {list(ATI), list(ATJ)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({ADR, ADS})) -> {list(ADR), list(ADS)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(ADW), ADW, list(ADW)) -> list(ADW).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(AEA), AEA) -> list(AEA).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(AED), integer()) -> {ok, AED} | {error, nil}.
at(List, Index) ->
    case Index >= 0 of
        true ->
            _pipe = List,
            _pipe@1 = drop(_pipe, Index),
            first(_pipe@1);

        false ->
            {error, nil}
    end.

-spec unique(list(AEH)) -> list(AEH).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec merge_up(
    integer(),
    integer(),
    list(AEK),
    list(AEK),
    list(AEK),
    fun((AEK, AEK) -> gleam@order:order())
) -> list(AEK).
merge_up(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_up(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_up(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Ax@1, Bx@1) of
                gt ->
                    merge_up(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare);

                _ ->
                    merge_up(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare)
            end
    end.

-spec merge_down(
    integer(),
    integer(),
    list(AEP),
    list(AEP),
    list(AEP),
    fun((AEP, AEP) -> gleam@order:order())
) -> list(AEP).
merge_down(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_down(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_down(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Bx@1, Ax@1) of
                lt ->
                    merge_down(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare);

                _ ->
                    merge_down(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare)
            end
    end.

-spec merge_sort(
    list(AEU),
    integer(),
    fun((AEU, AEU) -> gleam@order:order()),
    boolean()
) -> list(AEU).
merge_sort(L, Ln, Compare, Down) ->
    N = Ln div 2,
    A = L,
    B = drop(L, N),
    case Ln < 3 of
        true ->
            case Down of
                true ->
                    merge_down(N, Ln - N, A, B, [], Compare);

                false ->
                    merge_up(N, Ln - N, A, B, [], Compare)
            end;

        false ->
            case Down of
                true ->
                    merge_down(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, false),
                        merge_sort(B, Ln - N, Compare, false),
                        [],
                        Compare
                    );

                false ->
                    merge_up(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, true),
                        merge_sort(B, Ln - N, Compare, true),
                        [],
                        Compare
                    )
            end
    end.

-spec sort(list(AEX), fun((AEX, AEX) -> gleam@order:order())) -> list(AEX).
sort(List, Compare) ->
    merge_sort(List, length(List), Compare, true).

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            tail_recursive_range(Start, Stop + 1, [Stop | Acc]);

        lt ->
            tail_recursive_range(Start, Stop - 1, [Stop | Acc])
    end.

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec do_repeat(AFD, integer(), list(AFD)) -> list(AFD).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(AFG, integer()) -> list(AFG).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(AFI), integer(), list(AFI)) -> {list(AFI), list(AFI)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(AFN), integer()) -> {list(AFN), list(AFN)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(AFR), fun((AFR) -> boolean()), list(AFR)) -> {list(AFR),
    list(AFR)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(AFW), fun((AFW) -> boolean())) -> {list(AFW), list(AFW)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({AGA, AGB}), AGA) -> {ok, AGB} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec key_filter(list({AGF, AGG}), AGF) -> list(AGG).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(AXB), fun((AXB) -> boolean()), list(AXB)) -> {ok,
        {AXB, list(AXB)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, append(reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(AGN), fun((AGN) -> boolean())) -> {ok, {AGN, list(AGN)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(AXP), fun((AXP) -> {ok, AYC} | {error, any()}), list(AXP)) -> {ok,
        {AYC, list(AXP)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, append(reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(AGW), fun((AGW) -> {ok, AGY} | {error, any()})) -> {ok,
        {AGY, list(AGW)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({AHF, AHG}), AHF) -> {ok, {AHG, list({AHF, AHG})}} |
    {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({AHL, AHM}), AHL, AHM) -> list({AHL, AHM}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(AHP), fun((AHP) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec try_each(list(AHS), fun((AHS) -> {ok, any()} | {error, AHV})) -> {ok, nil} |
    {error, AHV}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [X | Xs] ->
            case Fun(X) of
                {ok, _} ->
                    try_each(Xs, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-spec do_partition(list(AZJ), fun((AZJ) -> boolean()), list(AZJ), list(AZJ)) -> {list(AZJ),
    list(AZJ)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {reverse(Trues), reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(AIF), fun((AIF) -> boolean())) -> {list(AIF), list(AIF)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(AIJ)) -> list(list(AIJ)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            _pipe = L,
            _pipe@5 = index_map(_pipe, fun(I_idx, I) -> _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end) end),
            concat(_pipe@5)
    end.

-spec do_window(list(list(AIN)), list(AIN), integer()) -> list(list(AIN)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(AIT), integer()) -> list(list(AIT)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec window_by_2(list(AIX)) -> list({AIX, AIX}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(AJA), fun((AJA) -> boolean())) -> list(AJA).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(AJD), fun((AJD) -> boolean()), list(AJD)) -> list(AJD).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    reverse(Acc)
            end
    end.

-spec take_while(list(AJH), fun((AJH) -> boolean())) -> list(AJH).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(AJK), fun((AJK) -> AJM), AJM, list(AJK), list(list(AJK))) -> list(list(AJK)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            reverse([reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(AJS), fun((AJS) -> any())) -> list(list(AJS)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-spec do_sized_chunk(
    list(AJX),
    integer(),
    integer(),
    list(AJX),
    list(list(AJX))
) -> list(list(AJX)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    reverse(Acc);

                Remaining ->
                    reverse([reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Rest,
                        Count,
                        Count,
                        [],
                        [reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(AKE), integer()) -> list(list(AKE)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(AKI), fun((AKI, AKI) -> AKI)) -> {ok, AKI} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-spec do_scan(list(AKM), AKO, list(AKO), fun((AKO, AKM) -> AKO)) -> list(AKO).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(AKR), AKT, fun((AKT, AKR) -> AKT)) -> list(AKT).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(AKV)) -> {ok, AKV} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(AKZ), integer()) -> list(list(AKZ)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(ALD)) -> list(list({ALD, ALD})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(ALH)) -> list({ALH, ALH}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    concat(_pipe).

-spec transpose(list(list(ALO))) -> list(list(ALO)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                concat(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-spec interleave(list(list(ALK))) -> list(ALK).
interleave(List) ->
    _pipe = transpose(List),
    concat(_pipe).

-spec do_shuffle_pair_unwrap(list({float(), ALT}), list(ALT)) -> list(ALT).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        _ ->
            [Elem_pair | Enumerable] = List,
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-spec do_shuffle_by_pair_indexes(list({float(), ALX})) -> list({float(), ALX}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-spec shuffle(list(AMA)) -> list(AMA).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(
        _pipe,
        [],
        fun(Acc, A) -> [{gleam@float:random(+0.0, 1.0), A} | Acc] end
    ),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
