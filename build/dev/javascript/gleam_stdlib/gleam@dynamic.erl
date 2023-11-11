-module(gleam@dynamic).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([from/1, unsafe_coerce/1, dynamic/1, bit_array/1, bit_string/1, classify/1, int/1, float/1, bool/1, shallow_list/1, optional/1, any/1, decode1/2, result/2, list/1, string/1, field/2, optional_field/2, element/2, tuple2/2, tuple3/3, tuple4/4, tuple5/5, tuple6/6, map/2, decode2/3, decode3/4, decode4/5, decode5/6, decode6/7, decode7/8, decode8/9, decode9/10]).
-export_type([dynamic_/0, decode_error/0, unknown_tuple/0]).

-type dynamic_() :: any().

-type decode_error() :: {decode_error, binary(), binary(), list(binary())}.

-type unknown_tuple() :: any().

-spec from(any()) -> dynamic_().
from(A) ->
    gleam_stdlib:identity(A).

-spec unsafe_coerce(dynamic_()) -> any().
unsafe_coerce(A) ->
    gleam_stdlib:identity(A).

-spec dynamic(dynamic_()) -> {ok, dynamic_()} | {error, list(decode_error())}.
dynamic(Value) ->
    {ok, Value}.

-spec bit_array(dynamic_()) -> {ok, bitstring()} | {error, list(decode_error())}.
bit_array(Data) ->
    gleam_stdlib:decode_bit_array(Data).

-spec bit_string(dynamic_()) -> {ok, bitstring()} |
    {error, list(decode_error())}.
bit_string(Data) ->
    bit_array(Data).

-spec put_expected(decode_error(), binary()) -> decode_error().
put_expected(Error, Expected) ->
    erlang:setelement(2, Error, Expected).

-spec classify(dynamic_()) -> binary().
classify(Data) ->
    gleam_stdlib:classify_dynamic(Data).

-spec int(dynamic_()) -> {ok, integer()} | {error, list(decode_error())}.
int(Data) ->
    gleam_stdlib:decode_int(Data).

-spec float(dynamic_()) -> {ok, float()} | {error, list(decode_error())}.
float(Data) ->
    gleam_stdlib:decode_float(Data).

-spec bool(dynamic_()) -> {ok, boolean()} | {error, list(decode_error())}.
bool(Data) ->
    gleam_stdlib:decode_bool(Data).

-spec shallow_list(dynamic_()) -> {ok, list(dynamic_())} |
    {error, list(decode_error())}.
shallow_list(Value) ->
    gleam_stdlib:decode_list(Value).

-spec optional(fun((dynamic_()) -> {ok, DNG} | {error, list(decode_error())})) -> fun((dynamic_()) -> {ok,
        gleam@option:option(DNG)} |
    {error, list(decode_error())}).
optional(Decode) ->
    fun(Value) -> gleam_stdlib:decode_option(Value, Decode) end.

-spec at_least_decode_tuple_error(integer(), dynamic_()) -> {ok, any()} |
    {error, list(decode_error())}.
at_least_decode_tuple_error(Size, Data) ->
    S = case Size of
        1 ->
            <<""/utf8>>;

        _ ->
            <<"s"/utf8>>
    end,
    Error = begin
        _pipe = [<<"Tuple of at least "/utf8>>,
            gleam@int:to_string(Size),
            <<" element"/utf8>>,
            S],
        _pipe@1 = gleam@string_builder:from_strings(_pipe),
        _pipe@2 = gleam@string_builder:to_string(_pipe@1),
        {decode_error, _pipe@2, classify(Data), []}
    end,
    {error, [Error]}.

-spec any(list(fun((dynamic_()) -> {ok, DRG} | {error, list(decode_error())}))) -> fun((dynamic_()) -> {ok,
        DRG} |
    {error, list(decode_error())}).
any(Decoders) ->
    fun(Data) -> case Decoders of
            [] ->
                {error,
                    [{decode_error, <<"another type"/utf8>>, classify(Data), []}]};

            [Decoder | Decoders@1] ->
                case Decoder(Data) of
                    {ok, Decoded} ->
                        {ok, Decoded};

                    {error, _} ->
                        (any(Decoders@1))(Data)
                end
        end end.

-spec all_errors({ok, any()} | {error, list(decode_error())}) -> list(decode_error()).
all_errors(Result) ->
    case Result of
        {ok, _} ->
            [];

        {error, Errors} ->
            Errors
    end.

-spec decode1(
    fun((DRK) -> DRL),
    fun((dynamic_()) -> {ok, DRK} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DRL} | {error, list(decode_error())}).
decode1(Constructor, T1) ->
    fun(Value) -> case T1(Value) of
            {ok, A} ->
                {ok, Constructor(A)};

            A@1 ->
                {error, all_errors(A@1)}
        end end.

-spec push_path(decode_error(), any()) -> decode_error().
push_path(Error, Name) ->
    Name@1 = from(Name),
    Decoder = any(
        [fun string/1,
            fun(X) -> gleam@result:map(int(X), fun gleam@int:to_string/1) end]
    ),
    Name@3 = case Decoder(Name@1) of
        {ok, Name@2} ->
            Name@2;

        {error, _} ->
            _pipe = [<<"<"/utf8>>, classify(Name@1), <<">"/utf8>>],
            _pipe@1 = gleam@string_builder:from_strings(_pipe),
            gleam@string_builder:to_string(_pipe@1)
    end,
    erlang:setelement(4, Error, [Name@3 | erlang:element(4, Error)]).

-spec result(
    fun((dynamic_()) -> {ok, DMU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DMW} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {ok, DMU} | {error, DMW}} |
    {error, list(decode_error())}).
result(Decode_ok, Decode_error) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_result(Value),
            fun(Inner_result) -> case Inner_result of
                    {ok, Raw} ->
                        gleam@result:'try'(
                            begin
                                _pipe = Decode_ok(Raw),
                                map_errors(
                                    _pipe,
                                    fun(_capture) ->
                                        push_path(_capture, <<"ok"/utf8>>)
                                    end
                                )
                            end,
                            fun(Value@1) -> {ok, {ok, Value@1}} end
                        );

                    {error, Raw@1} ->
                        gleam@result:'try'(
                            begin
                                _pipe@1 = Decode_error(Raw@1),
                                map_errors(
                                    _pipe@1,
                                    fun(_capture@1) ->
                                        push_path(_capture@1, <<"error"/utf8>>)
                                    end
                                )
                            end,
                            fun(Value@2) -> {ok, {error, Value@2}} end
                        )
                end end
        )
    end.

-spec list(fun((dynamic_()) -> {ok, DNB} | {error, list(decode_error())})) -> fun((dynamic_()) -> {ok,
        list(DNB)} |
    {error, list(decode_error())}).
list(Decoder_type) ->
    fun(Dynamic) ->
        gleam@result:'try'(shallow_list(Dynamic), fun(List) -> _pipe = List,
                _pipe@1 = gleam@list:try_map(_pipe, Decoder_type),
                map_errors(
                    _pipe@1,
                    fun(_capture) -> push_path(_capture, <<"*"/utf8>>) end
                ) end)
    end.

-spec map_errors(
    {ok, DLP} | {error, list(decode_error())},
    fun((decode_error()) -> decode_error())
) -> {ok, DLP} | {error, list(decode_error())}.
map_errors(Result, F) ->
    gleam@result:map_error(
        Result,
        fun(_capture) -> gleam@list:map(_capture, F) end
    ).

-spec decode_string(dynamic_()) -> {ok, binary()} |
    {error, list(decode_error())}.
decode_string(Data) ->
    _pipe = bit_array(Data),
    _pipe@1 = map_errors(
        _pipe,
        fun(_capture) -> put_expected(_capture, <<"String"/utf8>>) end
    ),
    gleam@result:'try'(
        _pipe@1,
        fun(Raw) -> case gleam@bit_array:to_string(Raw) of
                {ok, String} ->
                    {ok, String};

                {error, nil} ->
                    {error,
                        [{decode_error,
                                <<"String"/utf8>>,
                                <<"BitArray"/utf8>>,
                                []}]}
            end end
    ).

-spec string(dynamic_()) -> {ok, binary()} | {error, list(decode_error())}.
string(Data) ->
    decode_string(Data).

-spec field(
    any(),
    fun((dynamic_()) -> {ok, DNQ} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DNQ} | {error, list(decode_error())}).
field(Name, Inner_type) ->
    fun(Value) ->
        Missing_field_error = {decode_error,
            <<"field"/utf8>>,
            <<"nothing"/utf8>>,
            []},
        gleam@result:'try'(
            gleam_stdlib:decode_field(Value, Name),
            fun(Maybe_inner) -> _pipe = Maybe_inner,
                _pipe@1 = gleam@option:to_result(_pipe, [Missing_field_error]),
                _pipe@2 = gleam@result:'try'(_pipe@1, Inner_type),
                map_errors(
                    _pipe@2,
                    fun(_capture) -> push_path(_capture, Name) end
                ) end
        )
    end.

-spec optional_field(
    any(),
    fun((dynamic_()) -> {ok, DNU} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, gleam@option:option(DNU)} |
    {error, list(decode_error())}).
optional_field(Name, Inner_type) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_field(Value, Name),
            fun(Maybe_inner) -> case Maybe_inner of
                    none ->
                        {ok, none};

                    {some, Dynamic_inner} ->
                        _pipe = Dynamic_inner,
                        _pipe@1 = gleam_stdlib:decode_option(_pipe, Inner_type),
                        map_errors(
                            _pipe@1,
                            fun(_capture) -> push_path(_capture, Name) end
                        )
                end end
        )
    end.

-spec element(
    integer(),
    fun((dynamic_()) -> {ok, DOC} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DOC} | {error, list(decode_error())}).
element(Index, Inner_type) ->
    fun(Data) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple(Data),
            fun(Tuple) ->
                Size = gleam_stdlib:size_of_tuple(Tuple),
                gleam@result:'try'(case Index >= 0 of
                        true ->
                            case Index < Size of
                                true ->
                                    gleam_stdlib:tuple_get(Tuple, Index);

                                false ->
                                    at_least_decode_tuple_error(Index + 1, Data)
                            end;

                        false ->
                            case gleam@int:absolute_value(Index) =< Size of
                                true ->
                                    gleam_stdlib:tuple_get(Tuple, Size + Index);

                                false ->
                                    at_least_decode_tuple_error(
                                        gleam@int:absolute_value(Index),
                                        Data
                                    )
                            end
                    end, fun(Data@1) -> _pipe = Inner_type(Data@1),
                        map_errors(
                            _pipe,
                            fun(_capture) -> push_path(_capture, Index) end
                        ) end)
            end
        )
    end.

-spec tuple_errors({ok, any()} | {error, list(decode_error())}, binary()) -> list(decode_error()).
tuple_errors(Result, Name) ->
    case Result of
        {ok, _} ->
            [];

        {error, Errors} ->
            gleam@list:map(
                Errors,
                fun(_capture) -> push_path(_capture, Name) end
            )
    end.

-spec tuple2(
    fun((dynamic_()) -> {ok, DPC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPE} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DPC, DPE}} | {error, list(decode_error())}).
tuple2(Decode1, Decode2) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple2(Value),
            fun(_use0) ->
                {A, B} = _use0,
                case {Decode1(A), Decode2(B)} of
                    {{ok, A@1}, {ok, B@1}} ->
                        {ok, {A@1, B@1}};

                    {A@2, B@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        {error, _pipe@1}
                end
            end
        )
    end.

-spec tuple3(
    fun((dynamic_()) -> {ok, DPH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPJ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPL} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DPH, DPJ, DPL}} | {error, list(decode_error())}).
tuple3(Decode1, Decode2, Decode3) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple3(Value),
            fun(_use0) ->
                {A, B, C} = _use0,
                case {Decode1(A), Decode2(B), Decode3(C)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}} ->
                        {ok, {A@1, B@1, C@1}};

                    {A@2, B@2, C@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        {error, _pipe@2}
                end
            end
        )
    end.

-spec tuple4(
    fun((dynamic_()) -> {ok, DPO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPU} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DPO, DPQ, DPS, DPU}} |
    {error, list(decode_error())}).
tuple4(Decode1, Decode2, Decode3, Decode4) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple4(Value),
            fun(_use0) ->
                {A, B, C, D} = _use0,
                case {Decode1(A), Decode2(B), Decode3(C), Decode4(D)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}} ->
                        {ok, {A@1, B@1, C@1, D@1}};

                    {A@2, B@2, C@2, D@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = gleam@list:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        {error, _pipe@3}
                end
            end
        )
    end.

-spec tuple5(
    fun((dynamic_()) -> {ok, DPX} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQB} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQD} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQF} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DPX, DPZ, DQB, DQD, DQF}} |
    {error, list(decode_error())}).
tuple5(Decode1, Decode2, Decode3, Decode4, Decode5) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple5(Value),
            fun(_use0) ->
                {A, B, C, D, E} = _use0,
                case {Decode1(A),
                    Decode2(B),
                    Decode3(C),
                    Decode4(D),
                    Decode5(E)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}, {ok, E@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1}};

                    {A@2, B@2, C@2, D@2, E@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = gleam@list:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = gleam@list:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        {error, _pipe@4}
                end
            end
        )
    end.

-spec tuple6(
    fun((dynamic_()) -> {ok, DQI} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQK} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQM} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQS} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DQI, DQK, DQM, DQO, DQQ, DQS}} |
    {error, list(decode_error())}).
tuple6(Decode1, Decode2, Decode3, Decode4, Decode5, Decode6) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple6(Value),
            fun(_use0) ->
                {A, B, C, D, E, F} = _use0,
                case {Decode1(A),
                    Decode2(B),
                    Decode3(C),
                    Decode4(D),
                    Decode5(E),
                    Decode6(F)} of
                    {{ok, A@1},
                        {ok, B@1},
                        {ok, C@1},
                        {ok, D@1},
                        {ok, E@1},
                        {ok, F@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1, F@1}};

                    {A@2, B@2, C@2, D@2, E@2, F@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = gleam@list:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = gleam@list:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        _pipe@5 = gleam@list:append(
                            _pipe@4,
                            tuple_errors(F@2, <<"5"/utf8>>)
                        ),
                        {error, _pipe@5}
                end
            end
        )
    end.

-spec map(
    fun((dynamic_()) -> {ok, DQV} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQX} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, gleam@map:map_(DQV, DQX)} |
    {error, list(decode_error())}).
map(Key_type, Value_type) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_map(Value),
            fun(Map) ->
                gleam@result:'try'(
                    begin
                        _pipe = Map,
                        _pipe@1 = gleam@map:to_list(_pipe),
                        gleam@list:try_map(
                            _pipe@1,
                            fun(Pair) ->
                                {K, V} = Pair,
                                gleam@result:'try'(
                                    begin
                                        _pipe@2 = Key_type(K),
                                        map_errors(
                                            _pipe@2,
                                            fun(_capture) ->
                                                push_path(
                                                    _capture,
                                                    <<"keys"/utf8>>
                                                )
                                            end
                                        )
                                    end,
                                    fun(K@1) ->
                                        gleam@result:'try'(
                                            begin
                                                _pipe@3 = Value_type(V),
                                                map_errors(
                                                    _pipe@3,
                                                    fun(_capture@1) ->
                                                        push_path(
                                                            _capture@1,
                                                            <<"values"/utf8>>
                                                        )
                                                    end
                                                )
                                            end,
                                            fun(V@1) -> {ok, {K@1, V@1}} end
                                        )
                                    end
                                )
                            end
                        )
                    end,
                    fun(Pairs) -> {ok, gleam@map:from_list(Pairs)} end
                )
            end
        )
    end.

-spec decode2(
    fun((DRO, DRP) -> DRQ),
    fun((dynamic_()) -> {ok, DRO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRP} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DRQ} | {error, list(decode_error())}).
decode2(Constructor, T1, T2) ->
    fun(Value) -> case {T1(Value), T2(Value)} of
            {{ok, A}, {ok, B}} ->
                {ok, Constructor(A, B)};

            {A@1, B@1} ->
                {error, gleam@list:concat([all_errors(A@1), all_errors(B@1)])}
        end end.

-spec decode3(
    fun((DRU, DRV, DRW) -> DRX),
    fun((dynamic_()) -> {ok, DRU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRV} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRW} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DRX} | {error, list(decode_error())}).
decode3(Constructor, T1, T2, T3) ->
    fun(Value) -> case {T1(Value), T2(Value), T3(Value)} of
            {{ok, A}, {ok, B}, {ok, C}} ->
                {ok, Constructor(A, B, C)};

            {A@1, B@1, C@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1), all_errors(B@1), all_errors(C@1)]
                    )}
        end end.

-spec decode4(
    fun((DSC, DSD, DSE, DSF) -> DSG),
    fun((dynamic_()) -> {ok, DSC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSD} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSE} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSF} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DSG} | {error, list(decode_error())}).
decode4(Constructor, T1, T2, T3, T4) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}} ->
                {ok, Constructor(A, B, C, D)};

            {A@1, B@1, C@1, D@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1)]
                    )}
        end end.

-spec decode5(
    fun((DSM, DSN, DSO, DSP, DSQ) -> DSR),
    fun((dynamic_()) -> {ok, DSM} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSN} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSP} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSQ} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DSR} | {error, list(decode_error())}).
decode5(Constructor, T1, T2, T3, T4, T5) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}} ->
                {ok, Constructor(A, B, C, D, E)};

            {A@1, B@1, C@1, D@1, E@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1)]
                    )}
        end end.

-spec decode6(
    fun((DSY, DSZ, DTA, DTB, DTC, DTD) -> DTE),
    fun((dynamic_()) -> {ok, DSY} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTB} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTD} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DTE} | {error, list(decode_error())}).
decode6(Constructor, T1, T2, T3, T4, T5, T6) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}} ->
                {ok, Constructor(A, B, C, D, E, F)};

            {A@1, B@1, C@1, D@1, E@1, F@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1)]
                    )}
        end end.

-spec decode7(
    fun((DTM, DTN, DTO, DTP, DTQ, DTR, DTS) -> DTT),
    fun((dynamic_()) -> {ok, DTM} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTN} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTP} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTR} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTS} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DTT} | {error, list(decode_error())}).
decode7(Constructor, T1, T2, T3, T4, T5, T6, T7) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}, {ok, G}} ->
                {ok, Constructor(A, B, C, D, E, F, G)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1)]
                    )}
        end end.

-spec decode8(
    fun((DUC, DUD, DUE, DUF, DUG, DUH, DUI, DUJ) -> DUK),
    fun((dynamic_()) -> {ok, DUC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUD} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUE} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUF} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUI} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUJ} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DUK} | {error, list(decode_error())}).
decode8(Constructor, T1, T2, T3, T4, T5, T6, T7, T8) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X)} of
            {{ok, A},
                {ok, B},
                {ok, C},
                {ok, D},
                {ok, E},
                {ok, F},
                {ok, G},
                {ok, H}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1),
                            all_errors(H@1)]
                    )}
        end end.

-spec decode9(
    fun((DUU, DUV, DUW, DUX, DUY, DUZ, DVA, DVB, DVC) -> DVD),
    fun((dynamic_()) -> {ok, DUU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUV} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUW} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUX} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUY} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DUZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DVA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DVB} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DVC} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DVD} | {error, list(decode_error())}).
decode9(Constructor, T1, T2, T3, T4, T5, T6, T7, T8, T9) ->
    fun(X) ->
        case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X), T9(X)} of
            {{ok, A},
                {ok, B},
                {ok, C},
                {ok, D},
                {ok, E},
                {ok, F},
                {ok, G},
                {ok, H},
                {ok, I}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H, I)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1, I@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1),
                            all_errors(H@1),
                            all_errors(I@1)]
                    )}
        end
    end.
