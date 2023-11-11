-module(lustre@attribute).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([attribute/2, property/2, on/2, map/2, to_string_parts/1, to_string/1, to_string_builder/1, style/1, class/1, classes/1, id/1, type_/1, value/1, checked/1, placeholder/1, selected/1, accept/1, accept_charset/1, msg/1, autocomplete/1, autofocus/1, disabled/1, name/1, pattern/1, readonly/1, required/1, for/1, max/1, min/1, step/1, cols/1, rows/1, wrap/1, href/1, target/1, download/1, rel/1, src/1, height/1, width/1, alt/1, autoplay/1, controls/1, loop/1]).
-export_type([attribute/1]).

-opaque attribute(FEK) :: {attribute,
        binary(),
        gleam@dynamic:dynamic_(),
        boolean()} |
    {event,
        binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, FEK} | {error, nil})}.

-spec attribute(binary(), binary()) -> attribute(any()).
attribute(Name, Value) ->
    {attribute, Name, gleam@dynamic:from(Value), false}.

-spec property(binary(), any()) -> attribute(any()).
property(Name, Value) ->
    {attribute, Name, gleam@dynamic:from(Value), true}.

-spec on(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, FEQ} | {error, any()})
) -> attribute(FEQ).
on(Name, Handler) ->
    {event,
        <<"on"/utf8, Name/binary>>,
        gleam@function:compose(
            Handler,
            fun(_capture) -> gleam@result:replace_error(_capture, nil) end
        )}.

-spec map(attribute(FEV), fun((FEV) -> FEX)) -> attribute(FEX).
map(Attr, F) ->
    case Attr of
        {attribute, Name, Value, As_property} ->
            {attribute, Name, Value, As_property};

        {event, On, Handler} ->
            {event, On, fun(E) -> gleam@result:map(Handler(E), F) end}
    end.

-spec to_string_parts(attribute(any())) -> {ok, {binary(), binary()}} |
    {error, nil}.
to_string_parts(Attr) ->
    case Attr of
        {attribute, Name, Value, As_property} ->
            case gleam@dynamic:classify(Value) of
                <<"String"/utf8>> ->
                    {ok, {Name, gleam@dynamic:unsafe_coerce(Value)}};

                <<"Boolean"/utf8>> ->
                    case gleam@dynamic:unsafe_coerce(Value) of
                        true ->
                            {ok, {Name, Name}};

                        false ->
                            {error, nil}
                    end;

                _ when As_property ->
                    {error, nil};

                _ ->
                    {ok, {Name, gleam@string:inspect(Value)}}
            end;

        {event, On, _} ->
            {ok, {<<"data-lustre-on"/utf8>>, On}}
    end.

-spec to_string(attribute(any())) -> binary().
to_string(Attr) ->
    case to_string_parts(Attr) of
        {ok, {Key, Val}} ->
            <<<<<<Key/binary, "=\""/utf8>>/binary, Val/binary>>/binary,
                "\""/utf8>>;

        {error, _} ->
            <<""/utf8>>
    end.

-spec to_string_builder(attribute(any())) -> gleam@string_builder:string_builder().
to_string_builder(Attr) ->
    case Attr of
        {attribute, Name, Value, As_property} ->
            case gleam@dynamic:classify(Value) of
                <<"String"/utf8>> ->
                    _pipe = [Name,
                        <<"=\""/utf8>>,
                        gleam@dynamic:unsafe_coerce(Value),
                        <<"\""/utf8>>],
                    gleam@string_builder:from_strings(_pipe);

                <<"Boolean"/utf8>> ->
                    case gleam@dynamic:unsafe_coerce(Value) of
                        true ->
                            gleam@string_builder:from_string(Name);

                        false ->
                            gleam@string_builder:new()
                    end;

                _ when As_property ->
                    gleam@string_builder:new();

                _ ->
                    _pipe@1 = [Name,
                        <<"=\""/utf8>>,
                        gleam@string:inspect(Value),
                        <<"\""/utf8>>],
                    gleam@string_builder:from_strings(_pipe@1)
            end;

        {event, On, _} ->
            _pipe@2 = [<<"data-lustre-on:"/utf8>>, On],
            gleam@string_builder:from_strings(_pipe@2)
    end.

-spec style(list({binary(), binary()})) -> attribute(any()).
style(Properties) ->
    attribute(
        <<"style"/utf8>>,
        (gleam@list:fold(
            Properties,
            <<""/utf8>>,
            fun(Styles, _use1) ->
                {Name, Value} = _use1,
                <<<<<<<<Styles/binary, Name/binary>>/binary, ":"/utf8>>/binary,
                        Value/binary>>/binary,
                    ";"/utf8>>
            end
        ))
    ).

-spec class(binary()) -> attribute(any()).
class(Name) ->
    attribute(<<"class"/utf8>>, Name).

-spec classes(list({binary(), boolean()})) -> attribute(any()).
classes(Names) ->
    attribute(
        <<"class"/utf8>>,
        begin
            _pipe = Names,
            _pipe@1 = gleam@list:filter_map(
                _pipe,
                fun(Class) -> case erlang:element(2, Class) of
                        true ->
                            {ok, erlang:element(1, Class)};

                        false ->
                            {error, nil}
                    end end
            ),
            gleam@string:join(_pipe@1, <<" "/utf8>>)
        end
    ).

-spec id(binary()) -> attribute(any()).
id(Name) ->
    attribute(<<"id"/utf8>>, Name).

-spec type_(binary()) -> attribute(any()).
type_(Name) ->
    attribute(<<"type"/utf8>>, Name).

-spec value(gleam@dynamic:dynamic_()) -> attribute(any()).
value(Val) ->
    property(<<"value"/utf8>>, Val).

-spec checked(boolean()) -> attribute(any()).
checked(Is_checked) ->
    property(<<"checked"/utf8>>, Is_checked).

-spec placeholder(binary()) -> attribute(any()).
placeholder(Text) ->
    attribute(<<"placeholder"/utf8>>, Text).

-spec selected(boolean()) -> attribute(any()).
selected(Is_selected) ->
    property(<<"selected"/utf8>>, Is_selected).

-spec accept(list(binary())) -> attribute(any()).
accept(Types) ->
    attribute(<<"accept"/utf8>>, gleam@string:join(Types, <<" "/utf8>>)).

-spec accept_charset(list(binary())) -> attribute(any()).
accept_charset(Types) ->
    attribute(<<"acceptCharset"/utf8>>, gleam@string:join(Types, <<" "/utf8>>)).

-spec msg(binary()) -> attribute(any()).
msg(Uri) ->
    attribute(<<"msg"/utf8>>, Uri).

-spec autocomplete(binary()) -> attribute(any()).
autocomplete(Name) ->
    attribute(<<"autocomplete"/utf8>>, Name).

-spec autofocus(boolean()) -> attribute(any()).
autofocus(Should_autofocus) ->
    property(<<"autoFocus"/utf8>>, Should_autofocus).

-spec disabled(boolean()) -> attribute(any()).
disabled(Is_disabled) ->
    property(<<"disabled"/utf8>>, Is_disabled).

-spec name(binary()) -> attribute(any()).
name(Name) ->
    attribute(<<"name"/utf8>>, Name).

-spec pattern(binary()) -> attribute(any()).
pattern(Regex) ->
    attribute(<<"pattern"/utf8>>, Regex).

-spec readonly(boolean()) -> attribute(any()).
readonly(Is_readonly) ->
    property(<<"readonly"/utf8>>, Is_readonly).

-spec required(boolean()) -> attribute(any()).
required(Is_required) ->
    property(<<"required"/utf8>>, Is_required).

-spec for(binary()) -> attribute(any()).
for(Id) ->
    attribute(<<"for"/utf8>>, Id).

-spec max(binary()) -> attribute(any()).
max(Val) ->
    attribute(<<"max"/utf8>>, Val).

-spec min(binary()) -> attribute(any()).
min(Val) ->
    attribute(<<"min"/utf8>>, Val).

-spec step(binary()) -> attribute(any()).
step(Val) ->
    attribute(<<"step"/utf8>>, Val).

-spec cols(integer()) -> attribute(any()).
cols(Val) ->
    attribute(<<"cols"/utf8>>, gleam@int:to_string(Val)).

-spec rows(integer()) -> attribute(any()).
rows(Val) ->
    attribute(<<"rows"/utf8>>, gleam@int:to_string(Val)).

-spec wrap(binary()) -> attribute(any()).
wrap(Mode) ->
    attribute(<<"wrap"/utf8>>, Mode).

-spec href(binary()) -> attribute(any()).
href(Uri) ->
    attribute(<<"href"/utf8>>, Uri).

-spec target(binary()) -> attribute(any()).
target(Target) ->
    attribute(<<"target"/utf8>>, Target).

-spec download(binary()) -> attribute(any()).
download(Filename) ->
    attribute(<<"download"/utf8>>, Filename).

-spec rel(binary()) -> attribute(any()).
rel(Relationship) ->
    attribute(<<"rel"/utf8>>, Relationship).

-spec src(binary()) -> attribute(any()).
src(Uri) ->
    attribute(<<"src"/utf8>>, Uri).

-spec height(integer()) -> attribute(any()).
height(Val) ->
    property(<<"height"/utf8>>, gleam@int:to_string(Val)).

-spec width(integer()) -> attribute(any()).
width(Val) ->
    property(<<"width"/utf8>>, gleam@int:to_string(Val)).

-spec alt(binary()) -> attribute(any()).
alt(Text) ->
    attribute(<<"alt"/utf8>>, Text).

-spec autoplay(boolean()) -> attribute(any()).
autoplay(Should_autoplay) ->
    property(<<"autoplay"/utf8>>, Should_autoplay).

-spec controls(boolean()) -> attribute(any()).
controls(Visible) ->
    property(<<"controls"/utf8>>, Visible).

-spec loop(boolean()) -> attribute(any()).
loop(Should_loop) ->
    property(<<"loop"/utf8>>, Should_loop).
