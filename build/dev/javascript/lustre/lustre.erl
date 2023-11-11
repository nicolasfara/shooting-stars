-module(lustre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([application/3, element/1, simple/3, component/5, start/3, destroy/1, is_browser/0, is_registered/1]).
-export_type([app/3, error/0]).

-opaque app(GPX, GPY, GPZ) :: app | {gleam_phantom, GPX, GPY, GPZ}.

-type error() :: app_already_started |
    app_not_yet_started |
    bad_component_name |
    component_already_registered |
    element_not_found |
    not_a_browser.

-spec application(
    fun((GQM) -> {GQN, lustre@effect:effect(GQO)}),
    fun((GQN, GQO) -> {GQN, lustre@effect:effect(GQO)}),
    fun((GQN) -> lustre@element:element(GQO))
) -> app(GQM, GQN, GQO).
application(_, _, _) ->
    gleam@dynamic:unsafe_coerce(gleam@dynamic:from(nil)).

-spec element(lustre@element:element(GQA)) -> app(nil, nil, GQA).
element(Element) ->
    Init = fun(_) -> {nil, lustre@effect:none()} end,
    Update = fun(_, _) -> {nil, lustre@effect:none()} end,
    View = fun(_) -> Element end,
    application(Init, Update, View).

-spec simple(
    fun((GQF) -> GQG),
    fun((GQG, GQH) -> GQG),
    fun((GQG) -> lustre@element:element(GQH))
) -> app(GQF, GQG, GQH).
simple(Init, Update, View) ->
    Init@1 = fun(Flags) -> {Init(Flags), lustre@effect:none()} end,
    Update@1 = fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
    application(Init@1, Update@1, View).

-spec component(
    binary(),
    fun(() -> {GQV, lustre@effect:effect(GQW)}),
    fun((GQV, GQW) -> {GQV, lustre@effect:effect(GQW)}),
    fun((GQV) -> lustre@element:element(GQW)),
    gleam@map:map_(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, GQW} |
        {error, list(gleam@dynamic:decode_error())}))
) -> {ok, nil} | {error, error()}.
component(_, _, _, _, _) ->
    {ok, nil}.

-spec start(app(GRF, any(), GRH), binary(), GRF) -> {ok, fun((GRH) -> nil)} |
    {error, error()}.
start(_, _, _) ->
    {error, not_a_browser}.

-spec destroy(app(any(), any(), any())) -> {ok, nil} | {error, error()}.
destroy(_) ->
    {ok, nil}.

-spec is_browser() -> boolean().
is_browser() ->
    false.

-spec is_registered(binary()) -> boolean().
is_registered(_) ->
    false.
