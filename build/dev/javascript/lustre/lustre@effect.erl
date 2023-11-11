-module(lustre@effect).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([from/1, event/2, none/0, batch/1, map/2]).
-export_type([effect/1]).

-opaque effect(GHW) :: {effect,
        list(fun((fun((GHW) -> nil), fun((binary(), gleam@dynamic:dynamic_()) -> nil)) -> nil))}.

-spec from(fun((fun((GHX) -> nil)) -> nil)) -> effect(GHX).
from(Effect) ->
    {effect, [fun(Dispatch, _) -> Effect(Dispatch) end]}.

-spec event(binary(), any()) -> effect(any()).
event(Name, Data) ->
    {effect, [fun(_, Emit) -> Emit(Name, gleam@dynamic:from(Data)) end]}.

-spec none() -> effect(any()).
none() ->
    {effect, []}.

-spec batch(list(effect(GIE))) -> effect(GIE).
batch(Effects) ->
    {effect,
        (gleam@list:fold(
            Effects,
            [],
            fun(B, _use1) ->
                {effect, A} = _use1,
                gleam@list:append(B, A)
            end
        ))}.

-spec map(effect(GII), fun((GII) -> GIK)) -> effect(GIK).
map(Effect, F) ->
    {effect,
        (gleam@list:map(
            erlang:element(2, Effect),
            fun(Effect@1) ->
                fun(Dispatch, Emit) ->
                    Dispatch@1 = gleam@function:compose(F, Dispatch),
                    Effect@1(Dispatch@1, Emit)
                end
            end
        ))}.
