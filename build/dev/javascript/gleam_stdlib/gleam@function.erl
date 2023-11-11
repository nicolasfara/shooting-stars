-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((ETS) -> ETT), fun((ETT) -> ETU)) -> fun((ETS) -> ETU).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((ETV, ETW) -> ETX)) -> fun((ETV) -> fun((ETW) -> ETX)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((ETZ, EUA, EUB) -> EUC)) -> fun((ETZ) -> fun((EUA) -> fun((EUB) -> EUC))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EUE, EUF, EUG, EUH) -> EUI)) -> fun((EUE) -> fun((EUF) -> fun((EUG) -> fun((EUH) -> EUI)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EUK, EUL, EUM, EUN, EUO) -> EUP)) -> fun((EUK) -> fun((EUL) -> fun((EUM) -> fun((EUN) -> fun((EUO) -> EUP))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EUR, EUS, EUT, EUU, EUV, EUW) -> EUX)) -> fun((EUR) -> fun((EUS) -> fun((EUT) -> fun((EUU) -> fun((EUV) -> fun((EUW) -> EUX)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EUZ, EVA) -> EVB)) -> fun((EVA, EUZ) -> EVB).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EVC) -> EVC.
identity(X) ->
    X.

-spec constant(EVD) -> fun((any()) -> EVD).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EVF, fun((EVF) -> any())) -> EVF.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EVH) -> EVI), EVH) -> EVI.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EVJ, EVK) -> EVL), EVJ, EVK) -> EVL.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EVM, EVN, EVO) -> EVP), EVM, EVN, EVO) -> EVP.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
