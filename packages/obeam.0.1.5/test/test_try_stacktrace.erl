-module(test_try_stacktrace).

-export([catch_stacktrace/0]).

-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new catch clause syntax or not.
-define(STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE, ; Class:Err:Stacktrace when Err =:= error -> {Class, Err, Stacktrace}).
-define(STACKTRACE_CLAUSE, ; Class:Err:Stacktrace -> {Class, Err, Stacktrace}).
-else.
-define(STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE, ).
-define(STACKTRACE_CLAUSE, ).
-endif.

%% test case for catch clause with stacktrace (>= OTP-21)
catch_stacktrace() ->
    try
        error
    catch
        dummy -> dummy
        ?STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE
        ?STACKTRACE_CLAUSE
    end.
