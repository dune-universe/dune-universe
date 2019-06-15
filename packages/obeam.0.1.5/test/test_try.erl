-module(test_try).

-export([try_catch/0, try_of_catch/0, try_after/0,
         try_of_after/0, try_catch_after/0, try_of_catch_after/0]).

%% test case for try-catch expression
try_catch() ->
    try
        R = reason,
        error(R)
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        Err -> Err;
        error:Err -> Err
    end.

%% test case for try-of-catch expression
try_of_catch() ->
    try
        R = reason,
        error(R)
    of
        {ok, A} -> A;
        error -> error
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        Err -> Err;
        error:Err -> Err
    end.

%% test case for try-after expression
try_after() ->
    try
        R = reason,
        error(R)
    after
        Ok = ok,
        Ok
    end.

%% test case for try-of-after expression
try_of_after() ->
    try
        R = reason,
        error(R)
    of
        {ok, A} -> A;
        error -> error
    after
        Ok = ok,
        Ok
    end.

%% test case for try-catch-after expression
try_catch_after() ->
    try
        R = reason,
        error(R)
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        Err -> Err;
        error:Err -> Err
    after
        Ok = ok,
        Ok
    end.

%% test case for try-of-catch-after expression
try_of_catch_after() ->
    try
        R = reason,
        error(R)
    of
        {ok, A} -> A;
        error -> error
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        Err -> Err;
        error:Err -> Err
   after
        Ok = ok,
        Ok
    end.
