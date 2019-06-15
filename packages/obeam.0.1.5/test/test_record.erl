-module(test_record).

%% test case for record
-record(r, {a,
            b = 42,
            c :: string(),
            d = 57 :: integer()}).

-export([f/0, g/1]).

f() ->
    R = #r{a = 3, c = "hello"}, % Record Creation
    _ = R#r.c,                  % Record Field Access
    Index = #r.b,               % Record Field Index
    R#r{a = 100, c = "hoge"}.   % Record Update

g(R) ->
    _ =
        case R of
            _ when #r{a = true, _ = 111} -> foo; % Record Creation in Guard test
            _ when R#r.b -> bar;                 % Record Field Access in Guard test
            _ when #r.c -> baz                   % Record Field Index in Guard test
        end,
    _ =
        case R of
            #r.a -> foo;                 % Record Field Index Pattern
            #r{a = true, _ = 111} -> bar % Record Pattern
        end,
    ok.
