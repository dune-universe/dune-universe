-module(test_user_type).

-export_type([b/0]).

%% test case for user_type
-type a() :: term().
-type b() :: a().
