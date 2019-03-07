-module(test_type_attr).

-export_type([tuple/2, int/0]).

%% test case for type decl
-type tuple(A, B) :: {A, B}.

%% test case for opaque decl
-opaque int() :: integer().

%% test case for record type and record field type
-record(state, {name :: string(), param :: term()}).
-type record_as_type(A) :: #state{param :: A}.

%% test case for remote type
-type some_remote(A) :: dict:dict(integer(), A).
