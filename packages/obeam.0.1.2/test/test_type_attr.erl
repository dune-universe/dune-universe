-module(test_type_attr).

-export_type([tuple/2, int/0]).

%% test case for type decl
-type tuple(A, B) :: {A, B}.

%% test case for opaque decl
-opaque int() :: integer().
