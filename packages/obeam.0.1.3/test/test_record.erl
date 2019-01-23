-module(test_record).

%% test case for record decl
-record(r, {a,
            b = 42,
            c :: string(),
            d = 57 :: integer()}).
