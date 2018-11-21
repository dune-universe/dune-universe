-module(test05).

-record(r, {a,
            b = 42,
            c :: string(),
            d = 57 :: integer()}).
