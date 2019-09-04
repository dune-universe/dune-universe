#define RESET_MODULE(m) module m = struct

#define RESET_VALUE(v, repl) let v = `CONCAT(Please_use_General__, repl)

#define RESET_TYPE(t, repl) type t = [`CONCAT(Please_use_General__, repl)]

#define ALIAS_MODULE(m) module m = m

#ifdef HAS_Stdlib
#define ALIAS_STDLIB_MODULE(m) module m = Stdlib.m
#else
#define ALIAS_STDLIB_MODULE(m) module m = m
#endif

#define EMPTY_MODULE(m) module m = struct end

#define ALIAS_VALUE(name, t, v) let name = v
