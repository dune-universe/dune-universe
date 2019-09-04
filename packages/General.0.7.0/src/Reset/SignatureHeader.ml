#define RESET_MODULE(m) module m: sig

#define RESET_VALUE(v, repl) val v: [`CONCAT(Please_use_General__, repl)]

#define RESET_TYPE(t, repl) type t = [`CONCAT(Please_use_General__, repl)]

#define ALIAS_MODULE(m) module m = m

#ifdef HAS_Stdlib
#define ALIAS_STDLIB_MODULE(m) module m = Stdlib.m
#else
#define ALIAS_STDLIB_MODULE(m) module m = m
#endif

#define EMPTY_MODULE(m) module m: sig end

#define ALIAS_VALUE(name, t, v) val name: t
