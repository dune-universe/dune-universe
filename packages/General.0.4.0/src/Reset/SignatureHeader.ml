#define RESET_MODULE(m) module m: sig

#define RESET_VALUE(v, repl) val v: [`CONCAT(Please_use_General__, repl)]

#define RESET_TYPE(t, repl) type t = [`CONCAT(Please_use_General__, repl)]

#define ALIAS_MODULE(m) module m = m

#define EMPTY_MODULE(m) module m:sig end

#define ALIAS_VALUE(name, t, v) val name: t
