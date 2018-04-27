#define RESET_MODULE(m) module m = struct

#define RESET_VALUE(v, repl) let v = `CONCAT(Please_use_General__, repl)

#define RESET_TYPE(t, repl) type t = [`CONCAT(Please_use_General__, repl)]

#define ALIAS_MODULE(m) module m = m

#define EMPTY_MODULE(m) module m = struct end

#define ALIAS_VALUE(name, t, v) let name = v
