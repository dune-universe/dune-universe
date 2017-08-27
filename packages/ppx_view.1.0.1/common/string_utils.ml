let keywords = [
  "and"; "as"; "assert"; "asr";
  "begin";
  "class"; "constraint";
  "do"; "done"; "downto";
  "else"; "end"; "exception"; "external";
  "false"; "for"; "fun"; "function"; "functor";
  "if"; "in"; "include"; "inherit"; "initializer";
  "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor";
  "match"; "method"; "mod"; "module"; "mutable";
  "new"; "nonrec";
  "object"; "of"; "open"; "or";
  "private";
  "rec";
  "sig"; "struct";
  "then"; "to"; "true"; "try"; "type";
  "val"; "virtual";
  "when"; "while"; "with";
]

let safe_ident s =
  if List.mem s keywords then
    s ^ "_"
  else
    s

let safe_uncapitalize s =
  safe_ident (String.uncapitalize_ascii s)

let starts_with ~prefix s =
  let len_prefix = String.length prefix in
  let len_s = String.length s in
  if len_s >= len_prefix then begin
    let idx = ref 0 in
    while (!idx < len_prefix) && (prefix.[!idx] = s.[!idx]) do
      incr idx
    done;
    !idx = len_prefix
  end else
    false

let ends_with ~suffix s =
  let len_suffix = String.length suffix in
  let len_s = String.length s in
  if len_s >= len_suffix then begin
    let idx = ref (pred len_suffix) in
    while (!idx >= 0) && (suffix.[!idx] = s.[!idx + (len_s - len_suffix)]) do
      decr idx
    done;
    !idx < 0
  end else
    false
