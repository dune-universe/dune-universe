(* {{{ COPYING *(

   Sexp by Frédéric Bour <frederic.bour(_)lakaban.net>

   To the extent possible under law, the person who associated CC0 with
   Sexp has waived all copyright and related or neighboring rights
   to Sexp.

   You should have received a copy of the CC0 legalcode along with this
   work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

)* }}} *)

type 'a sexp =
    C of 'a sexp * 'a sexp
  | S of string
  | T of string
  | P of 'a sexp
  | I of int
  | F of float
  | V of 'a sexp list
  | M of 'a

type void
let void (_ : void) = assert false

let transform_list ~inj ?(map=fun x -> x) t =
  let rec aux = function
    | S _ | T _ | I _ | F _ as t' -> map t'
    | C (a, b) -> map (C (aux a, aux_cons b))
    | P a -> map (P (aux a))
    | V xs -> V (List.map aux xs)
    | M x -> inj x
  and aux_cons = function
    | C (a, b) -> C (aux a, aux_cons b)
    | S "nil" as t -> t
    | t -> aux t
  in
  aux t

let transform_cons ~inj ?(map=fun x -> x) t =
  let rec aux = function
    | S _ | T _ | I _ | F _ as t' -> map t'
    | C (a, b) -> map (C (aux a, aux b))
    | P a -> map (P (aux a))
    | V xs -> V (List.map aux xs)
    | M x -> inj x
  in
  aux t

let generalize_basic x =
  transform_cons ~inj:void x

type basic = void sexp

let sym_t = S "t"
let sym_nil = S "nil"

let sexp_of_list ?(tail=sym_nil) l =
  let rec aux =  function
    | [] -> tail
    | a :: tl -> C (a, aux tl)
  in
  aux l

let rec sexp_mem x = function
  | C (x', _) when x = x' -> true
  | C (_, xs) -> sexp_mem x xs
  | _ -> false

let is_num c = (c >= '0' && c <= '9')

let escaped s =
  let count = ref 0 in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
    | '\n' | '"' | '\\' ->
      count := !count + 1
    | c when c < ' ' || c > '\x7F' ->
      if (i + 1 < len - 1) && is_num s.[i+1]
      then count := !count + 5
      else count := !count + 3
    | _ -> ()
  done;
  if !count = 0 then s
  else
    let s' = Bytes.create (len + !count) in
    let j = ref 0 in
    for i = 0 to len - 1 do
      match s.[i] with
      | '"' | '\\' as c ->
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) c;
        j := !j + 2
      | '\n' ->
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) 'n';
        j := !j + 2
      | c when c < ' ' || c > '\x7F' ->
        let c = Char.code c in
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) Char.(unsafe_chr (code '0' + (c / 64) land 0x7));
        Bytes.set s' (!j + 2) Char.(unsafe_chr (code '0' + ( c / 8) land 0x7));
        Bytes.set s' (!j + 3) Char.(unsafe_chr (code '0' + (     c) land 0x7));
        if (i + 1 < len - 1) && is_num s.[i+1] then (
          Bytes.set s' (!j + 4) '\\';
          Bytes.set s' (!j + 5) ' ';
          j := !j + 6
        )
        else j := !j + 4
      | c ->
        Bytes.set s' !j c;
        incr j
    done;
    Bytes.unsafe_to_string s'

let rec tell_sexp (tell : _ -> unit) sexp =
  match sexp with
  | C (a,b) ->
    tell "(";
    tell_sexp tell a;
    tell_cons tell b
  | T s -> tell ("\"" ^ escaped s ^ "\"")
  | S s -> tell (escaped s)
  | I i -> tell (string_of_int i)
  | F f -> tell (string_of_float f)
  | P s -> tell "#"; tell_sexp tell s
  | V [] -> tell "[]"
  | V (x :: xs) ->
    tell "[";
    tell_sexp tell x;
    List.iter (fun x' -> tell " "; tell_sexp tell x') xs;
    tell "]"
  | M v -> void v


and tell_cons tell = function
  | S "nil" -> tell ")"
  | C (a,b) ->
    tell " ";
    tell_sexp tell a;
    tell_cons tell b
  | sexp ->
    tell " . ";
    tell_sexp tell sexp;
    tell ")"

let is_alpha c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')

let is_num c =
  (c >= '0' && c <= '9')

let is_alphanum c = is_alpha c || is_num c

let read_sexp getch =
  let buf = Buffer.create 10 in
  let rec read_sexp = function
    | ' ' | '\t' | '\n' ->
      read_sexp (getch ())

    | c when is_num c ->
      read_num c

    | '\'' | ':' | '_' | '\\' as c -> read_sym c
    | c when is_alpha c -> read_sym c

    | '"' -> read_string ()

    | '\000' -> raise End_of_file

    | '(' -> read_cons [] (getch ())

    | '[' -> read_vector [] (getch ())

    | '#' ->
      let t, c = read_sexp (getch ()) in
      P t, c

    | _ -> failwith "Invalid parse"

  and read_cons cells = function
    | ' ' | '\t' | '\n' -> read_cons cells (getch ())
    | ')' -> sexp_of_list (List.rev cells), '\000'
    | '.' ->
      let rhs, c = read_sexp (getch ()) in
      let rec aux = function
        | ')' -> sexp_of_list ~tail:rhs (List.rev cells)
        | ' ' | '\t' | '\n' -> aux (getch ())
        | _ -> failwith "Invalid parse"
      in
      aux (if c = '\000' then getch() else c), '\000'
    | c ->
      let cell, c = read_sexp c in
      read_cons
        (cell :: cells)
        (if c = '\000' then getch() else c)

  and read_vector cells = function
    | ' ' | '\t' | '\n' -> read_vector cells (getch ())
    | ']' -> V (List.rev cells), '\000'
    | c ->
      let cell, c = read_sexp c in
      read_vector
        (cell :: cells)
        (if c = '\000' then getch() else c)

  and read_num c =
    Buffer.clear buf;
    Buffer.add_char buf c;
    let rec aux is_float =
      match getch () with
      | c when c >= '0' && c <= '9' ->
        Buffer.add_char buf c; aux is_float
      | '.' | 'e' | 'E' as c ->
        Buffer.add_char buf c; aux true
      | c ->
        let s = Buffer.contents buf in
        (if is_float
         then F (float_of_string s)
         else I (int_of_string s)),
        c
    in
    aux false

  and read_string () =
    Buffer.clear buf;
    let rec aux = function
      | '\000' -> failwith "Unterminated string"
      | '\\' ->
        begin match getch () with
          | 't' -> Buffer.add_char buf '\t'
          | 'r' -> Buffer.add_char buf '\r'
          | 'n' -> Buffer.add_char buf '\n'
          | ' ' -> ()
          | '0'..'9' as c0 ->
            let c0 = Char.code c0 - Char.code '0' in
            let c1 = Char.code (getch ()) - Char.code '0' in
            let c2 = Char.code (getch ()) - Char.code '0' in
            Buffer.add_char buf (Char.chr (c0 * 64 + c1 * 8 + c2))
          | c -> Buffer.add_char buf c
        end;
        aux (getch ())
      | '"' ->
        T (Buffer.contents buf), '\000'
      | c ->
        Buffer.add_char buf c;
        aux (getch ())
    in
    aux (getch ())

  and read_sym c =
    Buffer.clear buf;
    let rec aux = function
      | ('\'' | '-' | ':' | '_') as c ->
        Buffer.add_char buf c;
        aux (getch ())
      | c when is_alphanum c ->
        Buffer.add_char buf c;
        aux (getch ())
      | '\\' ->
        Buffer.add_char buf (getch ());
        aux (getch ())
      | c -> S (Buffer.contents buf), c
    in
    aux (if c = '\000' then getch() else c)
  in
  read_sexp (getch ())

let to_buf sexp buf =
  tell_sexp (Buffer.add_string buf) sexp

let to_string sexp =
  let buf = Buffer.create 100 in
  to_buf sexp buf;
  Buffer.contents buf

let getch_of_substring str pos len =
  let len = pos + len in
  if pos < 0 || len > String.length str then
    invalid_arg "Sexp.getch_of_substring";
  let pos = ref pos in
  let getch () =
    if !pos < len then
      let r = str.[!pos] in
      incr pos;
      r
    else '\000'
  in
  getch

let getch_of_string str =
  getch_of_substring str 0 (String.length str)

let of_string str =
  fst (read_sexp (getch_of_string str))

let of_file_descr ~on_read fd =
  let getch = ref (fun () -> '\000') in
  let rest = ref '\000' in
  let buffer = Bytes.create 1024 in
  let getch () =
    match !rest with
    | '\000' ->
      begin match !getch () with
        | '\000' ->
          (on_read fd : unit);
          let read = Unix.read fd buffer 0 1024 in
          if read = 0 then '\000'
          else
            begin
              getch := getch_of_substring
                  (Bytes.unsafe_to_string buffer) 0 read;
              !getch ()
            end
        | c -> c
      end
    | c -> rest := '\000'; c
  in
  fun () ->
    try
      let sexp, rest' = read_sexp getch in
      rest := rest';
      Some sexp
    with End_of_file -> None

let of_channel ic =
  let getch = ref (fun () -> '\000') in
  let rest = ref '\000' in
  let buffer = Bytes.create 1024 in
  let getch () =
    match !rest with
    | '\000' ->
      begin match !getch () with
        | '\000' ->
          let read = input ic buffer 0 1024 in
          if read = 0 then '\000'
          else
            begin
              getch := getch_of_substring
                  (Bytes.unsafe_to_string buffer) 0 read;
              !getch ()
            end
        | c -> c
      end
    | c -> rest := '\000'; c
  in
  fun () ->
    try
      let sexp, rest' = read_sexp getch in
      rest := rest';
      Some sexp
    with End_of_file -> None
