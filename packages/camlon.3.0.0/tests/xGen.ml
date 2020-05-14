open QCheck.Gen

let rec filter p at = 
  at >>= fun a ->
  if p a then return a else filter p at
  
let alpha = 
  map Char.chr 
  @@ oneof [ int_range (Char.code 'a') (Char.code 'z')
           ; int_range (Char.code 'A') (Char.code 'Z') ]

let lower_alpha = map Char.chr @@ int_range (Char.code 'a') (Char.code 'z')

let upper_alpha = map Char.chr @@ int_range (Char.code 'A') (Char.code 'Z')

let alphanumeral = frequency [ 26, alpha; 10, numeral ]

let char_of_digit x = 
  assert (x >= 0 && x < 10);
  Char.chr @@ Char.code '0' + x

(** ['a'-'z' '_'] *)
let lowercase = 
  frequency [ 26, lower_alpha
            ; 1,  return '_' ]

let uppercase = upper_alpha

(** ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9'] *)
let identchar = 
  frequency [ 26, uppercase
            ; 26, lowercase
            ; 1, return '_'
            ; 1, return '\''
            ; 10, numeral ]
    
(** ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *)
let symbolchar = 
  oneofl ['!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~']
    
(** ['0'-'9'] ['0'-'9' '_']* *)
let decimal_literal sz =
  numeral >>= fun d ->
  sz >>= fun n ->
  string_size ~gen:(frequency [10, numeral; 1, return '_']) (return (max 0 (n-1))) >>= fun s ->
  return @@ String.make 1 d ^ s
  
(** ['0'-'9' 'A'-'F' 'a'-'f'] *)
let hex_digit = frequency [10, numeral
                          ; 6, (int_range 0 5 >>= fun i ->
                                bool >>= fun b ->
                                return @@ Char.chr (Char.code (if b then 'A' else 'a') + i)) ]

(** '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']* *)
let hex_literal sz =
  let sz = sz >>= fun i -> return (max 0 (i-3)) in
  oneofl ['x'; 'X'] >>= fun x ->
  hex_digit >>= fun first ->
  string_size ~gen:(frequency [ 16, hex_digit; 1, return '_' ]) sz >>= fun body ->
  return ("0" ^ String.make 1 x ^ String.make 1 first ^ body)

(** '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']* *)
let oct_literal sz =
  let sz = sz >>= fun i -> return (max 0 (i-3)) in
  oneofl ['o'; 'O'] >>= fun o ->
  int_bound 7 >>= fun first ->
  string_size ~gen:(frequency [ 8, map char_of_digit @@ int_bound 7
                              ; 1, return '_']) sz >>= fun body ->
  return ("0" ^ String.make 1 o ^ String.make 1 (char_of_digit first) ^ body)

(** '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']* *)
let bin_literal sz =
  let sz = sz >>= fun i -> return (max 0 (i-3)) in
  oneofl ['b'; 'B'] >>= fun b ->
  oneofl ['0'; '1'] >>= fun first ->
  string_size ~gen:(frequency [ 10, oneofl ['0'; '1']
                              ; 1, return '_']) sz >>= fun body ->
  return ("0" ^ String.make 1 b ^ String.make 1 first ^ body)

let int_literal sz = frequency [ 10, decimal_literal sz
                               ; 5, hex_literal sz
                               ; 2, oct_literal sz
                               ; 2, bin_literal sz ]

let literal_modifier = 
  (* exclude l, L, n *)
  filter (function 'l' | 'L' | 'n' -> false | _ -> true)
  @@
  map Char.chr 
  @@ oneof [ int_range (Char.code 'g') (Char.code 'z')
           ; int_range (Char.code 'G') (Char.code 'Z')]

(*
  let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
   *)

let lident = 
  let p x = x <> "_" && not @@ Hashtbl.mem Camlon.Lexer.keyword_table x in
  filter p (lowercase >>= fun c -> 
            string_size ~gen:identchar (int_bound 16) >>= fun s -> 
            return (String.make 1 c ^ s))

let uident = 
  uppercase >>= fun c -> 
  string_size ~gen:identchar (int_bound 16) >>= fun s -> 
  return (String.make 1 c ^ s)

let ident = oneof [ lident; uident ]

open Typerep_lib.Std
open Ppx_sexp_conv_lib.Conv

type int_literal = string [@@deriving typerep, sexp]
type literal_modifier = char [@@deriving typerep, sexp]
type lident = string [@@deriving typerep, sexp]
type uident = string [@@deriving typerep, sexp]
type ident = string [@@deriving typerep, sexp]

type 'a non_empty_list = 'a list [@@deriving typerep, sexp]

let override_non_empty_list (module M : QCheck_of_typerep.Gen.S) =
  M.register1 (module struct
    type 'a t = 'a non_empty_list
    let typerep_of_t = typerep_of_non_empty_list
    let typename_of_t = typename_of_non_empty_list
    let compute f ex sz =
      let open QCheck.Gen in
      list_size (int_range 1 (max sz 1 * 3)) (f ex (sz - 1)) (* XXX hard coded param *)
  end)

type 'a two_or_more_list = 'a list [@@deriving typerep, sexp]

let override_two_or_more_list (module M : QCheck_of_typerep.Gen.S) =
  M.register1 (module struct
    type 'a t = 'a two_or_more_list
    let typerep_of_t = typerep_of_two_or_more_list
    let typename_of_t = typename_of_two_or_more_list
    let compute f ex sz =
      let open QCheck.Gen in
      list_size (int_range 2 (max sz 2 * 3)) (f ex (sz - 1)) (* XXX hard coded param *)
  end)

let override (module M : QCheck_of_typerep.Gen.S) =
  M.register typerep_of_int_literal (fun _ _ -> int_literal (int_bound 16));
  M.register typerep_of_literal_modifier (fun _ _ -> literal_modifier);
  M.register typerep_of_lident (fun _ _ -> lident);
  M.register typerep_of_uident (fun _ _ -> uident);
  M.register typerep_of_ident (fun _ _ -> ident);
  override_non_empty_list (module M);
  override_two_or_more_list (module M)
  

