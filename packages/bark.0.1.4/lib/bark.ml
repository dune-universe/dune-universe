(* Helpers *)

let slice : int -> int -> string -> string =
  fun lo hi s ->
    String.sub s lo (hi - lo)

let uncons : string -> (char * string) option =
  fun s ->
    let len =
      String.length s
    in
    if len = 0 then
      None
    else
      Some (String.get s 0, String.sub s 1 (len - 1))

(* Character Predicates *)
(* Source: https://stackoverflow.com/a/49184157/1157526 *)

let is_alpha : char -> bool =
  function
    | 'a' .. 'z'
    | 'A' .. 'Z' ->
        true

    | _ ->
        false

let is_num : char -> bool =
  function
    | '0' .. '9' ->
        true

    | _ ->
        false

(* Low-Level Helpers *)

let is_sub_string : string -> int -> int -> int -> string -> int * int * int =
  fun small_string offset row col big_string ->
    let small_length =
      String.length small_string
    in
    let (offset', row', col') =
      (ref offset, ref row, ref col)
    in
    let is_good =
      ref (offset + small_length <= String.length big_string)
    in
    let i =
      ref 0
    in
    while (!is_good && !i < small_length) do
      let big_letter =
        String.get big_string !offset'
      in
      is_good :=
        String.get small_string !i = big_letter;
      i :=
        !i + 1;
      offset' :=
        !offset' + 1;
      begin if big_letter = '\n' then
        begin
          row' := !row' + 1;
          col' := 1
        end
      else
        begin
          col' := !col' + 1
        end
      end
    done;
    ( (if !is_good then !offset' else -1)
    , !row'
    , !col'
    )

let is_sub_char : (char -> bool) -> int -> string -> int =
  fun predicate offset string ->
    if String.length string <= offset then
      -1
    else if predicate (String.get string offset) then
      if String.get string offset = '\n' then
        -2
      else
        offset + 1
    else
      -1

let find_sub_string : string -> int -> int -> int -> string -> int * int * int =
  fun small_string offset row col big_string ->
    let small_re =
      Str.regexp_string small_string
    in
    let new_offset =
      try
        Str.search_forward small_re big_string offset
      with
        Not_found ->
          -1
    in
    let target =
      if new_offset < 0 then
        String.length big_string
      else
        new_offset + String.length small_string
    in
    let (offset', row', col') =
      (ref offset, ref row, ref col)
    in
    while (!offset' < target) do
      offset' :=
        !offset' + 1;
      begin if String.get big_string !offset' = '\n' then
        begin
          row' := !row' + 1;
          col' := 1
        end
      else
        begin
          col' := !col' + 1
        end
      end
    done;
    ( new_offset
    , !row'
    , !col'
    )

(* Parsers *)

type 'context located =
  { row : int
  ; col : int
  ; context : 'context
  }

type 'context state =
  { src : string
  ; offset : int
  ; indent : int
  ; context : 'context located list
  ; row : int
  ; col :int
  }

type ('context, 'problem) dead_end =
  { row : int
  ; col : int
  ; problem : 'problem
  ; context_stack : 'context located list
  }

type ('c, 'x) bag =
  | Empty
  | AddRight of ('c, 'x) bag * ('c, 'x) dead_end
  | Append of ('c, 'x) bag * ('c, 'x) bag

type ('context, 'problem, 'value) pstep =
  | Good of bool * 'value * 'context state
  | Bad of bool * ('context, 'problem) bag

type ('context, 'problem, 'value) parser =
  'context state -> ('context, 'problem, 'value) pstep

(* Problems *)

let from_state : 'c state -> 'x -> ('c, 'x) bag =
  fun s x ->
    AddRight
      ( Empty
      , { row = s.row
        ; col = s.col
        ; problem = x
        ; context_stack = s.context
        }
      )

let from_info : int -> int -> 'x -> 'c located list -> ('c, 'x) bag =
  fun row col x context ->
    AddRight
      ( Empty
      , { row = row
        ; col = col
        ; problem = x
        ; context_stack = context
        }
      )

let rec bag_to_list :
  ('c, 'x) bag -> ('c, 'x) dead_end list -> ('c, 'x) dead_end list =
    fun bag ls ->
      match bag with
        | Empty ->
            ls

        | AddRight (bag1, x) ->
            bag_to_list bag1 (x :: ls)

        | Append (bag1, bag2) ->
            bag_to_list bag1 (bag_to_list bag2 ls)

(* Run *)

let run : ('c, 'x, 'a) parser -> string -> ('a, ('c, 'x) dead_end list) result =
  fun parse src ->
    match
      parse
        { src = src
        ; offset = 0
        ; indent = 1
        ; context = []
        ; row = 1
        ; col = 1
        }
    with
      | Good (_, value, _) ->
          Ok value

      | Bad (_, bag) ->
          Error (bag_to_list bag [])

(* Primitives *)

let succeed : 'a -> ('c, 'x, 'a) parser =
  fun a ->
    fun s -> Good (false, a, s)

let problem : 'x -> ('c, 'x, 'a) parser =
  fun x ->
    fun s -> Bad (false, from_state s x)

(* Mapping *)

let map : ('a -> 'b) -> ('c, 'x, 'a) parser -> ('c, 'x, 'b) parser =
  fun func parse ->
    fun s0 ->
      match parse s0 with
        | Good (p, a, s1) ->
            Good (p, func a, s1)

        | Bad (p, x) ->
            Bad (p, x)

let map2 :
 ('a -> 'b -> 'value) ->
 ('c, 'x, 'a) parser ->
 ('c, 'x, 'b) parser ->
 ('c, 'x, 'value) parser =
  fun func parse_a parse_b ->
    fun s0 ->
      match parse_a s0 with
        | Bad (p, x) ->
            Bad (p, x)

        | Good (p1, a, s1) ->
            begin match parse_b s1 with
              | Bad (p2, x) ->
                  Bad (p1 || p2, x)

              | Good (p2, b, s2) ->
                  Good (p1 || p2, func a b, s2)
            end

let keeper :
 ('c, 'x, 'a -> 'b) parser ->
 ('c, 'x, 'a) parser ->
 ('c, 'x, 'b) parser =
  fun parse_func parse_arg ->
    map2 (@@) parse_func parse_arg

let ignorer :
 ('c, 'x, 'keep) parser ->
 ('c, 'x, 'ignore) parser ->
 ('c, 'x, 'keep) parser =
  fun keep_parser ignore_parser ->
    map2 (fun k _ -> k) keep_parser ignore_parser

(* And Then *)

let and_then :
 ('a -> ('c, 'x, 'b) parser) ->
 ('c, 'x, 'a) parser ->
 ('c, 'x, 'b) parser =
  fun callback parse_a ->
    fun s0 ->
      match parse_a s0 with
        | Bad (p, x) ->
            Bad (p, x)

        | Good (p1, a, s1) ->
            let parse_b =
              callback a
            in
            begin match parse_b s1 with
              | Bad (p2, x) ->
                  Bad (p1 || p2, x)

              | Good (p2, b, s2) ->
                  Good (p1 || p2, b, s2)
            end

(* Lazily *)

let lazily : (unit -> ('c, 'x, 'a) parser) -> ('c, 'x, 'a) parser =
  fun thunk ->
    fun s ->
      let parse =
        thunk ()
      in
      parse s

(* One Of *)

let rec one_of_help :
 'c state ->
 ('c, 'x) bag ->
 ('c, 'x, 'a) parser list ->
 ('c, 'x, 'a) pstep =
  fun s0 bag parsers ->
    match parsers with
      | [] ->
          Bad (false, bag)

      | parse :: remaining_parsers ->
          begin match parse s0 with
            | Good (_, _, _) as step ->
                step

            | Bad (p, x) as step ->
                if p then
                  step
                else
                  one_of_help s0 (Append (bag, x)) remaining_parsers
          end

let one_of : ('c, 'x, 'a) parser list -> ('c, 'x, 'a) parser =
  fun parsers ->
    fun s ->
      one_of_help s Empty parsers

(* Loop *)

type ('state, 'a) step =
  | Loop of 'state
  | Done of 'a

let rec loop_help :
 bool ->
 'state ->
 ('state -> ('c, 'x, ('state, 'a) step) parser) ->
 'c state ->
 ('c, 'x, 'a) pstep =
  fun p state callback s0 ->
    let parse =
      callback state
    in
    match parse s0 with
      | Good (p1, step, s1) ->
          begin match step with
            | Loop new_state ->
                loop_help (p || p1) new_state callback s1

            | Done result ->
                Good (p || p1, result, s1)
          end

      | Bad (p1, x) ->
          Bad (p || p1, x)

let loop :
 'state ->
 ('state -> ('c, 'x, ('state, 'a) step) parser) ->
 ('c, 'x, 'a) parser =
  fun state callback ->
    fun s ->
      loop_help false state callback s

(* Backtrackable *)

let backtrackable : ('c, 'x, 'a) parser -> ('c, 'x, 'a) parser =
  fun parse ->
    fun s0 ->
      match parse s0 with
        | Bad (_, x) ->
            Bad (false, x)

        | Good (_, a, s1) ->
            Good (false, a, s1)

let commit : 'a -> ('c, 'x, 'a) parser =
  fun a ->
    fun s ->
      Good (true, a, s)

(* Token *)

type 'x token =
  | Token of string * 'x

let token : 'x token -> ('c, 'x, unit) parser =
  fun (Token (str, expecting)) ->
    let progress =
      str <> ""
    in
    fun s ->
      let (new_offset, new_row, new_col) =
        is_sub_string str s.offset s.row s.col s.src
      in
      if new_offset = -1 then
        Bad (false, from_state s expecting)
      else
        Good
          ( progress
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = new_row
            ; col = new_col
            }
          )

(* Symbol *)

let symbol : 'x token -> ('c, 'x, unit) parser =
  token

(* Keyword *)

let keyword : 'x token -> ('c, 'x, unit) parser =
  fun (Token (kwd, expecting)) ->
    let progress =
      kwd <> ""
    in
    fun s ->
      let (new_offset, new_row, new_col) =
        is_sub_string kwd s.offset s.row s.col s.src
      in
      if
        new_offset = -1 ||
        0 <= is_sub_char
               (fun c -> is_alpha c || is_num c || c = '_')
               new_offset
               s.src
      then
        Bad (false, from_state s expecting)
      else
        Good
          ( progress
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = new_row
            ; col = new_col
            }
          )

(* End *)

let endd : 'x -> ('c, 'x, unit) parser =
  fun x ->
    fun s ->
      if String.length s.src = s.offset then
        Good (false, (), s)
      else
        Bad (false, from_state s x)

(* Chomped Strings *)

let map_chomped_string :
 (string -> 'a -> 'b) -> ('c, 'x, 'a) parser -> ('c, 'x, 'b) parser =
  fun func parse ->
    fun s0 ->
      match parse s0 with
        | Bad (p, x) ->
            Bad (p, x)

        | Good (p, a, s1) ->
            Good (p, func (slice s0.offset s1.offset s0.src) a, s1)

let get_chomped_string : ('c, 'x, 'a) parser -> ('c, 'x, string) parser =
  fun parse ->
    map_chomped_string (fun s _ -> s) parse

(* Chomp If *)

let chomp_if : (char -> bool) -> 'x -> ('c, 'x, unit) parser =
  fun is_good expecting ->
    fun s ->
      let new_offset =
        is_sub_char is_good s.offset s.src
      in
      if new_offset = -1 then
        Bad (false, from_state s expecting)

      else if new_offset = -2 then
        Good
          ( true
          , ()
          , { src = s.src
            ; offset = s.offset + 1
            ; indent = s.indent
            ; context = s.context
            ; row = s.row + 1
            ; col = 1
            }
          )

      else
        Good
          ( true
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = s.row
            ; col = s.col + 1
            }
          )

(* Chomp While *)

let rec chomp_while_help :
 (char -> bool) ->
 int -> int -> int ->
 'c state ->
 ('c, 'x, unit) pstep =
  fun is_good offset row col s0 ->
    let new_offset =
      is_sub_char is_good offset s0.src
    in
    if new_offset = -1 then
      Good
        ( s0.offset < offset
        , ()
        , { src = s0.src
          ; offset = offset
          ; indent = s0.indent
          ; context = s0.context
          ; row = row
          ; col = col
          }
        )

    else if new_offset = -2 then
      chomp_while_help is_good (offset + 1) (row + 1) 1 s0

    else
      chomp_while_help is_good new_offset row (col + 1) s0

let chomp_while : (char -> bool) -> ('c, 'x, unit) parser =
  fun is_good ->
    fun s ->
      chomp_while_help is_good s.offset s.row s.col s

(* Chomp Until *)

let chomp_until : 'x token -> ('c, 'x, unit) parser =
  fun (Token (str, expecting)) ->
    fun s ->
      let (new_offset, new_row, new_col) =
        find_sub_string str s.offset s.row s.col s.src
      in
      if new_offset = -1 then
        Bad (false, from_info new_row new_col expecting s.context)

      else
        Good
          ( s.offset < new_offset
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = new_row
            ; col = new_col
            }
          )

let chomp_until_end_or : string -> ('c, 'x, unit) parser =
  fun str ->
    fun s ->
      let (new_offset, new_row, new_col) =
        find_sub_string str s.offset s.row s.col s.src
      in
      let adjusted_offset =
        if new_offset < 0 then
          String.length s.src
        else
          new_offset
      in
      Good
        ( s.offset < adjusted_offset
        , ()
        , { src = s.src
          ; offset = adjusted_offset
          ; indent = s.indent
          ; context = s.context
          ; row = new_row
          ; col = new_col
          }
        )

(* Context *)

let change_context : 'c located list -> 'c state -> 'c state =
  fun new_context s ->
    { src = s.src
    ; offset = s.offset
    ; indent = s.indent
    ; context = new_context
    ; row = s.row
    ; col = s.col
    }

let in_context :
 'context ->
 ('context, 'x, 'a) parser ->
 ('context, 'x, 'a) parser =
  fun context parse ->
    fun s0 ->
      match
        parse @@
          change_context
            ( { row = s0.row
              ; col = s0.col
              ; context = context
              } :: s0.context
            )
            s0
      with
        | Good (p, a, s1) ->
            Good (p, a, change_context s0.context s1)

        | Bad (_, _) as step ->
            step

(* Indentation *)

let get_indent : ('c, 'x, int) parser =
  fun s ->
    Good (false, s.indent, s)

let change_indent : int -> 'c state -> 'c state =
  fun new_indent s ->
    { src = s.src
    ; offset = s.offset
    ; indent = new_indent
    ; context = s.context
    ; row = s.row
    ; col = s.col
    }

let with_indent : int -> ('c, 'x, 'a) parser -> ('c, 'x, 'a) parser =
  fun new_indent parse ->
    fun s0 ->
      match parse (change_indent new_indent s0) with
        | Good (p, a, s1) ->
            Good (p, a, change_indent s0.indent s1)

        | Bad (p, x) ->
            Bad (p, x)

(* Position *)

let get_position : ('c, 'x, int * int) parser =
  fun s ->
    Good (false, (s.row, s.col), s)

let get_row : ('c, 'x, int) parser =
  fun s ->
    Good (false, s.row, s)

let get_col : ('c, 'x, int) parser =
  fun s ->
    Good (false, s.col, s)

let get_offset : ('c, 'x, int) parser =
  fun s ->
    Good (false, s.offset, s)

let get_source : ('c, 'x, string) parser =
  fun s ->
    Good (false, s.src, s)

(* Variables *)

let rec var_help :
  (char -> bool) ->
  int -> int -> int ->
  string ->
  int ->
  'c located list ->
  'c state =
    fun is_good offset row col src indent context ->
      let new_offset =
        is_sub_char is_good offset src
      in
      if new_offset = -1 then
        { src = src
        ; offset = offset
        ; indent = indent
        ; context = context
        ; row = row
        ; col = col
        }

      else if new_offset = -2 then
        var_help is_good (offset + 1) (row + 1) 1 src indent context

      else
        var_help is_good new_offset row (col + 1) src indent context

module String_set =
  Set.Make(String)

let variable ~start ~inner ~reserved ~expecting =
  fun s ->
    let first_offset =
      is_sub_char start s.offset s.src
    in
    if first_offset = -1 then
      Bad (false, from_state s expecting)
    else
      let s1 =
        if first_offset = -2 then
          var_help
            inner (s.offset + 1) (s.row + 1) 1 s.src s.indent s.context
        else
          var_help
            inner first_offset s.row (s.col + 1) s.src s.indent s.context
      in
      let name =
        slice s.offset s1.offset s.src
      in
      if String_set.mem name reserved then
        Bad (false, from_state s expecting)
      else
        Good (true, name, s1)

(* Sequences *)

let skip :
 ('c, 'x, 'ignore) parser ->
 ('c, 'x, 'keep) parser ->
 ('c, 'x, 'keep) parser =
  fun ignore_parser keep_parser ->
    map2 (fun _ k -> k) ignore_parser keep_parser

type trailing =
  | Forbidden
  | Optional
  | Mandatory

let sequence_end_forbidden :
 ('c, 'x, unit) parser ->
 ('c, 'x, unit) parser ->
 ('c, 'x, 'a) parser ->
 ('c, 'x, unit) parser ->
 'a list ->
 ('c, 'x, ('a list, 'a list) step) parser =
  fun ender ws parse_item sep rev_items ->
    skip ws @@
      one_of
        [ skip sep @@ skip ws @@
            map (fun item -> Loop (item :: rev_items)) parse_item
        ; ender |> map (fun _ -> Done (List.rev rev_items))
        ]

let sequence_end_optional :
 ('c, 'x, unit) parser ->
 ('c, 'x, unit) parser ->
 ('c, 'x, 'a) parser ->
 ('c, 'x, unit) parser ->
 'a list ->
 ('c, 'x, ('a list, 'a list) step) parser =
  fun ender ws parse_item sep rev_items ->
    let parse_end =
      map (fun _ -> Done (List.rev rev_items)) ender
    in
    skip ws @@
      one_of
        [ skip sep @@ skip ws @@
            one_of
              [ parse_item |> map (fun item -> Loop (item :: rev_items))
              ; parse_end
              ]
        ; parse_end
        ]

let sequence_end_mandatory :
 ('c, 'x, unit) parser ->
 ('c, 'x, 'a) parser ->
 ('c, 'x, unit) parser ->
 'a list ->
 ('c, 'x, ('a list, 'a list) step) parser =
  fun ws parse_item sep rev_items ->
    one_of
      [ map (fun item -> Loop (item :: rev_items)) @@
          ignorer parse_item (ignorer ws (ignorer sep ws))
      ; map (fun _ -> Done (List.rev rev_items)) (succeed ())
      ]


let sequence_end :
 ('c, 'x, unit) parser ->
 ('c, 'x, unit) parser ->
 ('c, 'x, 'a) parser ->
 ('c, 'x, unit) parser ->
 trailing ->
 ('c, 'x, 'a list) parser =
  fun ender ws parse_item sep trailing ->
    let chomp_rest item =
      match trailing with
        | Forbidden ->
            loop [item] (sequence_end_forbidden ender ws parse_item sep)

        | Optional ->
            loop [item] (sequence_end_optional ender ws parse_item sep)

        | Mandatory ->
            ignorer
              ( skip ws @@ skip sep @@ skip ws @@
                  loop [item] (sequence_end_mandatory ws parse_item sep)
              )
            ender
    in
      one_of
        [ parse_item |> and_then chomp_rest
        ; ender |> map (fun _ -> [])
        ]

let sequence ~start ~separator ~endd ~spaces ~item ~trailing =
  skip (token start) @@
  skip spaces @@
    sequence_end (token endd) spaces item (token separator) trailing

(* Whitespace *)

let spaces : ('c, 'x, unit) parser =
  fun s ->
    chomp_while (fun c -> c = ' ' || c = '\n' || c = '\r') s

let line_comment : 'x token -> ('c, 'x, unit) parser =
  fun start ->
    ignorer (token start) (chomp_until_end_or "\n")

let rec nestable_help :
 (char -> bool) ->
 ('c, 'x, unit) parser ->
 ('c, 'x, unit) parser ->
 'x ->
 int ->
 ('c, 'x, unit) parser =
  fun is_not_relevant openn close expecting_close nest_level ->
    skip (chomp_while is_not_relevant) @@
      one_of
        [ if nest_level = 1 then
            close
          else
            close
              |> and_then
                   ( fun _ ->
                       nestable_help
                         is_not_relevant
                         openn
                         close
                         expecting_close
                         (nest_level - 1)
                   )
        ; openn
            |> and_then
                 ( fun _ ->
                     nestable_help
                       is_not_relevant
                       openn
                       close
                       expecting_close
                       (nest_level + 1)
                 )
        ; chomp_if (fun _ -> true) expecting_close
            |> and_then
                 ( fun _ ->
                     nestable_help
                       is_not_relevant
                       openn
                       close
                       expecting_close
                       nest_level
                 )
        ]

let nestable_comment : 'x token -> 'x token -> ('c, 'x, unit) parser =
  fun (Token (o_str, o_x) as openn) (Token (c_str, c_x) as close) ->
    match uncons o_str with
      | None ->
          problem o_x

      | Some (open_char, _) ->
          begin match uncons c_str with
            | None ->
                problem c_x

            | Some (close_char, _) ->
                let is_not_relevant c =
                  c <> open_char && c <> close_char
                in
                let chomp_open =
                  token openn
                in
                ignorer
                  chomp_open
                  (nestable_help is_not_relevant chomp_open (token close) c_x 1)
          end

type nestable =
  | NotNestable
  | Nestable

let multi_comment : 'x token -> 'x token -> nestable -> ('c, 'x, unit) parser =
  fun openn close nestable ->
    match nestable with
      | NotNestable ->
          ignorer (token openn) (chomp_until close)

      | Nestable ->
          nestable_comment openn close

(* Syntax *)

module Syntax = struct
  let ( let+ ) p f =
    map f p

  let ( and+ ) pa pb =
    map2 (fun x y -> (x, y)) pa pb

  let ( and* ) pa pb =
    ( and+ ) pa pb

  let ( let* ) p f =
    p |> and_then f
end

let (|=) = keeper
let (|.) = ignorer

(* Int *)

let int : 'x -> ('c, 'x, int) parser =
  fun expecting ->
    let open Syntax in
    let* s =
      get_chomped_string (chomp_while is_num)
    in
    if s = "" then
      problem expecting
    else
      succeed (int_of_string s)

(* Float *)

let float : 'x -> 'x -> ('c, 'x, float) parser =
  fun expecting invalid ->
    let open Syntax in
    let* s1 =
      get_chomped_string (chomp_while is_num)
    in
    if s1 = "" then
      problem expecting
    else
      let* _ =
        symbol (Token (".", invalid))
      in
      let* s2 =
        get_chomped_string (chomp_while is_num)
      in
      if s2 = "" then
        problem invalid
      else
        succeed (float_of_string @@ s1 ^ "." ^ s2)
