open Core
open Poly

module P  = Patience_diff_lib.Std.Patience_diff.String
module MB = Patience_diff_lib.Std.Patience_diff.Matching_block

module Interval = Core.Interval.Int

let combine_triples (triples : MB.t list list) =
  let intervals =
    List.map triples ~f:(fun triples ->
      List.map triples ~f:(fun mb ->
        Interval.create mb.MB.prev_start (mb.MB.prev_start + mb.MB.length - 1)))
  in
  match intervals with
  | [] -> []
  | head::tail -> List.fold tail
                    ~init:head ~f:(fun acc l -> Interval.list_intersect acc l)

let find_idx (triples : MB.t list) idx =
  List.find_map triples ~f:(fun mb ->
    if mb.MB.prev_start <= idx && mb.MB.prev_start + mb.MB.length > idx
    then Some (mb.MB.next_start + (idx - mb.MB.prev_start))
    else None
  )

let combine triples =
  let module I = Interval in
  let intervals = combine_triples triples in
  let intervals = List.filter intervals
                    ~f:(fun interval -> not (Interval.is_empty interval)) in
  List.map intervals ~f:(fun interval ->
    let ids = List.map triples
                ~f:(fun triples -> find_idx triples (I.lbound_exn interval))
    in
    let len = (I.ubound_exn interval) - (I.lbound_exn interval) + 1 in
    (I.lbound_exn interval, ids, len)
  )

let words_rex =
  Pcre.regexp
    "\\{|\\}|\\[|\\]|[\\=\\+\\-\\/\\!\\@\\$\\%\\^\\&\\*\\:]+|\\#|,|;|\\)|\\(|\\s+"

(* Split a string into a list of strings delimited by words_rex (delimiters included) *)
let split s =
  let list = Pcre.full_split ~rex:words_rex s in
  let list = List.group list ~break:(fun res1 res2 ->
    match res1, res2 with
    | Pcre.Text _, Pcre.Delim s -> String.strip s = ""
    | _ -> true
  )
  in
  List.map list ~f:
    (fun split_results ->
       let strings = List.map split_results ~f:(function
         | Pcre.Text  s -> String.strip s
         | Pcre.Delim s -> String.strip s
         | Pcre.Group _ -> assert false  (* Irrelevant, since rex has no groups *)
         | Pcre.NoGroup -> assert false) (* Ditto                               *)
       in
       String.concat strings ~sep:""
    )

let foldi_right list ~f ~init =
  (* Inefficient but does the job. *)
  List.fold_right list ~init:(List.length list - 1, init)
    ~f:(fun a (i, b) -> (i - 1, f i a b))
  |> snd
let%expect_test "example" =
  print_s [%sexp (foldi_right [4; 5; 6] ~f:(fun i a b -> (i, a) :: b) ~init:[]
                  : (int * int) list)];
  [%expect {| ((0 4) (1 5) (2 6)) |}]

let split_respecting_quotations s =
  let alternating_literals_and_quotations =
    String.Escaping.split s ~on:'"' ~escape_char:'\\'
  in
  foldi_right alternating_literals_and_quotations ~init:[]
    ~f:(fun i s words ->
      if i % 2 = 0 then  (* "literal" *)
        split s @ words
      else  (* quotation *)
        match words with
        (* A string followed by space is a separate word. *)
        | ""   :: _     | [] -> ("\"" ^ s ^ "\"")        :: words
        (* However, if a string is followed immediately by another word, not space, bind
           it like [split] does any other word followed by a non-space delimiter. *)
        | word :: words      -> ("\"" ^ s ^ "\"" ^ word) :: words)

(* This function tries to determine if a line is a comment by replacing "(*" with ( and
   "*)" with ) and seeing if it parses as a sexp.  To allow unbalanced parentheses within
   comments, non-comment-opening parens are replaced with other characters. *)
let line_is_comment =
  let open_comment_rex           = Re2.create_exn "\\(\\*"    in
  let close_comment_rex          = Re2.create_exn "\\*\\)"    in
  let open_paren_noncomment_rex  = Re2.create_exn "\\(([^*])" in
  let close_paren_noncomment_rex = Re2.create_exn "([^*])\\)" in
  fun line ->
    if not (String.is_prefix line ~prefix:"(*" && String.is_suffix line ~suffix:"*)")
    then false
    else
      let non_comments_rewritten =
        let line =
          Re2.rewrite_exn open_paren_noncomment_rex line ~template:"l\\1"
        in
        Re2.rewrite_exn close_paren_noncomment_rex line ~template:"\\1r"
      in
      let comments_rewritten =
        let line =
          Re2.rewrite_exn open_comment_rex non_comments_rewritten ~template:"("
        in
        Re2.rewrite_exn close_comment_rex line ~template:")"
      in
      try ignore (Sexp.of_string comments_rewritten : Sexp.t); true
      with _ -> false

let line_up_words lines =
  let non_comment_lines, comment_lines =
    let lines = List.map lines ~f:String.strip |> Array.of_list in
    let num_lines = Array.length lines in
    (* inclusive *)
    let sub_line ~from ~to_ =
      String.concat ~sep:"" (
        List.init (to_ - from + 1) ~f:(fun i -> lines.(i + from))
      )
    in
    (* This loop attempts to find multi-line comments.  Given a line which looks like it
       starts a comment, it looks forward to find a line which ends the comment, and if it
       finds such a line, it marks all the lines between as comments in
       [lines_to_ignore]*)
    let lines_to_ignore = Array.create ~len:num_lines false in
    let rec loop i =
      if i < num_lines
      then begin
        if String.is_empty lines.(i) then
          begin
            lines_to_ignore.(i) <- true;
            loop (i + 1)
          end
        else if not (String.is_prefix lines.(i) ~prefix:"(*")
        then loop (i + 1)
        else
          let rec closing_line j =
            if j >= num_lines
            then None
            else
            if String.is_suffix lines.(j) ~suffix:"*)"
            && line_is_comment (sub_line ~from:i ~to_:j)
            then Some j
            else closing_line (j + 1)
          in
          let closing_line = closing_line i in
          match closing_line with
          | None -> loop (i + 1)
          | Some closing_line ->
            for k = i to closing_line do
              lines_to_ignore.(k) <- true
            done;
            loop (closing_line + 1)
      end
    in
    loop 0;
    let non_comment_lines =
      Array.map2_exn lines_to_ignore lines ~f:(fun line_is_comment line ->
        if line_is_comment
        then None
        else Some line
      ) |> Array.filter_opt |> Array.to_list
    in
    let comment_lines =
      Array.map2_exn lines_to_ignore lines ~f:(fun line_is_comment line ->
        if line_is_comment
        then Some line
        else None
      ) |> Array.filter_mapi ~f:(fun i opt ->
        Option.map opt ~f:(fun x -> (i,x))
      ) |> Array.to_list
    in
    non_comment_lines, comment_lines
  in
  let word_split_lines = List.map non_comment_lines ~f:split_respecting_quotations in
  let num_lines = List.length word_split_lines in
  match word_split_lines with
  | [] -> []
  | mine :: others ->
    let triples =
      List.map others ~f:(fun other ->
        P.get_matching_blocks
          ~big_enough:1
          ~transform:Fn.id
          ~prev:(Array.of_list mine)
          ~next:(Array.of_list other)
      )
    in
    let res = combine triples in
    let normalized_ids =
      List.map res ~f:(fun (id1, ids, len) ->
        let ids = id1 :: List.map ids ~f:(fun id -> Option.value_exn id) in
        ids, len)
    in
    let nonmatch_ids =
      let prefixed_ids =
        let first = ((List.init num_lines ~f:(fun _ -> 0)), 0) in
        first :: normalized_ids
      in
      let normalized_ids =
        let last = ((List.init num_lines ~f:(fun i ->
          List.length (List.nth_exn word_split_lines i))), 0) in
        normalized_ids @ [ last ]
      in
      List.map2_exn prefixed_ids normalized_ids ~f:(fun (ids, len) (next_ids, _) ->
        List.map ids ~f:(fun id -> (id, id + len)),
        List.map2_exn ids next_ids ~f:(fun id next_id -> (id + len, next_id))
      )
      |> List.concat_map ~f:(fun (a, b) -> [a; b])
    in
    let result_lines = Array.create ~len:num_lines "" in
    List.iter nonmatch_ids ~f:(fun start_stop_pairs ->
      let strings =
        List.mapi start_stop_pairs ~f:(fun i (start, stop) ->
          let words = List.nth_exn word_split_lines i in
          let sub_words = List.sub words ~pos:start ~len:(stop - start) in
          sub_words
          |> List.map ~f:(function "" -> " " | x -> x)
          |> String.concat ~sep:""
        )
      in
      let lengths = List.map strings ~f:String.length in
      let max_len = List.reduce_exn lengths ~f:max in
      let is_numeric c = Char.is_digit c || c = '_' || c = '.' in
      let all_numeric = List.for_all strings ~f:(String.for_all ~f:is_numeric) in
      let justification = if all_numeric then `right else `left in
      let padded_strings =
        List.map strings ~f:(fun str ->
          let len = String.length str in
          let padding = String.make (max_len - len) ' ' in
          match justification with
          | `left -> str ^ padding
          | `right -> padding ^ str
        )
      in
      List.iteri padded_strings ~f:(fun i str ->
        result_lines.(i) <- result_lines.(i) ^ str;
      );
    );
    let result_lines =
      let result_lines =
        List.mapi (Array.to_list result_lines) ~f:(fun i line -> (i,line))
      in
      let rec loop acc result_lines comment_lines =
        match result_lines with
        | [] -> List.rev (List.rev_append (List.map ~f:snd comment_lines) acc)
        | (result_i,result_line)::result_rest ->
          match comment_lines with
          | [] -> List.rev (List.rev_append (List.map ~f:snd result_lines) acc)
          | (comment_i,comment_line)::comment_rest ->
            if comment_i <= result_i
            then
              loop (comment_line::acc)
                (List.map result_lines ~f:(fun (i,line) -> (i+1,line)))
                comment_rest
            else loop (result_line::acc) result_rest comment_lines
      in
      loop [] result_lines comment_lines
    in
    result_lines

let%test_module _ =
  (module struct
    let test_line_up_words ~input ~expect =
      [%test_result: string list] (line_up_words input) ~expect

    let%test_unit _ = test_line_up_words
                        ~input:
                          ["  | Foo of int"
                          ;"| Foooooo of string"]
                        ~expect:
                          ["| Foo     of int   "
                          ;"| Foooooo of string"]
    ;;

    let%test_unit _ = test_line_up_words
                        ~input:
                          ["|                 Thing            of        bool"
                          ;"  | Foo of int"
                          ;""
                          ;"(* comments and blank lines should be ignored *)"
                          ;"| Foooooo of string"]
                        ~expect:
                          ["| Thing   of bool  "
                          ;"| Foo     of int   "
                          ;""
                          ;"(* comments and blank lines should be ignored *)"
                          ;"| Foooooo of string"]
    ;;

    let%test_unit _ = test_line_up_words
                        ~input:
                          ["int : Int.t;"
                          ;"string : String.t;"]
                        ~expect:
                          ["int    : Int.t;   "
                          ;"string : String.t;"]
    ;;

    let%test_unit _ =
      test_line_up_words
        ~input:[
          "foo.Bar.float <- 3.1415;";
          "barbaz.Qux.floatfloat <- 1.41;";
          "bangbangbang.Bang.foobar <- 2.7;";
        ]
        ~expect:[
          "foo.Bar.float            <- 3.1415;";
          "barbaz.Qux.floatfloat    <- 1.41;  ";
          "bangbangbang.Bang.foobar <- 2.7;   ";
        ]
    ;;

    let lines s = List.iter ~f:print_endline (line_up_words (String.split_lines s))

    let%expect_test "don't insert new space after strings before commas" =
      lines {| [ "analyze", analyze_command
               ; "show",    show_command ] |};
      [%expect {|
        [ "analyze", analyze_command
        ; "show",    show_command ] |}]

    let%expect_test "strings get treated like symbols when binding to commas" =
      print_s [%sexp (split_respecting_quotations {| [ "analyze", analyze_command |}
                      : string list)];
      [%expect {| ("" [ "" "\"analyze\"," "" analyze_command "") |}];
      print_s [%sexp (split_respecting_quotations {| [ analyze, analyze_command |}
                      : string list)];
      [%expect {| ("" [ "" analyze, "" analyze_command "") |}]

    let%expect_test "don't insert new space after strings before semicolons" =
      lines {| f a b "c";
               f   b "c" "d"; |};
      [%expect {|
        f a b "c";
        f   b "c" "d"; |}]
  end)
