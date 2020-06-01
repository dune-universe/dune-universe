open Core

module Underlying = struct
  type t =
    | String of String.t
    | Bigstring of Bigstring.t
    | Char of char

  let length = function
    | String str -> String.length str
    | Bigstring str -> Bigstring.length str
    | Char _ -> 1
  ;;

  let blit_bytes ~src =
    match src with
    | String src ->
      fun ?(src_pos = 0) ?src_len:(len = String.length src) ~dst ?(dst_pos = 0) () ->
        Bytes.From_string.blit ~src ~src_pos ~len ~dst ~dst_pos
    | Bigstring src -> Bigstring.To_bytes.blito ~src
    | Char c ->
      fun ?(src_pos = 0) ?(src_len = 1) ~dst ?(dst_pos = 0) () ->
        (match src_pos, src_len with
         | 0, 1 -> Bytes.set dst dst_pos c
         | (0 | 1), 0 -> ()
         | _, _ -> invalid_arg "index out of bounds")
  ;;

  let blit_bigstring ~src =
    match src with
    | String src -> Bigstring.From_string.blito ~src
    | Bigstring src -> Bigstring.blito ~src
    | Char c ->
      fun ?(src_pos = 0) ?(src_len = 1) ~dst ?(dst_pos = 0) () ->
        (match src_pos, src_len with
         | 0, 1 -> dst.{dst_pos} <- c
         | (0 | 1), 0 -> ()
         | _, _ -> invalid_arg "index out of bounds")
  ;;

  let output_channel ~channel = function
    | String str -> Out_channel.output_string channel str
    | Bigstring bstr -> Bigstring_unix.really_output channel bstr
    | Char c -> Out_channel.output_char channel c
  ;;

  let output_unix ~writer = function
    | String str -> Async.Writer.write writer str
    | Bigstring bstr -> Async.Writer.write_bigstring writer bstr
    | Char c -> Async.Writer.write_char writer c
  ;;

  let output_bigbuffer ~bigbuffer = function
    | String s -> Bigbuffer.add_string bigbuffer s
    | Bigstring bstr -> Bigbuffer.add_bigstring bigbuffer bstr
    | Char c -> Bigbuffer.add_char bigbuffer c
  ;;

  let is_substr_string ?(pos = 0) ?len t ~string =
    let len = Option.value len ~default:(length t - pos) in
    if len <> Substring.length string || pos < 0 || pos + len > length t
    then false
    else (
      let get =
        match t with
        | String str -> String.get str
        | Bigstring bstr -> Bigstring.get bstr
        | Char c -> const c
      in
      let rec loop i =
        if i >= len
        then true
        else Char.equal (get (i + pos)) (Substring.get string i) && loop (i + 1)
      in
      loop 0)
  ;;

  let is_substr_prefix ?(pos = 0) ?len t ~prefix =
    let len = Option.value len ~default:(length t - pos) in
    if len < Substring.length prefix
    then false
    else is_substr_string ~pos ~len:(Substring.length prefix) t ~string:prefix
  ;;

  let is_substr_suffix ?(pos = 0) ?len t ~suffix =
    let len = Option.value len ~default:(length t - pos) in
    if len < Substring.length suffix
    then false
    else
      is_substr_string
        ~pos:(pos + len - Substring.length suffix)
        ~len:(Substring.length suffix)
        t
        ~string:suffix
  ;;

  let is_substr_substring ?(pos = 0) ?len t ~substring =
    let len = Option.value len ~default:(length t - pos) in
    if len < Substring.length substring
    then false
    else (
      let rec loop i =
        if i + Substring.length substring > len
        then false
        else
          is_substr_string
            ~pos:(pos + i)
            ~len:(Substring.length substring)
            t
            ~string:substring
          || loop (i + 1)
      in
      loop 0)
  ;;
end

type t =
  | List of (int * t list)
  | Leaf of Underlying.t

let empty = List (0, [])

let of_string s = if String.is_empty s then empty else Leaf (Underlying.String s)

let of_bigstring bs =
  if 0 = Bigstring.length bs then empty else Leaf (Underlying.Bigstring bs)
;;

let of_char c = Leaf (Underlying.Char c)
let nl = of_char '\n'

let length = function
  | List (len, _) -> len
  | Leaf underlying -> Underlying.length underlying
;;

let is_empty t = length t = 0

(**
   The plus operation is not associative over individual representations,
   but is associative over the quotient space with the equivalence
   relationship
   x ~ y == (to_string x) = (to_string y)
*)
let plus a b =
  match a, b with
  | b, List (0, _) -> b
  | List (0, _), b -> b
  | List (len, _), b -> List (len + length b, [ a; b ])
  | Leaf a', List (len, l) -> List (Underlying.length a' + len, a :: l)
  | Leaf x, Leaf y -> List (Underlying.(length x + length y), [ a; b ])
;;

let concat ?(sep = empty) ts =
  (* Fold right is more efficient than fold_left, as it will create a
     flat List node *)
  match ts with
  | [] -> empty
  | t :: ts ->
    plus t (List.fold_right ts ~f:(fun t ts -> plus sep (plus t ts)) ~init:empty)
;;

let concat_underlying ~of_underlying ?sep strs =
  let sep = Option.map sep ~f:of_underlying in
  let ts = List.map strs ~f:of_underlying in
  concat ?sep ts
;;

let concat_string = concat_underlying ~of_underlying:of_string

(*
   let __UNUSED_VALUE__concat_bigstring =
   concat_underlying ~of_underlying:of_bigstring;;
*)

type blitter =
  src:Underlying.t -> ?src_pos:int -> ?src_len:int -> ?dst_pos:int -> unit -> unit

let blit ~(dst_blit : blitter) t =
  let rec blit dst_pos t =
    match t with
    | Leaf src -> dst_blit ~src ~dst_pos ()
    | List (len, srcs) ->
      let len' =
        List.fold_left srcs ~init:dst_pos ~f:(fun dst_pos t ->
          blit dst_pos t;
          dst_pos + length t)
      in
      assert (len' - dst_pos = len)
  in
  blit 0 t
;;

let to_string t =
  let dst = Bytes.create (length t) in
  blit ~dst_blit:(Underlying.blit_bytes ~dst) t;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
;;

let to_bigstring t =
  let dst = Bigstring.create (length t) in
  blit ~dst_blit:(Underlying.blit_bigstring ~dst) t;
  dst
;;

let output ~dst_output t =
  let rec output t =
    match t with
    | Leaf underlying -> dst_output underlying
    | List (_, ts) -> List.iter ~f:output ts
  in
  output t
;;

let output_channel t channel = output ~dst_output:(Underlying.output_channel ~channel) t
let output_unix t writer = output ~dst_output:(Underlying.output_unix ~writer) t

let output_bigbuffer t bigbuffer =
  output ~dst_output:(Underlying.output_bigbuffer ~bigbuffer) t
;;

let rec fold t ~init ~f =
  match t with
  | List (_, list) -> List.fold list ~init ~f:(fun init t -> fold t ~init ~f)
  | Leaf underlying -> f init underlying
;;

let iter t ~f = fold t ~init:() ~f:(fun () -> f)



let rec is_substr_string t ~string =
  if is_empty t && Substring.is_empty string
  then true
  else if length t <> Substring.length string
  then false
  else (
    match t with
    | Leaf u -> Underlying.is_substr_string u ~string
    | List (len, ts) ->
      assert (len = Substring.length string);
      List.fold_until
        ts
        ~init:0
        ~f:(fun pos t ->
          if is_substr_string t ~string:(Substring.sub string ~pos ~len:(length t))
          then Continue_or_stop.Continue (pos + length t)
          else Continue_or_stop.Stop false)
        ~finish:(Fn.const true))
;;

let substring_split substring ~pos =
  Substring.sub substring ~len:pos, Substring.sub substring ~pos
;;

let rec is_substr_suffix t ~suffix =
  if Substring.length suffix = 0
  then true
  else if length t < Substring.length suffix
  then false
  else (
    match t with
    | Leaf u -> Underlying.is_substr_suffix u ~suffix
    | List (_, []) -> Substring.is_empty suffix
    | List (_, [ t ]) -> is_substr_suffix t ~suffix
    | List (len, hd :: tl) ->
      let tl = List (len - length hd, tl) in
      if length tl >= Substring.length suffix
      then is_substr_suffix tl ~suffix
      else (
        let hd_part, tl_part =
          substring_split suffix ~pos:(Substring.length suffix - length tl)
        in
        is_substr_suffix hd ~suffix:hd_part && is_substr_string tl ~string:tl_part))
;;

let rec is_substr_prefix t ~prefix =
  if Substring.is_empty prefix
  then true
  else if length t < Substring.length prefix
  then false
  else (
    match t with
    | Leaf u -> Underlying.is_substr_prefix u ~prefix
    | List (_, []) -> Substring.is_empty prefix
    | List (len, hd :: tl) ->
      if length hd >= Substring.length prefix
      then is_substr_prefix hd ~prefix
      else (
        let tl = List (len - length hd, tl) in
        let hd_part, tl_part = substring_split prefix ~pos:(length hd) in
        is_substr_string hd ~string:hd_part && is_substr_prefix tl ~prefix:tl_part))
;;

let rec is_substr_substring t ~substring =
  if Substring.is_empty substring
  then true
  else if length t < Substring.length substring
  then false
  else (
    match t with
    | Leaf u -> Underlying.is_substr_substring u ~substring
    | List (_, []) -> Substring.is_empty substring
    | List (_, [ t ]) -> is_substr_substring t ~substring
    | List (len, hd :: tl) ->
      let tl = List (len - length hd, tl) in
      let rec suffix_loop pos =
        if pos <= 0
        then is_substr_substring tl ~substring
        else (
          let hd_part, tl_part = substring_split substring ~pos in
          (is_substr_suffix hd ~suffix:hd_part && is_substr_prefix tl ~prefix:tl_part)
          || suffix_loop (pos - 1))
      in
      is_substr_substring hd ~substring || suffix_loop (Substring.length substring - 1))
;;

let is_string t ~string =
  is_substr_string
    t
    ~string:(Substring.create (Bytes.unsafe_of_string_promise_no_mutation string))
;;

let is_prefix t ~prefix =
  is_substr_prefix
    t
    ~prefix:(Substring.create (Bytes.unsafe_of_string_promise_no_mutation prefix))
;;

let is_suffix t ~suffix =
  is_substr_suffix
    t
    ~suffix:(Substring.create (Bytes.unsafe_of_string_promise_no_mutation suffix))
;;

let is_substring t ~substring =
  is_substr_substring
    t
    ~substring:(Substring.create (Bytes.unsafe_of_string_promise_no_mutation substring))
;;

let%test_module _ =
  (module struct
    let haystack =
      concat
        ~sep:(of_char ' ')
        [ of_string "hello"; of_bigstring (Bigstring.of_string "big"); of_string "world" ]
    ;;

    let%expect_test "is_string" =
      printf "%b" (is_string haystack ~string:"hello big world");
      [%expect {| true |}];
      printf "%b" (is_string haystack ~string:"hello");
      [%expect {| false |}];
      printf "%b" (is_string haystack ~string:"o big");
      [%expect {| false |}]
    ;;

    let%expect_test "is_prefix" =
      printf "%b" (is_prefix haystack ~prefix:"");
      [%expect {| true |}];
      printf "%b" (is_prefix haystack ~prefix:"h");
      [%expect {| true |}];
      printf "%b" (is_prefix haystack ~prefix:"hello");
      [%expect {| true |}];
      printf "%b" (is_prefix haystack ~prefix:"hello ");
      [%expect {| true |}];
      printf "%b" (is_prefix haystack ~prefix:"hello b");
      [%expect {| true |}];
      printf "%b" (is_prefix haystack ~prefix:"hello big");
      [%expect {| true |}];
      printf "%b" (is_prefix haystack ~prefix:"hello big world");
      [%expect {| true |}];
      printf "%b" (is_prefix haystack ~prefix:"hello big round");
      [%expect {| false |}];
      printf "%b" (is_prefix haystack ~prefix:"hello big world!");
      [%expect {| false |}];
      printf "%b" (is_prefix haystack ~prefix:"b");
      [%expect {| false |}];
      printf "%b" (is_prefix haystack ~prefix:"world");
      [%expect {| false |}];
      printf "%b" (is_prefix haystack ~prefix:"d");
      [%expect {| false |}]
    ;;

    let%expect_test "is_suffix" =
      printf "%b" (is_suffix haystack ~suffix:"");
      [%expect {| true |}];
      printf "%b" (is_suffix haystack ~suffix:"d");
      [%expect {| true |}];
      printf "%b" (is_suffix haystack ~suffix:"world");
      [%expect {| true |}];
      printf "%b" (is_suffix haystack ~suffix:" world");
      [%expect {| true |}];
      printf "%b" (is_suffix haystack ~suffix:"g world");
      [%expect {| true |}];
      printf "%b" (is_suffix haystack ~suffix:"big world");
      [%expect {| true |}];
      printf "%b" (is_suffix haystack ~suffix:"hello big world");
      [%expect {| true |}];
      printf "%b" (is_suffix haystack ~suffix:"round world");
      [%expect {| false |}];
      printf "%b" (is_suffix haystack ~suffix:"hello big world!");
      [%expect {| false |}];
      printf "%b" (is_suffix haystack ~suffix:"hello");
      [%expect {| false |}]
    ;;

    let%expect_test "is_substring" =
      printf "%b" (is_substring haystack ~substring:"");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"w");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"d");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"big");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"ell");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"ell");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"o b");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"o big w");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"hello big world");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"hello big world!");
      [%expect {| false |}];
      printf "%b" (is_substring haystack ~substring:"big world");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"hello big");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"hello big ");
      [%expect {| true |}];
      printf "%b" (is_substring haystack ~substring:"hello big round");
      [%expect {| false |}];
      printf "%b" (is_substring haystack ~substring:"round");
      [%expect {| false |}];
      printf "%b" (is_substring haystack ~substring:"big round");
      [%expect {| false |}]
    ;;
  end)
;;
