open Common


type t =
  | Empty
  | Scalar of string
  | Object of (string * t) array
  | Array of t array



let fold (y:t)
          (f1:'a)
          (f2:string->'a)
          (f3:(string * 'a) array -> 'a)
          (f4:'a array -> 'a)
    : 'a =
  let rec fld = function
    | Empty ->
       f1
    | Scalar s ->
       f2 s
    | Object arr ->
       f3 @@ Array.map (fun (s,y) -> s, fld y) arr
    | Array arr ->
       f4 @@ Array.map fld arr
  in
  fld y


let to_string (y:t): string =
  fold
    y "Empty" identity
    (fun arr ->
      "{"
      ^ String.concat
          ";"
          (List.map (fun (key,v) -> key ^ ":" ^ v) (Array.to_list arr))
      ^ "}")
    (fun arr ->
      "["
      ^ String.concat
          ","
          (Array.to_list arr)
      ^ "]")



module Parser =
  struct
    type t0 = t

    module P = Character_parser.Simple (struct type t = t0 end)
    include P

    let scalar: string t =
      word
        Char.is_letter
        (fun c -> Char.is_letter c || Char.is_digit c || c = '_' || c = '-')
        "atom"

    let white_space: int t =
      detached P.whitespace



    let rec yaml (): t0 t =
      let key_value =
        get_bounds >>= fun (lb,ub) ->
        let open Printf in
        printf "key_value lb %d" lb;
        (match ub with
         | None -> printf "\n"
         | Some ub -> printf ", ub %d\n" ub);
        scalar >>= fun key ->
        char ':' >>= fun _ ->
        white_space >>= fun _ ->
        indented (yaml ()) >>= fun v ->
        white_space >>= fun _ ->
        return (key,v)
      and item =
        char '-' >>= fun _ ->
        white_space >>= fun _ ->
        yaml () >>= fun v ->
        white_space >>= fun _ ->
        return v
      in
      let scalar_or_object =
        scalar >>= fun str ->
        (char ':' >>= fun _ ->
         white_space >>= fun _ ->
         indented (yaml ()) >>= fun v ->
         white_space >>= fun _ ->
         zero_or_more (absolute key_value) >>= fun lst ->
         return (Object (Array.of_list @@ (str,v) :: lst))
        )
        <|> return (Scalar str)
      and list =
        char '-' >>= fun _ ->
        white_space >>= fun _ ->
        indented (yaml ()) >>= fun v ->
        white_space >>= fun _ ->
        zero_or_more (absolute item) >>= fun lst ->
        return @@ Array (Array.of_list @@ v :: lst)
      in
      absolute
      @@ (scalar_or_object <|> list <|> return Empty)


    let result_string (p:parser): string =
      P.result_string p to_string


    let make () : parser =
      P.make (return identity
              |= yaml ()
              |. expect_end)


    let run (str:string): parser =
      P.run (return identity
             |= yaml ()
             |. expect_end) str
  end








(* ------------------------------------------------------------------

   Unit Test

   ------------------------------------------------------------------ *)

let string_of_result res =
  match res with
  | Ok y -> to_string y
  | Error _ ->
     "ERROR"

let _ = string_of_result

(*
let%test _ =
  let open Parser in
  let p = run "" in
  Printf.printf "%s\n" (string_of_result (result p));
  has_ended p
  && column p = 0
  && result p = Ok Empty

let%test _ =
  let open Parser in
  let p = run "hello" in
  Printf.printf "%s\n" (result_string p);
  has_ended p
  && column p = 5
  && result p = Ok (Scalar "hello")


let%test _ =
  let open Parser in
  let str = "a:x\nb:y\nc:z\n  " in
  let p = run str in
  Printf.printf "string <%s>\n" (String.escaped str);
  Printf.printf "line %d, column %d\n" (line p) (column p);
  Printf.printf "%s\n"  (result_string p);
  Printf.printf "lookahead %s\n" (lookahead_string p);
  has_ended p


let%test _ =
  let open Parser in
  let str = "a:x\nb:y\n c:z" in
  let p = run str in
  Printf.printf "string <%s>\n" (String.escaped str);
  Printf.printf "line %d, column %d\n" (line p) (column p);
  Printf.printf "%s\n"  (result_string p);
  Printf.printf "lookahead %s\n" (lookahead_string p);
  has_ended p


let%test _ =
  let open Parser in
  let str = "a:x\nb:y\nc:\n -d\n -e" in
  let p = run str in
  Printf.printf "string <%s>\n" (String.escaped str);
  Printf.printf "line %d, column %d\n" (line p) (column p);
  Printf.printf "%s\n"  (result_string p);
  Printf.printf "lookahead %s\n" (lookahead_string p);
  has_ended p


let%test _ =
  let open Parser in
  let str = "a:x\nb:y\nc:\n -b1\n -b2\nd: \n   z" in
  let p = run str in
  Printf.printf "string <%s>\n" (String.escaped str);
  Printf.printf "line %d, column %d\n" (line p) (column p);
  Printf.printf "%s\n"  (result_string p);
  Printf.printf "lookahead %s\n" (lookahead_string p);
  has_ended p
  && line p = 6
  && column p = 4

 *)
