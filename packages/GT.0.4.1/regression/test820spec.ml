open GT

(*
(* Have become broken when we intrduced combinatorial type abbreviations *)
type 'a maybe = Just of 'a | Nothing [@@deriving gt ~options: { show } ]

module P = struct
  type t = (int -> string) maybe
  [@@deriving gt ~options:{ show={ _1 = (fun () _ -> "<fun>")  } }]
end

let () =
  Printf.printf "%s\n%!" @@ GT.show P.t @@  Just (fun x -> "?")
*)

module O = struct
  type t = { a:int; b:(string [@opaque]) } [@@deriving gt ~options: { show } ]
end

let () =
  Printf.printf "%s\n%!" @@ GT.show O.t { O.a = 5; O.b = "asdf" }
