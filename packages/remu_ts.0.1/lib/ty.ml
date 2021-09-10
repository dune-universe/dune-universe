(* open Remu_ts.Comxm *)
open Comm
module type Algebra =
sig
  type repr
  val app : repr * repr -> repr
  val arrow : repr * repr -> repr
  val var : int -> repr
  val nom : int -> repr
  val fresh: string -> repr
  val tuple : repr list -> repr
  val record : (string, repr) map -> repr
  val forall: string set * repr -> repr
end

let mk_record : type a.
  (module Algebra with type repr = a) ->
  (string * a) list -> a = fun (module M) xs ->
  let open M in
  record @@ Map.of_enum @@ List.enum xs

type printed = {str : string; is_complex : bool}
module PrintAlgebra : Algebra with type repr = printed
= struct
  type repr = printed
  let unbox {str; _} = str
  let paren a = "(" ^ a ^ ")"

  let app ({str=f; _}, {str=arg; is_complex}) =
    let g =
      if is_complex then paren
      else fun x -> x
    in {str=f ^ " " ^ g arg; is_complex=true}

  let arrow ({str=arg; is_complex=i1}, {str=ret; is_complex=i2}) =
    let g =
      if i1 then paren
      else fun x -> x
    in
    let is_complex = i1 || i2
    in {str=g arg ^ " -> " ^ ret; is_complex}

  let var i =  {str="typevar{" ^ string_of_int i ^ "}"; is_complex=false}
  let nom i =  {str="type{" ^ string_of_int i ^ "}"; is_complex=false}
  let fresh s =  {str="'" ^ s ^ "}"; is_complex=false}
  let forall (ns, {str; is_complex}) =
    let str =
      Set.fold (fun st cur -> st ^ " " ^ cur) ns "" ^ ". " ^ str
    in {str; is_complex}
  let tuple xs =
    let str =
      match xs with
      | [x] -> "(" ^ unbox x ^ ")"
      | _   -> "(" ^ String.concat ", " (List.map unbox xs) ^ ")"
    in {str; is_complex=false}
  let record fields =
    let fields = Map.foldi (fun key a s -> (key, a)::s) fields [] in
    let fields = List.map (fun (key, {str; _}) -> key ^ ": " ^ str) fields in
    let str = "{" ^ String.concat ", " fields ^ "}" in
    {str; is_complex=false}
end
