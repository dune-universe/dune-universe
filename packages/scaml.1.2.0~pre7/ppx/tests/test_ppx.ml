open Typerep_lib.Std (* open Tyeprep_lib.Std must come before open SCaml *)
open SCaml

(* Type definitins with [@@deriving typerep] must be declared outside 
   of [@@@SCaml], since it generates code which SCaml cannot handle. *)
type t = Foo of int | Bar [@@deriving typerep]
         
module Smart_contract = struct
  [@@@SCaml] (* Here is the smart contract code *)
  let x = Int 1
  let [@entry] default () _ = 
    [], 
    (assert (One.i = x && One.f () = x); Foo (Int 42))
end

(* Emit the compiled Michelson.  This call must be executed after all 
   the smart contract code are declared. *)
let () = SCamlc.Ppx.emit ~outputprefix:"out"

(* Example of converting OCaml/SCaml values to Michelson constants *)
let () = 
  let c = SCamlc.Typerep.to_michelson (typerep_of_list typerep_of_t) [ Foo (Int 42); Bar ] in
  Format.eprintf "Foo=%a@." SCamlc.Michelson.Constant.pp c

(* Example of reverting Michelson constants to OCaml/SCaml values *)
let () =
  match SCamlc.Michelson.Micheline.parse_expression_string "{ Right 42 ; Left 0 }" with
  | Error _ -> assert false
  | Ok node ->
      match SCamlc.Typerep.of_micheline (typerep_of_list typerep_of_t) node with
      | None -> assert false
      | Some e -> 
          assert (e = [ Foo (Int 42); Bar ]);
          prerr_endline "OK!"

(* Example of converting OCaml/SCaml type to Michelson *)
let () =
  let mty =
    SCamlc.Typerep.to_michelson_type (typerep_of_list typerep_of_t)
  in
  (* list (or (int %Bar) (int %Foo)) *)
  Format.eprintf "%a@." SCamlc.Michelson.Type.pp mty;
  assert (mty = SCamlc.Michelson.Type.(tyList (tyOr (attribute ["%Bar"] tyInt , attribute ["%Foo"] tyInt))))


