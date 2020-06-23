open Typerep_lib.Std (* must be before open SCaml *)
open SCaml
open SCamlc
open SCamlc.Typerep

let f tr v =
  let m = to_michelson tr v in
  let ml = Michelson.Constant.to_micheline m in
  Format.eprintf "to_micheline: %a@." Michelson.Mline.pp ml;
  begin match of_michelson tr m with
  | Some v' -> assert (v = v')
  | None -> assert false
  end;
  prerr_endline "of_michelson: ok";
  match michelson_of_micheline tr @@ Michelson.Micheline.to_parsed ml with
  | None -> prerr_endline "of_micheline failed"; assert false
  | Some c -> 
      Format.eprintf "of_michelson: %a@." Michelson.Constant.pp c;
      begin match of_michelson tr c with
        | Some v' -> assert (v = v')
        | None -> assert false
      end

type t = nat list * tz option [@@deriving typerep]
let () = f typerep_of_t ([Nat 1; Nat 2; Nat 3], Some (Tz 1.0))

type t2 = (string, nat) sum list [@@deriving typerep]
let () = f typerep_of_t2 [Left "hello"; Right (Nat 2)]

type t3 = { x : int; y : nat; z : tz }  [@@deriving typerep]
let () = f typerep_of_t3 { x= Int 1; y = Nat 2; z= Tz 1.0 }

type t4' = Bar of int | Bee of nat | Boo of string [@@deriving typerep]
type t5' = t4' list [@@deriving typerep]
let () = f typerep_of_t5' [Bar (Int 1); Bee (Nat 1); Boo "hello"]

type t4 = Foo | Bar of int | Bee of nat | Boo of string | Xee [@@deriving typerep]
type t5 = t4 list [@@deriving typerep]
let () = f typerep_of_t5 [Foo; Xee]
let () = f typerep_of_t5 [Bar (Int 1)]
let () = f typerep_of_t5 [Foo; Bar (Int 1); Bee (Nat 1); Boo "hello"; Xee]

let () = f typerep_of_bytes (Bytes "00112233")
let () = f Bytes.typerep_of_t (Bytes "00112233")

type t6 = int set [@@deriving typerep]
let () = f typerep_of_t6 (Set [Int 1; Int 2; Int 3])

type t7 = int Set.t [@@deriving typerep]
let () = f typerep_of_t7 (Set [Int 1; Int 2; Int 3])

type t8 = (int, nat) map [@@deriving typerep]
let () = f typerep_of_t8 (Map [(Int 1, Nat 1); (Int 2, Nat 2); (Int 3, Nat 3)])

type t9 = (int, nat) Map.t [@@deriving typerep]
let () = f typerep_of_t8 (Map [(Int 1, Nat 1); (Int 2, Nat 2); (Int 3, Nat 3)])

let () = f typerep_of_address (Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN")
let () = f typerep_of_key_hash (Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
let () = f typerep_of_timestamp (Timestamp "2020-04-01T00:00:00Z")
let () = f typerep_of_key (Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav")
let () = f typerep_of_signature (Signature "edsigu3QszDjUpeqYqbvhyRxMpVFamEnvm9FYnt7YiiNt9nmjYfh8ZTbsybZ5WnBkhA7zfHsRVyuTnRsGLR6fNHt1Up1FxgyRtF")
