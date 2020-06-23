(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2019  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The actual implementation of these API functions are given in
   primitives.ml as Michelson code.  This file is to give their ML types.
*)

open Typerep_lib.Std_internal

type 'a const = 'a [@@deriving typerep]

type ocaml_int = int [@@deriving typerep]
type ocaml_string = string 

type nat = Nat of ocaml_int const [@@deriving typerep]
type int = Int of ocaml_int const [@@deriving typerep]
type tz = Tz of float const [@@deriving typerep]

module Option = struct
  type 'a t = 'a option = None | Some of 'a [@@deriving typerep]

  let value o a = match o with
    | Some a -> a
    | None -> a

  let get = function
    | Some a -> a
    | None -> invalid_arg "Option.get"
end

type ('a, 'b) sum = Left of 'a | Right of 'b [@@deriving typerep]

module Sum = struct
  type ('a, 'b) t = ('a, 'b) sum = Left of 'a | Right of 'b [@@deriving typerep]

  let get_left = function
    | Left a -> a
    | _ -> invalid_arg "Sum.get_left"

  let get_right = function
    | Right a -> a
    | _ -> invalid_arg "Sum.get_right"

end

(* So far we use int instead of Z *)

exception Overflow

let check_overflow =
  let min_int = Z.of_int min_int in
  let max_int = Z.of_int max_int in
  fun z ->
    if z < min_int && max_int < z then raise Overflow
    else Z.to_int z

let check_non_negative z = if z < Z.zero then raise Overflow else z

let (!!) = check_overflow

let z_of_int (Int a) = Z.of_int a
let int_of_z z = Int (check_overflow z)

let z_of_nat (Nat a) = Z.of_int a    
let nat_of_z z = Nat (check_overflow (check_non_negative z))

let bin_int op a b = int_of_z (op (z_of_int a) (z_of_int b))
let bin_nat op a b = nat_of_z (op (z_of_nat a) (z_of_nat b))

let mutez_of_tz (Tz a) = Z.of_float (a *. 1000000.)
let tz_of_mutez =
  let max_mutez = Z.of_int64 Int64.max_int in
  fun z ->
    if z < Z.zero || max_mutez < z then raise Overflow
    else
      let f = Z.to_float z /. 1000000. in
      let z' = Z.of_float (f *. 1000000.) in
      assert (z = z');
      Tz f

let bin_tz op a b = tz_of_mutez (op (mutez_of_tz a) (mutez_of_tz b))

let (+) = bin_int Z.(+)
let (+^) = bin_nat Z.(+)
let (+$) = bin_tz Z.(+)

let (-) = bin_int Z.(-)
let (-^) a b = int_of_z (Z.(z_of_nat a - z_of_nat b))
let (-$) a b = tz_of_mutez (Z.(mutez_of_tz a - mutez_of_tz b))

let ( * ) = bin_int Z.( * )
let ( *^ ) = bin_nat Z.( * )
let ( *$ ) a b = tz_of_mutez Z.(mutez_of_tz a * z_of_nat b)

let (~-) a = int_of_z (Z.(~- (z_of_int a)))
let (~-^) a = int_of_z (Z.(~- (z_of_nat a)))

exception Division_by_zero

let ediv_int_int a b =
  if b = Int 0 then None
  else 
    let z1, z2 = Z.ediv_rem (z_of_int a) (z_of_int b) in
    Some (int_of_z z1, nat_of_z z2)

let ediv_int_nat a b =
  if b = Nat 0 then None
  else 
    let z1, z2 = Z.ediv_rem (z_of_int a) (z_of_nat b) in
    Some (int_of_z z1, nat_of_z z2)

let ediv_nat_int a b =
  if b = Int 0 then None
  else 
    let z1, z2 = Z.ediv_rem (z_of_nat a) (z_of_int b) in
    Some (int_of_z z1, nat_of_z z2)

let ediv_nat_nat a b =
  if b = Nat 0 then None
  else 
    let z1, z2 = Z.ediv_rem (z_of_nat a) (z_of_nat b) in
    Some (nat_of_z z1, nat_of_z z2)

let ediv_tz_tz a b =
  if b = Tz 0. then None
  else 
    let z1, z2 = Z.ediv_rem (mutez_of_tz a) (mutez_of_tz b) in
    Some (nat_of_z z1, tz_of_mutez z2)

let ediv_tz_nat a b =
  if b = Nat 0 then None
  else 
    let z1, z2 = Z.ediv_rem (mutez_of_tz a) (z_of_nat b) in
    Some (tz_of_mutez z1, tz_of_mutez z2)

let (/) a b = match ediv_int_int a b with
  | None -> raise Division_by_zero
  | Some (a, _) -> a
    
let (/^) a b = match ediv_nat_nat a b with
  | None -> raise Division_by_zero
  | Some (a, _) -> a
    
let (/$) a b = match ediv_tz_tz a b with
  | None -> raise Division_by_zero
  | Some (a, _) -> a
    
let (/$^) a b = match ediv_tz_nat a b with
  | None -> raise Division_by_zero
  | Some (a, _) -> a
    
let (lsl) n1 (Nat i2) = nat_of_z Z.(z_of_nat n1 lsl i2)
let (lsr) n1 (Nat i2) = nat_of_z Z.(z_of_nat n1 asr i2) (* XXX correct? *)
let (lor) n1 n2 = nat_of_z Z.(z_of_nat n1 lor z_of_nat n2)
let (land) n1 n2 = nat_of_z Z.(z_of_nat n1 land z_of_nat n2)
let land_int_nat i1 n2 = nat_of_z Z.(z_of_int i1 land z_of_nat n2)
let (lxor) n1 n2 = nat_of_z Z.(z_of_nat n1 lxor z_of_nat n2)
let lnot_nat n1 = int_of_z Z.(~! (z_of_nat n1))
let lnot i1 = int_of_z Z.(~! (z_of_int i1))

let abs a = nat_of_z (Z.abs (z_of_int a))
let isnat (Int i as a) = if i < 0 then None else Some (abs a)

let fst = fst
let snd = snd  
let compare x y = Int (compare x y)
let (=) = (=)
let (<>) = (<>)
let (<) = (<)
let (<=) = (<=)
let (>) = (>)
let (>=) = (>=)
let (&&) = (&&)
let (||) = (||)
let xor b1 b2 = not b1 = b2
let not = not

exception Fail (* XXX We cannot carry the argument of failwith... *)
  
module Error = struct
  let failwith : 'a -> 'b = fun _ -> raise Fail
end

let failwith : 'a -> 'b = fun _ -> raise Fail

let raise = raise

module List = struct
  type 'a t = 'a list [@@deriving typerep]
  let length xs = Nat (List.length xs)
  let map = List.map
  let fold_left = List.fold_left
  let fold_left' f acc xs = List.fold_left (fun x y -> f (x,y)) acc xs
  let rev = List.rev
  let rev_append = List.rev_append
end

type 'a set = Set of 'a const list [@@deriving typerep]

module Set = struct
  (* Simulation by list.  Of course very inefficient *)
  type 'a t = 'a set [@@deriving typerep]
  let empty : 'a t = Set []
  let length (Set xs) = Nat (Stdlib.List.length xs)
  let mem x (Set xs) = Stdlib.List.mem x xs
  let update x b (Set xs) =
    Set (if b then
           let rec add st = function
             | [] -> List.rev (x::st)
             | (y::_ as ys) when x < y ->
                 List.(rev (rev_append ys (x::st)))
             | y::ys when x = y -> xs
             | y::ys -> add (y::st) ys
           in
           add [] xs
         else
           let rec remove st = function
             | [] -> xs
             | y::_ when x < y -> xs
             | y::ys when x = y -> List.(rev (rev_append ys st))
             | y::ys -> remove (y::st) ys
           in
           remove [] xs)
  let fold f (Set xs) i = List.fold_left (fun st x -> f x st) i xs
  let fold' f (Set xs) i = List.fold_left (fun x y -> f (y,x)) i xs
end

type ('k, 'v) map = Map of ('k const * 'v const) list [@@deriving typerep]

module Map = struct
  (* Simulation by list.  Of course very inefficient *)
  type ('k, 'v) t = ('k, 'v) map [@@deriving typerep]
  let empty = Map []
  let length (Map xs) = Nat (Stdlib.List.length xs)
  let map f (Map xs) = Map (List.map (fun (k,v) -> (k, f k v)) xs)
  let map' f (Map xs) = Map (List.map (fun (k,v) -> (k, f (k, v))) xs)
  let get k (Map xs) = Stdlib.List.assoc_opt k xs
  let mem k (Map xs) = Stdlib.List.mem_assoc k xs
  let update k vo (Map kvs) = 
    Map (match vo with
        | None -> 
            let rec remove st = function
              | [] -> kvs
              | (k',_)::_ when k < k' -> kvs
              | (k',_)::kvs when k = k' -> List.(rev (rev_append kvs st))
              | kv::kvs -> remove (kv::st) kvs
            in
            remove [] kvs
        | Some v ->
            let rec add st = function
              | [] -> List.rev ((k,v)::st)
              | ((k',_)::_ as kvs) when k < k' -> 
                  List.rev_append kvs ((k,v)::st)
              | (k',_)::kvs when k = k' -> List.rev_append kvs ((k,v)::st)
              | kv::kvs -> add (kv::st) kvs
            in
            add [] kvs)
  let fold f (Map m) acc = List.fold_left (fun acc (k,v) -> f k v acc) acc m
  let fold' f (Map m) acc = List.fold_left (fun acc (k,v) -> f (k,v,acc)) acc m
end

type ('k, 'v) big_map = BigMap of ('k const * 'v const) list [@@deriving typerep]

module BigMap : sig
  type ('k, 'v) t = ('k, 'v) big_map = BigMap of ('k * 'v) list [@@deriving typerep]
  val empty : ('k, 'v) t
  val get : 'k -> ('k, 'v) t -> 'v option
  val mem : 'k -> ('k, 'v) t -> bool
  val update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t
end = struct
  type ('k, 'v) t = ('k, 'v) big_map = BigMap of ('k * 'v) list [@@deriving typerep]
  let empty : ('k, 'v) t = BigMap []
  let get k (BigMap kvs) = Stdlib.List.assoc_opt k kvs
  let mem k (BigMap kvs) = Stdlib.List.mem_assoc k kvs
  let update k vo (BigMap kvs) = 
    BigMap (match vo with
        | None -> 
            let rec remove st = function
              | [] -> kvs
              | (k',_)::_ when k < k' -> kvs
              | (k',_)::kvs when k = k' -> List.(rev (rev_append kvs st))
              | kv::kvs -> remove (kv::st) kvs
            in
            remove [] kvs
        | Some v ->
            let rec add st = function
              | [] -> List.rev ((k,v)::st)
              | ((k',_)::_ as kvs) when k < k' -> 
                  List.rev_append kvs ((k,v)::st)
              | (k',_)::kvs when k = k' -> List.rev_append kvs ((k,v)::st)
              | kv::kvs -> add (kv::st) kvs
            in
            add [] kvs)
end

module Loop = struct
  let rec left f a = match f a with
    | Left a -> left f a
    | Right b -> b
end
  
module String = struct
  let concat = (^)
  let slice (Nat a) (Nat b) s =
    try Some (String.sub s a b) with _ -> None
  let length s = Nat (String.length s)
end

let (^) = String.concat

type bytes = Bytes of string const [@@deriving typerep]

module Bytes = struct
  type t = bytes [@@deriving typerep]
  let concat (Bytes a) (Bytes b) = Bytes (a ^ b)
  let slice (Nat a) (Nat b) (Bytes s) =
    try Some (Bytes (Stdlib.String.sub s (Stdlib.( * ) a 2) (Stdlib.( * ) b 2))) with _ -> None
  let length (Bytes a) = Nat (Stdlib.(/) (Stdlib.String.length a) 2)
      
  let of_ocaml_string s = 
    let `Hex hs = Hex.of_string s in
    Bytes hs

  let to_ocaml_string (Bytes hs) = Hex.to_string (`Hex hs)
end

type address = Address of string const [@@deriving typerep]
module Address = struct
  type t = address [@@deriving typerep]
end

type key_hash = Key_hash of string const [@@deriving typerep]
module Key_hash = struct
  type t = key_hash [@@deriving typerep]
end

type 'a contract =
   | Self : 'a contract
   | Of_address : address -> 'a contract
   | Implicit_account : key_hash -> unit contract

type source =
  | File of string
  | Code of string

type operation =
  | Transfer_tokens : 'a * tz * 'a contract -> operation
  | Set_delegate : key_hash option -> operation
  | Originate : source * key_hash option * tz * 'storage -> operation

type operations = operation list

type ('param, 'storage) entry = 'param -> 'storage -> operations * 'storage

module Contract : sig
  type 'a t = 'a contract
  val self : 'a t
  val contract : address -> 'a t option
  val contract' : address -> string const -> 'a t option
  val implicit_account : key_hash -> unit t
  val address : 'a t -> address

  val create_from_tz_code : string const -> key_hash option -> tz -> 'storage -> operation * address
  (** Raw interface for CREATE_CONTRACT.
  
      Michelson code must be given as a string LITERAL.
      In Tezos you cannot generate contract code programically in a contract.

      The types of the contract and the initial storage are NOT checked 
      by SCaml.
  *)

  val create_raw : string const -> key_hash option -> tz -> 'storage -> operation * address
  (** Same as [create_from_tz_code] *)

  val create_from_tz_file : string const -> key_hash option -> tz -> 'storage -> operation * address
  (** CREATE_CONTRACT from a michelson source file.
  
      Michelson file name must be given as a string literal.
      In Tezos you cannot generate contract code programically in a contract.

      The types of the contract and the initial storage are NOT checked 
      by SCaml.
  *)
end = struct
  type 'a t = 'a contract
  let self = Self
  let contract a = Some (Of_address a) (* always succeeds *)
  let contract' _a _s = assert false
  let implicit_account kh = Implicit_account kh
  let address (type a) (c : a contract) = match c with
    | Of_address a -> a
    | Implicit_account _kh -> assert false (* XXX *)
    | Self -> assert false (* XXX *)
  let create_raw code kho tz storage =
    Originate (Code code, kho, tz, storage),
    Address (assert false)
  let create_from_tz_code = create_raw
  let create_from_tz_file file kho tz storage =
    Originate (File file, kho, tz, storage),
    Address (assert false)
end

module Operation = struct
  type t = operation
  let transfer_tokens a tz c = Transfer_tokens (a, tz, c)
  let set_delegate kho = Set_delegate kho
end

type timestamp = Timestamp of string const [@@deriving typerep]

module Timestamp = struct
  type t = timestamp [@@deriving typerep]

  let add (Timestamp t) (Int i) =
    match Ptime.of_rfc3339 t with
    | Error _ -> assert false
    | Ok (utc, _, _) -> 
        match Ptime.add_span utc (Ptime.Span.of_int_s i) with
        | None -> assert false
        | Some pt -> Timestamp (Ptime.to_rfc3339 pt)
      
  let sub (Timestamp t) (Int i) =
    match Ptime.of_rfc3339 t with
    | Error _ -> assert false
    | Ok (utc, _, _) -> 
        match Ptime.sub_span utc (Ptime.Span.of_int_s i) with
        | None -> assert false
        | Some pt -> Timestamp (Ptime.to_rfc3339 pt)
      
  let diff (Timestamp t1) (Timestamp t2) =
    let t1 = match Ptime.of_rfc3339 t1 with
      | Error _ -> assert false
      | Ok (utc, _, _) -> utc 
    in
    let t2 = match Ptime.of_rfc3339 t2 with
      | Error _ -> assert false
      | Ok (utc, _, _) -> utc 
    in
    Int (int_of_float (Ptime.Span.to_float_s (Ptime.diff t1 t2)))
end

type chain_id = Chain_id of string const [@@deriving typerep]

module Chain_id = struct
  type t = chain_id [@@deriving typerep]
end

module Env = struct
  type t = 
    { now : timestamp
    ; amount : tz
    ; balance : tz
    ; source : address
    ; sender : address
    ; chain_id : chain_id
    }
    
  let env = ref (None : t option)
  let get () = Option.get !env

  let now      env = env.now
  let amount   env = env.amount
  let balance  env = env.balance
  let source   env = env.source
  let sender   env = env.sender
  let chain_id env = env.chain_id
end

(* maybe the place is not good *)
module Global : sig
  val get_now      : unit -> timestamp
  val get_amount   : unit -> tz
  val get_balance  : unit -> tz
  val get_source   : unit -> address
  val get_sender   : unit -> address
  val get_chain_id : unit -> chain_id
end = struct
  open Env
  let get_now      () = (get ()).now
  let get_amount   () = (get ()).amount
  let get_balance  () = (get ()).balance
  let get_source   () = (get ()).source
  let get_sender   () = (get ()).sender
  let get_chain_id () = (get ()).chain_id
end

type key = Key of string const [@@deriving typerep]

module Key = struct
  type t = key [@@deriving typerep]
end

type signature = Signature of string const [@@deriving typerep]

module Signature = struct
  type t = signature [@@deriving typerep]
end

module Obj = struct
  let pack_string s =
    let open Tezos_micheline in
    let s = Micheline.strip_locations (Micheline.String ((), s)) in
    let expr_encoding =
      Micheline.canonical_encoding_v1
        ~variant:"michelson_v1"
        Data_encoding.Encoding.string
    in
    match Data_encoding.(Binary.to_bytes expr_encoding s) with
    | Error _ -> assert false
    | Ok bs -> Bytes.of_ocaml_string ("\005" ^ Stdlib.Bytes.to_string bs)
    
  let pack : 'a -> bytes = fun _ -> assert false
  let unpack : bytes -> 'a option = fun _ -> assert false

  module Internal = struct
    module type TypeSafePack = sig
      val pack' : 'a Typerep.t -> 'a -> ocaml_string
      val unpack' : 'a Typerep.t -> ocaml_string -> 'a option
    end
  
    let type_safe_pack = ref (None : (module TypeSafePack) option)
  end

  let pack' rep a =
    let (module M : Internal.TypeSafePack) = Option.get !Internal.type_safe_pack in
    Bytes.of_ocaml_string (M.pack' rep a)
    
  let unpack' rep b =
    let (module M : Internal.TypeSafePack) = Option.get !Internal.type_safe_pack in
    M.unpack' rep (Bytes.to_ocaml_string b)
end

module Crypto = struct
  (* XXX These functions should have polymorphic versions *)

  let check_signature : key -> signature -> bytes -> bool = fun _ -> assert false
  (* XXX Signature.check key signature message of tezos-crypto *)

  let blake2b bs = 
    let Hash bs = Blake2.Blake2b.direct (Stdlib.Bytes.of_string @@ Bytes.to_ocaml_string bs) 32 in 
    Bytes.of_ocaml_string @@ Stdlib.Bytes.to_string bs

  (* test *)
  let test_blake2b () =
    let Bytes s = blake2b (Obj.pack_string "foobar") in
    assert (s = "c5b7e76c15ce98128a840b54c38f462125766d2ed3a6bff0e76f7f3eb415df04")

  let sha256 bs = 
    Bytes.of_ocaml_string
    @@ Bigstring.to_string 
    @@ Hacl.Hash.SHA256.digest 
    @@ Bigstring.of_string 
    @@ Bytes.to_ocaml_string bs

  (* test *)
  let test_sha256 () =
    assert (sha256 (Bytes "0123456789ABCDEF") =
            Bytes "55c53f5d490297900cefa825d0c8e8e9532ee8a118abe7d8570762cd38be9818")

  let sha512 bs = 
    Bytes.of_ocaml_string
    @@ Bigstring.to_string 
    @@ Hacl.Hash.SHA512.digest 
    @@ Bigstring.of_string 
    @@ Bytes.to_ocaml_string bs

  (* test *)
  let test_sha512 () =
    assert (sha512 (Bytes "0123456789ABCDEF") =
            Bytes "650161856da7d9f818e6047cf6b2092bc7aa3767d3495cfbefe2b710ed684a43ba933ea8286ef67d975e64e0482e5ebe0701788989396545b6badb3b0a136f19")

  let hash_key  : key -> key_hash = fun _ -> assert false
  (* XXX we need Signagure.Public_key.hash 
         of tezos-crypto but it requires 
         the current OPAM package of hacl which required dune < 2.0 for now *)

  module Internal = struct
    let test () =
      test_blake2b ();
      test_sha256 ();
      test_sha512 ()
  end
end
