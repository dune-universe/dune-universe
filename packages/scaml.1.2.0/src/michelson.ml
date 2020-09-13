(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Spotlib.Spot
open Tools

(* Micheline parser and printer tools *)
module Micheline = struct
  open Tezos_micheline.Micheline

  type t = Tezos_micheline.Micheline_printer.node
  (* Comment as Location *)

  type parsed = (canonical_location, string) node

  let to_parsed t = root @@ strip_locations t

  open Tezos_micheline.Micheline_printer

  let no_comment = { comment = None }

  let add_comment c n = match c, n with
    | None, _ -> n
    | Some _, Int ({comment=None}, x) -> Int ({comment= c}, x)
    | Some _, String ({comment=None}, x) -> String ({comment= c}, x)
    | Some _, Bytes ({comment=None}, x) -> Bytes ({comment= c}, x)
    | Some _, Prim ({comment=None}, x, y, z) -> Prim ({comment= c}, x, y, z)
    | Some _, Seq ({comment=None}, x) -> Seq ({comment= c}, x)
    | Some s1, Int ({comment=Some s2}, x) ->
        Int ({comment= Some (s1 ^ ", " ^ s2)}, x)
    | Some s1, String ({comment=Some s2}, x) ->
        String ({comment= Some (s1 ^ ", " ^ s2)}, x)
    | Some s1, Bytes ({comment=Some s2}, x) ->
        Bytes ({comment= Some (s1 ^ ", " ^ s2)}, x)
    | Some s1, Prim ({comment=Some s2}, x, y, z) ->
        Prim ({comment= Some (s1 ^ ", " ^ s2)}, x, y, z)
    | Some s1, Seq ({comment=Some s2}, x) ->
        Seq ({comment= Some (s1 ^ ", " ^ s2)}, x)

  let string s = String (no_comment, s)
  let bytes s =
    Bytes (no_comment, Tezos_stdlib.MBytes.of_string & Hex.to_string (`Hex s))
  let int n = Int (no_comment, n)
  let prim s ts annots = Prim (no_comment, s, ts, annots)
  let seq ts = Seq (no_comment, ts)

  let pp = print_expr_unwrapped

  let parse_expression_string s =
    let open Tezos_micheline.Micheline_parser in
    let open Result.Infix in
    no_parsing_error (tokenize s) >>= fun tokens ->
    no_parsing_error (parse_expression tokens) >>| fun node ->
    root & strip_locations node
end

module Mline = Micheline

module Type = struct

  (* Michelson type.  This is shamelessly used as types for IML, too. *)
  type t = { desc : desc
           ; tyannot : string option
           }

  and desc =
    | TyString
    | TyNat
    | TyInt
    | TyBytes
    | TyBool
    | TyUnit
    | TyList of t
    | TyPair of string option * t * string option * t
    | TyOption of string option * t
    | TyOr of string option * t * string option * t
    | TySet of t (* comparable *)
    | TyMap of t (* comparable *) * t
    | TyBigMap of t (* comparable *) * t

    | TyMutez
    | TyKeyHash
    | TyTimestamp
    | TyAddress
    | TyChainID

    | TyKey
    | TySignature
    | TyOperation
    | TyContract of t
    | TyLambda of t * t

    | TyNever (* from 008 *)

  let type_annotate f t = { t with tyannot= f t.tyannot }

  let mk desc = { desc ; tyannot= None  }

  let tyString              = mk TyString
  let tyNat                 = mk TyNat
  let tyInt                 = mk TyInt
  let tyBytes               = mk TyBytes
  let tyBool                = mk TyBool
  let tyUnit                = mk TyUnit
  let tyList t              = mk & TyList t
  let tyPair (f1,t1, f2,t2)       = mk & TyPair (f1,t1, f2,t2)
  let tyOption (f,t)            = mk & TyOption (f,t)
  let tyOr (f1,t1, f2,t2)         = mk & TyOr (f1,t1, f2,t2)
  let tySet t               = mk & TySet t
  let tyMap (t1, t2)        = mk & TyMap (t1, t2)
  let tyBigMap (t1, t2)     = mk & TyBigMap (t1, t2)
  let tyMutez               = mk TyMutez
  let tyKeyHash             = mk TyKeyHash
  let tyTimestamp           = mk TyTimestamp
  let tyAddress             = mk TyAddress
  let tyChainID             = mk TyChainID
  let tyKey                 = mk TyKey
  let tySignature           = mk TySignature
  let tyOperation           = mk TyOperation
  let tyContract t          = mk & TyContract t
  let tyLambda (t1, t2)     = mk & TyLambda (t1, t2)
  let tyNever               = mk & TyNever

  let rec args t = match t.desc with
    | TyLambda (t1, t2) ->
        let ts, t = args t2 in
        t1::ts, t
    | _ -> [], t

  let rec to_micheline t =
    let prim n args =
      let attrs =
        (match t.tyannot with Some s -> [":"^s] | None -> [])
      in
      Mline.prim n args attrs
    in
    (* XXX should not use Tezos_micheline *)
    let add_field f t = match f, t with
      | None, _ -> t
      | Some f, Tezos_micheline.Micheline.Prim (c,s,ts,annots) ->
          Tezos_micheline.Micheline.Prim (c,s,ts,("%"^f)::annots)
      | _ -> t
    in
    let (!) x = prim x [] in
    match t.desc with
    | TyString -> !"string"
    | TyNat    -> !"nat"
    | TyInt    -> !"int"
    | TyBytes  -> !"bytes"
    | TyBool   -> !"bool"
    | TyUnit   -> !"unit"
    | TyList t -> prim "list" [to_micheline t]
    | TyPair (f1,t1, f2,t2)   -> prim "pair" [add_field f1 & to_micheline t1
                                             ;add_field f2 & to_micheline t2]
    | TyOption (f,t)        -> prim "option" [add_field f & to_micheline t]
    | TyOr (f1,t1, f2,t2)     -> prim "or" [add_field f1 & to_micheline t1;
                                            add_field f2 & to_micheline t2]
    | TySet t           -> prim "set" [to_micheline t]
    | TyMap (t1, t2)    -> prim "map" [to_micheline t1; to_micheline t2]
    | TyBigMap (t1, t2) -> prim "big_map" [to_micheline t1; to_micheline t2]

    | TyMutez     -> !"mutez"
    | TyKeyHash   -> !"key_hash"
    | TyTimestamp -> !"timestamp"
    | TyAddress   -> !"address"
    | TyChainID   -> !"chain_id"

    | TyKey       -> !"key"
    | TySignature -> !"signature"
    | TyOperation -> !"operation"
    | TyContract t -> prim "contract" [to_micheline t]
    | TyLambda (t1, t2) -> prim "lambda" [to_micheline t1; to_micheline t2]
    | TyNever -> !"never"

  and pp fmt t = Mline.pp fmt & to_micheline t

  let rec validate ty =
    let open Result.Infix in
    let rec f ty = match ty.desc with
      | TyBigMap (k, v) ->
          f k >>= fun () -> f v >>= fun () ->
          if not (is_comparable k) then
            Error (ty, "big_map's key type must be comparable")
          else if not (is_packable ~legacy:false v) then
            Error (ty, "big_map's value type must be packable")
          else
            Ok ()

      | TySet e ->
          f e >>= fun () ->
          if not (is_comparable e) then
            Error (ty, "set's element type must be comparable")
          else
            Ok ()

      | TyMap (k, v) ->
          f k >>= fun () -> f v >>= fun () ->
          if not (is_comparable k) then
            Error (ty, "map's key type must be comparable")
          else
            Ok ()

      | TyContract p ->
          f p >>= fun () ->
          if not (is_parameterable p) then
            Error (ty, "contract's parameter type cannot contain operation")
          else
            Ok ()

      | (TyList ty | TyOption (_,ty)) -> f ty
      | (TyPair (_,ty1, _,ty2) | TyOr (_,ty1, _,ty2) | TyLambda (ty1, ty2)) ->
          f ty1 >>= fun () -> f ty2

      | TyString | TyNat | TyInt | TyBytes | TyBool | TyUnit | TyMutez
      | TyKeyHash | TyTimestamp | TyAddress |TyChainID | TyKey | TySignature
      | TyOperation -> Ok ()
      | TyNever -> Ok ()
    in
    f ty

  and is_comparable ty =
    (* See Script_ir_translator.parse_comparable_ty *)
    let rec f ty = match ty.desc with

      | TyChainID | TySignature | TyKey | TyKeyHash | TyUnit
        when Conf.get_protocol () >= (8,0) -> true (* since 008 *)

      | TyOption (_,ty) when Conf.get_protocol () >= (8,0) -> is_comparable ty

      | TyOr (_,ty1, _,ty2) when Conf.get_protocol () >= (8,0) ->
          is_comparable ty1 && is_comparable ty2

      | TyChainID | TySignature | TyKey -> false

      | TyString | TyNat | TyInt | TyBytes | TyBool | TyMutez
      | TyKeyHash | TyTimestamp | TyAddress -> true

      | TyPair (_,ty1, _,ty2) -> f ty1 && f ty2 (* since 005_Babylon *)

      | TyBigMap _ | TyContract _
      | TyOption _ | TyLambda _ | TyList _ | TyMap _ | TyOperation
      | TyOr _ | TySet _ | TyUnit -> false
      | TyNever -> false
    in
    f ty

  and is_packable ~legacy ty =
    (* leagcy: allow to pack contracts for hash/signature checks
       See Script_ir_translator.I_PACK case.
    *)
    let rec f ty = match ty.desc with
      | TyBigMap _ -> false
      | TyOperation -> false
      | TyContract _ -> legacy
      | TyLambda (_t1, _t2) -> true
      | TyList t | TyOption (_,t) | TySet t -> f t
      | TyPair (_,t1, _,t2) | TyOr (_,t1, _,t2) | TyMap (t1, t2) -> f t1 && f t2
      | TyString | TyNat | TyInt | TyBytes | TyBool | TyUnit
      | TyMutez | TyKeyHash | TyTimestamp | TyAddress | TyChainID
      | TyKey | TySignature -> true
      | TyNever -> false
    in
    f ty

  and is_parameterable ty =
      (* ~allow_big_map:true
         ~allow_operation:false
         ~allow_contract:true
      *)
    let rec f ty = match ty.desc with
      | TyBigMap _ -> true
      | TyOperation -> false
      | TyContract _ -> true

      | TyList t | TyOption (_,t) | TySet t -> f t
      | TyLambda (_t1, _t2) -> true
      | TyPair (_,t1, _,t2) | TyOr (_,t1, _,t2) | TyMap (t1, t2) -> f t1 && f t2
      | TyString | TyNat | TyInt | TyBytes | TyBool | TyUnit
      | TyMutez | TyKeyHash | TyTimestamp | TyAddress | TyChainID
      | TyKey | TySignature -> true
      | TyNever -> false
    in
    f ty

  and is_storable ty =
      (* ~allow_big_map:true
         ~allow_operation:false
         ~allow_contract:legacy -> false
      *)
    let rec f ty = match ty.desc with
      | TyBigMap _ -> true
      | TyOperation -> false
      | TyContract _ -> false

      | TyList t | TyOption (_,t) | TySet t -> f t
      | TyLambda (_t1, _t2) -> true
      | TyPair (_,t1, _,t2) | TyOr (_,t1, _,t2) | TyMap (t1, t2) -> f t1 && f t2
      | TyString | TyNat | TyInt | TyBytes | TyBool | TyUnit
      | TyMutez | TyKeyHash | TyTimestamp | TyAddress | TyChainID
      | TyKey | TySignature -> true
      | TyNever -> false
    in
    f ty
end

module rec Constant : sig
  type t =
    | Unit
    | Bool of bool
    | Int of Z.t
    | String of string
    | Bytes of string
    | Option of t option
    | List of t list
    | Set of t list
    | Map of (t * t) list
    | Pair of t * t
    | Left of t
    | Right of t
    | Timestamp of Z.t
    | Code of Opcode.t list

  val pp : Format.formatter -> t -> unit
  val to_micheline : ?block_comment:bool -> t -> Mline.t
end = struct
  type t =
    | Unit
    | Bool of bool
    | Int of Z.t
    | String of string
    | Bytes of string
    | Option of t option
    | List of t list
    | Set of t list
    | Map of (t * t) list
    | Pair of t * t
    | Left of t
    | Right of t
    | Timestamp of Z.t
    | Code of Opcode.t list

  let to_micheline ?block_comment =
    let open Mline in
    let rec f = function
      | Bool true  -> prim "True" [] []
      | Bool false -> prim "False" [] []
      | Unit     -> prim "Unit" [] []
      | Int n    -> int n
      | String s -> string s
      | Bytes s (* in hex *) ->  bytes s
      | Option None -> prim "None" [] []
      | Option (Some t) -> prim "Some" [f t] []
      | Pair (t1, t2) -> prim "Pair" [f t1; f t2] []
      | Left t -> prim "Left" [f t] []
      | Right t -> prim "Right" [f t] []
      | List ts -> seq (List.map f ts)
      | Set ts -> seq (List.map f & List.sort compare ts)
      | Map xs ->
          seq (List.map (fun (k,v) ->
              prim "Elt" [f k; f v] []) xs)
      | Timestamp z ->
          begin match Ptime.of_float_s @@ Z.to_float z with
            | None -> assert false
            | Some t -> string (Ptime.to_rfc3339 ~space:false ~frac_s:0 t)
          end
      | Code os ->
          seq (List.concat_map (Opcode.to_micheline ?block_comment) os)
    in
    f

  let pp fmt t = Mline.pp fmt & to_micheline t
end

and Opcode : sig
  type module_ =
    | Raw of Tezos_micheline.Micheline_printer.node list

  type t =
    | DUP
    | DIP of int * t list
    | DIG of int
    | DUG of int
    | DROP of int
    | SWAP
    | PAIR
    | ASSERT
    | CAR
    | CDR
    | LEFT of Type.t
    | RIGHT of Type.t
    | LAMBDA of Type.t * Type.t * t list
    | APPLY
    | PUSH of Type.t * Constant.t
    | NIL of Type.t
    | CONS
    | NONE of Type.t
    | SOME
    | COMPARE
    | EQ
    | LT
    | LE
    | GT
    | GE
    | NEQ
    | IF of t list * t list
    | ADD | SUB | MUL | EDIV | ABS | ISNAT | NEG | LSL | LSR
    | AND | OR | XOR | NOT
    | EXEC
    | IF_NONE of t list * t list
    | IF_LEFT of t list * t list
    | IF_CONS of t list * t list
    | FAILWITH
    | COMMENT of string * t list
    | UNIT
    | EMPTY_SET of Type.t
    | EMPTY_MAP of Type.t * Type.t
    | EMPTY_BIG_MAP of Type.t * Type.t
    | SIZE
    | MEM
    | UPDATE
    | ITER of t list
    | MAP of t list
    | LOOP of t list (* It is not really useful for SCaml *)
    | LOOP_LEFT of t list
    | CONCAT
    | SELF
    | GET
    | RENAME of string (* for debugging *)
    | PACK
    | UNPACK of Type.t
    | SLICE
    | CAST (* to remove type name. *)
    | CONTRACT of Type.t
    | CONTRACT' of Type.t * string (* entry point name *)
    | TRANSFER_TOKENS
    | SET_DELEGATE
    | CREATE_ACCOUNT (* Obsolete from 008 *)
    | CREATE_CONTRACT of module_ (* Legacy version is obsolete from 008 *)
    | IMPLICIT_ACCOUNT
    | NOW
    | AMOUNT
    | BALANCE
    | CHECK_SIGNATURE
    | BLAKE2B
    | SHA256
    | SHA512
    | HASH_KEY
    | STEPS_TO_QUOTA (* Obsolete from 008 *)
    | SOURCE
    | SENDER
    | ADDRESS
    | CHAIN_ID

    (* 008 *)
    | LEVEL
    | SELF_ADDRESS
    | UNPAIR

  val pp : Format.formatter -> t -> unit
  val to_micheline : ?block_comment:bool -> t -> Mline.t list
  val clean_failwith : t list -> t list * bool
  val dip_1_drop_n_compaction : t list -> t list
end = struct

  type module_ =
    | Raw of Tezos_micheline.Micheline_printer.node list
    (* | { parameter : Type.t ; storage : Type.t ; code : t list } *)

  type t =
    | DUP
    | DIP of int * t list
    | DIG of int
    | DUG of int
    | DROP of int
    | SWAP
    | PAIR
    | ASSERT
    | CAR | CDR
    | LEFT of Type.t
    | RIGHT of Type.t
    | LAMBDA of Type.t * Type.t * t list
    | APPLY
    | PUSH of Type.t * Constant.t
    | NIL of Type.t
    | CONS
    | NONE of Type.t
    | SOME
    | COMPARE
    | EQ | LT | LE | GT | GE | NEQ
    | IF of t list * t list
    | ADD | SUB | MUL | EDIV | ABS | ISNAT | NEG | LSL | LSR
    | AND | OR | XOR | NOT
    | EXEC
    | IF_NONE of t list * t list
    | IF_LEFT of t list * t list
    | IF_CONS of t list * t list
    | FAILWITH
    | COMMENT of string * t list
    | UNIT
    | EMPTY_SET of Type.t
    | EMPTY_MAP of Type.t * Type.t
    | EMPTY_BIG_MAP of Type.t * Type.t
    | SIZE
    | MEM
    | UPDATE
    | ITER of t list
    | MAP of t list
    | LOOP of t list (* It is not really useful for SCaml *)
    | LOOP_LEFT of t list
    | CONCAT
    | SELF
    | GET
    | RENAME of string (* for debugging *)
    | PACK
    | UNPACK of Type.t
    | SLICE
    | CAST (* to remove type name. *)
    | CONTRACT of Type.t
    | CONTRACT' of Type.t * string
    | TRANSFER_TOKENS
    | SET_DELEGATE
    | CREATE_ACCOUNT (* deprecated *)
    | CREATE_CONTRACT of module_
    | IMPLICIT_ACCOUNT
    | NOW
    | AMOUNT
    | BALANCE
    | CHECK_SIGNATURE
    | BLAKE2B
    | SHA256
    | SHA512
    | HASH_KEY
    | STEPS_TO_QUOTA
    | SOURCE
    | SENDER
    | ADDRESS
    | CHAIN_ID

    (* 008 *)
    | LEVEL
    | SELF_ADDRESS
    | UNPAIR

  let to_micheline ?(block_comment=false) t =
    let open Mline in
    let prim x args = Mline.prim x args [] in
    let (!) x = prim x [] in
    let rec fs ts = seq (List.concat_map f' ts)
    and f' = function
      | COMMENT (s, ts) when block_comment ->
          [ add_comment (Some s) @@ fs ts ]
      | COMMENT (s, ts) ->
          begin match List.concat_map f' ts with
            | [] -> [] (* comment against empty seq is gone *)
            | t::ts -> add_comment (Some s) t :: ts
          end
      | t -> [f t]
    and f = function
      | COMMENT _ -> assert false
      | DUP -> !"DUP"
      | DIP (1, code) -> prim "DIP" [fs code]
      | DIP (n, code) -> prim "DIP" [int & Z.of_int n; fs code]
      | DIG n -> prim "DIG" [int & Z.of_int n]
      | DUG n -> prim "DUG" [int & Z.of_int n]
      | SWAP -> !"SWAP"
      | PAIR -> !"PAIR"
      | PUSH (ty, const) -> prim "PUSH" [Type.to_micheline ty; Constant.to_micheline const]
      | ASSERT -> !"ASSERT"
      | CAR -> !"CAR"
      | CDR -> !"CDR"
      | LEFT ty -> prim "LEFT" [Type.to_micheline ty]
      | RIGHT ty -> prim "RIGHT" [Type.to_micheline ty]
      | LAMBDA (ty1, ty2, code) ->
          prim "LAMBDA" [Type.to_micheline ty1;
                         Type.to_micheline ty2;
                         fs code]
      | APPLY -> !"APPLY"
      | CONS -> !"CONS"
      | NIL ty -> prim "NIL" [ Type.to_micheline ty ]
      | SOME -> !"SOME"
      | NONE ty -> prim "NONE" [ Type.to_micheline ty ]
      | DROP 1 -> !"DROP"
      | DROP n -> prim "DROP" [int & Z.of_int n]
      | COMPARE -> !"COMPARE"
      | EQ  -> !"EQ"
      | LT  -> !"LT"
      | LE  -> !"LE"
      | GT  -> !"GT"
      | GE  -> !"GE"
      | NEQ -> !"NEQ"
      | IF (t,e) -> prim "IF" [ fs t; fs e ]
      | IF_NONE (t,e) -> prim "IF_NONE" [ fs t; fs e ]
      | ADD   -> !"ADD"
      | SUB   -> !"SUB"
      | MUL   -> !"MUL"
      | EDIV  -> !"EDIV"
      | ABS   -> !"ABS"
      | ISNAT -> !"ISNAT"
      | NEG   -> !"NEG"
      | LSL   -> !"LSL"
      | LSR   -> !"LSR"
      | AND   -> !"AND"
      | OR    -> !"OR"
      | XOR   -> !"XOR"
      | NOT   -> !"NOT"

      | EXEC -> !"EXEC"
      | FAILWITH -> !"FAILWITH"
      | IF_LEFT (t1, t2) -> prim "IF_LEFT" [ fs t1; fs t2 ]
      | IF_CONS (t1, t2) -> prim "IF_CONS" [ fs t1; fs t2 ]
      | UNIT -> !"UNIT"
      | EMPTY_SET ty -> prim "EMPTY_SET" [ Type.to_micheline ty ]
      | EMPTY_MAP (ty1, ty2) -> prim "EMPTY_MAP" [ Type.to_micheline ty1; Type.to_micheline ty2 ]
      | EMPTY_BIG_MAP (ty1, ty2) -> prim "EMPTY_BIG_MAP" [ Type.to_micheline ty1; Type.to_micheline ty2 ]
      | SIZE   -> !"SIZE"
      | MEM    -> !"MEM"
      | UPDATE -> !"UPDATE"
      | ITER code -> prim "ITER" [ fs code ]
      | MAP code -> prim "MAP" [ fs code ]
      | LOOP code -> prim "LOOP" [ fs code ]
      | LOOP_LEFT code -> prim "LOOP_LEFT" [ fs code ]
      | CONCAT -> !"CONCAT"
      | SELF   -> !"SELF"
      | GET    -> !"GET"
      | RENAME s -> prim "RENAME" [string s]
      | PACK -> !"PACK"
      | UNPACK ty -> prim "UNPACK" [Type.to_micheline ty]
      | SLICE -> !"SLICE"
      | CAST  -> !"CAST"
      | CONTRACT ty -> prim "CONTRACT" [Type.to_micheline ty]
      | CONTRACT' (ty, n) ->  Mline.prim "CONTRACT" [Type.to_micheline ty] ["%" ^ n]
      | TRANSFER_TOKENS  -> !"TRANSFER_TOKENS"
      | SET_DELEGATE     -> !"SET_DELEGATE"
      | CREATE_ACCOUNT   -> !"CREATE_ACCOUNT"
      | IMPLICIT_ACCOUNT -> !"IMPLICIT_ACCOUNT"
      | NOW              -> !"NOW"
      | AMOUNT           -> !"AMOUNT"
      | BALANCE          -> !"BALANCE"

      | CHECK_SIGNATURE -> !"CHECK_SIGNATURE"
      | BLAKE2B         -> !"BLAKE2B"
      | SHA256          -> !"SHA256"
      | SHA512          -> !"SHA512"
      | HASH_KEY        -> !"HASH_KEY"

      | STEPS_TO_QUOTA  -> !"STEPS_TO_QUOTA"
      | SOURCE          -> !"SOURCE"
      | SENDER          -> !"SENDER"
      | ADDRESS         -> !"ADDRESS"
      | CHAIN_ID        -> !"CHAIN_ID"
      | CREATE_CONTRACT (Raw nodes) -> prim "CREATE_CONTRACT" [ seq nodes ]

      | LEVEL           -> !"LEVEL"
      | SELF_ADDRESS    -> !"SELF_ADDRESS"
      | UNPAIR          -> !"UNPAIR"
    in
    f' t

  let pp ppf ts = Format.fprintf ppf "%a" (Format.list "@ " Mline.pp) & to_micheline ts

  let rec clean_failwith = function
    | [] -> [], false
    | x::xs ->
        let x, end_with_failwith = aux x in
        if end_with_failwith then [x], true
        else begin
          let xs, b = clean_failwith xs in
          x :: xs, b
        end

  and clean_failwith' xs = fst @@ clean_failwith xs
  and aux = function
    | FAILWITH -> FAILWITH, true
    | DIP (n, ts) ->
        let ts, b = clean_failwith ts in
        DIP (n, ts), b
    | ITER ts -> ITER (clean_failwith' ts), false
    | MAP ts -> MAP (clean_failwith' ts), false
    | LAMBDA (ty1, ty2, ts) -> LAMBDA (ty1, ty2, clean_failwith' ts), false
    | IF (t1, t2) ->
        let t1, b1 = clean_failwith t1 in
        let t2, b2 = clean_failwith t2 in
        IF (t1, t2), b1 && b2
    | IF_NONE (t1, t2) ->
        let t1, b1 = clean_failwith t1 in
        let t2, b2 = clean_failwith t2 in
        IF_NONE (t1, t2), b1 && b2
    | IF_LEFT (t1, t2) ->
        let t1, b1 = clean_failwith t1 in
        let t2, b2 = clean_failwith t2 in
        IF_LEFT (t1, t2), b1 && b2
    | IF_CONS (t1, t2) ->
        let t1, b1 = clean_failwith t1 in
        let t2, b2 = clean_failwith t2 in
        IF_CONS (t1, t2), b1 && b2
    | COMMENT (s, t) ->
        let t, b = clean_failwith t in
        COMMENT (s, t), b
    | LOOP ts -> LOOP (clean_failwith' ts), false
    | LOOP_LEFT ts -> LOOP_LEFT (clean_failwith' ts), false
    | CREATE_CONTRACT m -> CREATE_CONTRACT m, false
    | PUSH (ty, c) -> PUSH (ty, constant c), false
    | (DUP
      | DIG _ | DUG _ | DROP _
      | SWAP
      | PAIR
      | ASSERT
      | CAR | CDR
      | LEFT _ | RIGHT _
      | NIL _
      | CONS
      | NONE _
      | SOME
      | COMPARE
      | EQ | LT | LE | GT | GE | NEQ
      | ADD | SUB | MUL | EDIV | ABS | ISNAT | NEG | LSL | LSR
      | AND | OR | XOR | NOT
      | EXEC
      | UNIT
      | EMPTY_SET _ | EMPTY_MAP _ | EMPTY_BIG_MAP _
      | SIZE
      | MEM
      | UPDATE
      | CONCAT
      | SELF
      | GET
      | RENAME _
      | PACK | UNPACK _
      | SLICE
      | CAST

      | CONTRACT _
      | CONTRACT' _
      | TRANSFER_TOKENS
      | SET_DELEGATE
      | CREATE_ACCOUNT
      | IMPLICIT_ACCOUNT
      | NOW
      | AMOUNT
      | BALANCE
      | CHECK_SIGNATURE
      | BLAKE2B
      | SHA256
      | SHA512
      | HASH_KEY
      | STEPS_TO_QUOTA
      | SOURCE
      | SENDER
      | ADDRESS
      | APPLY
      | CHAIN_ID
      | UNPAIR
      | SELF_ADDRESS
      | LEVEL
      as t) -> t, false

  and constant =
    let open Constant in
    function
    | Code ops -> Code (clean_failwith' ops)
    | Option (Some t) -> Option (Some (constant t))
    | List ts -> List (List.map constant ts)
    | Set ts -> Set (List.map constant ts)
    | Map kvs -> Map (List.map (fun (k,v) -> (constant k, constant v)) kvs)
    | Pair (t1, t2) -> Pair (constant t1, constant t2)
    | Left t -> Left (constant t)
    | Right t -> Right (constant t)
    | (Unit | Bool _ | Int _ | String _ | Bytes _ | Timestamp _ | Option None as c) -> c

  let dip_1_drop_n_compaction ts =
    let rec loop n comments = function
      | DIP (1, [DROP m]) :: ts -> loop (n + m) comments ts
      | COMMENT (c, [DIP (1, [DROP m])]) :: ts -> loop (n + m) (c :: comments) ts
      | ts when n > 0 ->
          if comments <> [] then
            COMMENT (String.concat ", " (List.rev comments),
                     [ DIP (1, [DROP n]) ]) :: loop 0 [] ts
          else
            DIP (1, [DROP n]) :: loop 0 [] ts
      | [] -> []
      | t :: ts ->
          let t' = match t with
            | DIP (n, ts) -> DIP (n, loop 0 [] ts)
            | LAMBDA (t1, t2, ts) -> LAMBDA (t1, t2, loop 0 [] ts)
            | IF (ts1, ts2) -> IF (loop 0 [] ts1, loop 0 [] ts2)
            | IF_NONE (ts1, ts2) -> IF_NONE (loop 0 [] ts1, loop 0 [] ts2)
            | IF_LEFT (ts1, ts2) -> IF_LEFT (loop 0 [] ts1, loop 0 [] ts2)
            | IF_CONS (ts1, ts2) -> IF_CONS (loop 0 [] ts1, loop 0 [] ts2)
            | COMMENT (c, ts) -> COMMENT (c, loop 0 [] ts)
            | ITER ts -> ITER (loop 0 [] ts)
            | MAP ts -> MAP (loop 0 [] ts)
            | LOOP ts -> LOOP (loop 0 [] ts)
            | LOOP_LEFT ts -> LOOP_LEFT (loop 0 [] ts)
            | PUSH (ty, c) -> PUSH (ty, constant c)

            | DUP | DIG _ | DUG _ | DROP _ | SWAP | PAIR | ASSERT | CAR | CDR
            | LEFT _ | RIGHT _ | APPLY | NIL _ | CONS | NONE _
            | SOME | COMPARE | EQ | LT | LE | GT | GE | NEQ
            | ADD | SUB | MUL | EDIV | ABS | ISNAT | NEG | LSL | LSR
            | AND | OR | XOR | NOT | EXEC | FAILWITH | UNIT
            | EMPTY_SET _ | EMPTY_MAP _ | EMPTY_BIG_MAP _
            | SIZE | MEM | UPDATE | CONCAT | SELF | GET
            | RENAME _ | PACK | UNPACK _ | SLICE | CAST
            | CONTRACT _ | CONTRACT' _ | TRANSFER_TOKENS | SET_DELEGATE | CREATE_ACCOUNT
            | CREATE_CONTRACT _ | IMPLICIT_ACCOUNT | NOW | AMOUNT | BALANCE
            | CHECK_SIGNATURE | BLAKE2B | SHA256 | SHA512 | HASH_KEY | STEPS_TO_QUOTA
            | SOURCE | SENDER | ADDRESS | CHAIN_ID
            | UNPAIR | SELF_ADDRESS | LEVEL -> t
          in
          t' :: loop 0 [] ts
    and constant =
      let open Constant in
      function
      | Code ops -> Code (loop 0 [] ops)
      | Option (Some t) -> Option (Some (constant t))
      | List ts -> List (List.map constant ts)
      | Set ts -> Set (List.map constant ts)
      | Map kvs -> Map (List.map (fun (k,v) -> (constant k, constant v)) kvs)
      | Pair (t1, t2) -> Pair (constant t1, constant t2)
      | Left t -> Left (constant t)
      | Right t -> Right (constant t)
      | (Unit | Bool _ | Int _ | String _ | Bytes _ | Timestamp _ | Option None as c) -> c
    in
    loop 0 [] ts

end

module Module = struct
  type t = { parameter : Type.t ; storage : Type.t ; code : Opcode.t list }

  let pp ?block_comment ppf { parameter ; storage ; code } =
    let open Mline in
    Format.fprintf ppf "%a ;@." Mline.pp & prim "parameter" [ Type.to_micheline parameter ] [];
    Format.fprintf ppf "%a ;@." Mline.pp & prim "storage" [ Type.to_micheline storage ] [];
    Format.fprintf ppf "%a ;@." Mline.pp & prim "code" [ Mline.seq (List.concat_map (Opcode.to_micheline ?block_comment) code) ] []
end
