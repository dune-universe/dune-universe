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

(* We can omit the types here, since they are coded in SCaml.ml *)
(* XXX We should consider to merge SCaml.ml and primitives.ml into one *)

module M = Michelson
open M.Opcode
open M.Type
open Tools
open Spotlib.Spot

let simple ~loc:(_ : Location.t) (os : Michelson.Opcode.t list) =
  fun (_ty : Michelson.Type.t) (pre : Michelson.Opcode.t list) -> pre @ os

let protocol pred f ~loc ty pre =
  let proto = Conf.get_protocol () in
  if pred proto then f ~loc ty pre
  else
    errorf_primitive ~loc "This primitive is not supported in the current protocol version %s" (Protocol.to_string proto)

let rec args ty = function
  | 0 -> []
  | n ->
      match ty.desc with
      | TyLambda (ty1, ty2) -> ty1 :: args ty2 (n-1)
      | _ -> assert false
let comparison ~loc os ty pre =
  match args ty 2 with
  | [ty1; _ty2] -> (* ty1 == ty2 *)
      if not & M.Type.is_comparable ty1 then
        errorf_primitive ~loc "Comparison operator takes a non comparable type %a"
          M.Type.pp ty1;
      pre @ os
  | _ -> assert false

let primitives =
  [ "fst"     , (true,  1, simple [CAR])
  ; "snd"     , (true,  1, simple [CDR])

  ; "compare" , (true,  2, comparison [COMPARE])
  ; "="       , (true,  2, comparison [COMPARE; EQ])
  ; "<>"      , (true,  2, comparison [COMPARE; NEQ])
  ; "<"       , (true,  2, comparison [COMPARE; LT])
  ; ">"       , (true,  2, comparison [COMPARE; GT])
  ; "<="      , (true,  2, comparison [COMPARE; LE])
  ; ">="      , (true,  2, comparison [COMPARE; GE])

  ; "+"       , (true,  2, simple [ADD])
  ; "+^"      , (true,  2, simple [ADD])
  ; "+$"      , (false, 2, simple [ADD])
  ; "-"       , (true,  2, simple [SUB])
  ; "-^"      , (true,  2, simple [SUB])
  ; "-$"      , (false, 2, simple [SUB])
  ; "*"       , (true,  2, simple [MUL])
  ; "*^"      , (true,  2, simple [MUL])
  ; "*$"      , (false, 2, simple [MUL])
  ; "lsl"     , (true,  2, simple [LSL])
  ; "lsr"     , (true,  2, simple [LSR])
  ; "&&"      , (true,  2, simple [AND])
  ; "||"      , (true,  2, simple [OR])
  ; "xor"     , (true,  2, simple [XOR])
  ; "not"     , (true,  1, simple [NOT])
  ; "abs"     , (true,  1, simple [ABS])
  ; "isnat"   , (true,  1, simple [ISNAT])
  ; "~-"      , (true,  1, simple [NEG])
  ; "~-^"     , (true,  1, simple [NEG])

  ; "lor"          , (true,  2, simple [OR])
  ; "land"         , (true,  2, simple [AND])
  ; "land_int_nat" , (true,  2, simple [AND])
  ; "lxor"         , (true,  2, simple [XOR])
  ; "lnot_nat"     , (true,  1, simple [NOT])
  ; "lnot"         , (true,  1, simple [NOT])

  ; "List.length"  , (true,  1, simple [SIZE])
  ; "List.map"     , (false, 2,
(* lambda : map : S              SWAP ;
   { hd; <tl> } : lambda : S     MAP {
     hd : lambda : S              DIP DUP
     hd : lambda : lambda : S     EXECx
     hd' : lambda : S

   {} : lambda : S               MAP {..}

   list' : lambda : S             DIP DROP
   list' : S
*)
             simple
             [ SWAP ;
               MAP (
                 [ DIP (1, [ DUP ]) ]
                 @ [ EXEC ]
               ) ;
               DIP (1, [ DROP 1 ])
             ])


  ; "List.fold_left"    , (false, 3,
(*
  lam : acc : list : s                  SWAP; DIP { SWAP } SWAP
  list : acc : lam : s                  ITER {
  hd : acc : lam : s                       DIP 2 { DUP } ; DUG 2
  acc : lam : hd : lam : s                 EXEC ;
  lam' : hd : lam : s                      SWAP ; EXEC ;
  acc' : lam : s                        }

  [] : acc : lam : s                    ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)
        simple
        [ SWAP ; DIP (1, [ SWAP ]); SWAP;
          ITER [ DIP (2, [ DUP ]); DUG 2;
                 EXEC;
                 SWAP; EXEC ];
          DIP (1, [ DROP 1])
        ])

  ; "List.fold_left'"    , (false, 3,
(*
  lam : acc : list : s                  SWAP; DIP { SWAP } SWAP
  list : acc : lam : s                  ITER {
  hd : acc : lam : s                       SWAP PAIR
  (acc, hd) : lam : s                      DIP { DUP }
  (acc, hd) : lam : lam : s                EXEC
  acc : lam : s                         }

  [] : acc : lam : s                    ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)
        simple
        [ SWAP ; DIP (1, [ SWAP ]); SWAP;
          ITER [ SWAP ; PAIR ; DIP (1, [ DUP ]); EXEC ];
          DIP (1, [ DROP 1])
        ])

  ; "List.rev", (true,  1, fun ~loc:_ ty xs ->
        match ty.desc with
        | TyLambda ({ desc= TyList ty }, { desc= TyList _ty' }) ->
            (* ty = _ty' *)
            xs @ [DIP (1, [NIL ty]); ITER [CONS]]
        | _ -> assert false)
  ; "List.rev_append", (true,  2, simple [ITER [CONS]])

  ; "Set.empty", (true,  0, fun ~loc:_ typ xs ->
        assert (xs = []);
        match typ.desc with
        | TySet ty -> [EMPTY_SET ty]
        | _ -> assert false)

  ; "Set.length"  , (true,  1, simple [SIZE])
  ; "Set.mem"     , (true,  2, simple [MEM])
  ; "Set.update"  , (true,  3, simple [UPDATE])

  ; "Set.fold"    , (false, 3,
(*
  lam : set : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  elt : acc : lam : s                   DIP { DIP { DUP } SWAP }
  elt : lam : acc : lam : s             EXECx
  lam2 : acc : lam : s                  SWAP
  acc : lam2 : lam : s                  EXECx
  acc : lam : s                         }

  set : acc : lam : s                   ITER {

  empty : acc : lam : s                 ITER {
  acc : lam : s                         DIP { DROP }
  acc : s
*)

        simple
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER ([ DIP (1, [ DIP (1, [ DUP ]); SWAP ]) ]
                @ [ EXEC ]
                @ [ SWAP ]
                @ [ EXEC ]);
          DIP (1, [ DROP 1 ])
        ])

  ; "Set.fold'"    , (false, 3,
(*
  lam : set : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  elt : acc : lam : s                     PAIR
  (elt, acc) : lam : s                    DIP DUP
  (elt, acc) : lam : lam : s              EXEC
  acc : lam : s                         }

  empty : acc : lam : s                 ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)

        simple
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER [ PAIR; DIP (1, [ DUP ]); EXEC ];
          DIP (1, [ DROP 1 ])
        ])

  ; "Loop.left"    , (false, 2, fun ~loc:_ typ xs ->
        let rty =
          match typ.desc with
          | TyLambda (_, { desc= TyLambda(_, rty) }) -> rty
          | _ ->
              Format.eprintf "Loop.left %a@." M.Type.pp typ;
              assert false
        in
(* lambda : acc : S                 SWAP ; LEFT ;
   Left acc : lambda : S            LOOP_LEFT {
   acc : lambda : S                   DUP DIP
   acc : lambda : lambda : S          EXECx
   LorR : lambda : S

   Left acc : lambda : S            LOOP_LEFT { ..

   Right res : lambda : S           LOOP_LEFT { .. }
   res : lambda : S                 DIP DROP

*)
        xs @
        [ SWAP ; LEFT rty;
          LOOP_LEFT (  DIP (1, [ DUP ]) :: [ EXEC ] );
          DIP (1, [ DROP 1 ]) ])

  ; "String.concat",   (true,  2, simple [CONCAT])
  ; "^",               (true,  2, simple [CONCAT])
  ; "String.length",   (true,  1, simple [SIZE])
  ; "Bytes.concat",    (true,  2, simple [CONCAT])
  ; "Bytes.length",    (true,  1, simple [SIZE])

  ; "Map.empty", (true,  0, fun ~loc:_ typ xs ->
        assert (xs = []);
        match typ.desc with
        | TyMap (ty1,ty2) -> [EMPTY_MAP (ty1, ty2)]
        | _ -> assert false)

  ; "Map.length", (true,  1, simple [SIZE])
  ; "Map.get", (true,  2, simple [ GET ] )
  ; "Map.mem", (true,  2, simple [MEM])
  ; "Map.update", (true,  3, simple [UPDATE])

  ; "Map.map", (false, 2,
(* lambda : map : S                 SWAP ;
   { (k,v); <tl> } : lambda : S     MAP {
     (k, v) : lambda : S              DIP DUP
     (k, v) : lambda : lambda : S     DUP CAR DIP { CDR ; SWAP }
     k : lambda : v : lambda : S      EXECx
     lambda' : v : lambda : S         SWAP EXECx
     w : : lambda : S

   { <tl> } : lambda : S            MAP { ..

   {} : lambda : S                  Map {

   map' : lambda : S                  DIP DROP
   map' : S

*)
        simple
        [ SWAP ;
          MAP (
            [ DIP (1, [ DUP ]);
              DUP; CAR; DIP (1, [ CDR; SWAP ]) ]
            @ [ EXEC ]
            @ [ SWAP ]
            @ [ EXEC ]
          ) ;
          DIP (1, [ DROP 1 ])
        ])

  ; "Map.map'", (false, 2,
(* lambda : map : S                 SWAP ;
   { (k,v); <tl> } : lambda : S     MAP {
     (k, v) : lambda : S              DIP DUP
     (k, v) : lambda : lambda : S     EXEC
     w : : lambda : S

   {} : lambda : S                  Map {..}

   map' : lambda : S                  DIP DROP
   map' : S

*)
        simple
        [ SWAP ;
          MAP [ DIP (1, [ DUP ]);
                EXEC ];
          DIP (1, [ DROP 1 ])
        ])

  ; "Map.fold"    , (false, 3,
(*
  lam : map : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  (k,v) : acc : lam : s                 DUP CAR DIP { CDR }
  k : v : acc : lam : s                 DIP { DIP { DIP { DUP } SWAP } SWAP
  k : lam : v : acc : lam : s           EXECx
  lam2 : v : acc : lam : s              SWAP
  v : lam2 : acc : lam : s              EXECx
  lam3 : acc : lam : s                  SWAP
  acc : lam3 : lam : s                  EXECx
  acc' : lam : s

  empty : acc : lam : s                 ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)

        simple
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER ([ DUP; CAR; DIP (1, [ CDR ]);
                  DIP (1, [ DIP (1, [ DIP (1, [ DUP ]); SWAP ]); SWAP ]) ]
                @ [ EXEC ]
                @ [ SWAP ]
                @ [ EXEC ]
                @ [ SWAP ]
                @ [ EXEC ]);
          DIP (1, [ DROP 1 ])
        ])


  ; "Map.fold'"    , (false, 3,
(*
  lam : map : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  (k,v) : acc : lam : s                   DUP CAR DIP { CDR }
  k : v : acc : lam : s                   DIP { PAIR } PAIR
  (k, (v, acc)) : lam : s                 DIP DUP
  (k, (v, acc)) : lam : lam : s           EXEC
  acc : lam : s                         }

  empty : acc : lam : s                 ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)

        simple
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER [ DUP; CAR; DIP (1, [ CDR ]);
                 DIP (1, [ PAIR ]); PAIR;
                 DIP (1, [ DUP ]);
                 EXEC ];
          DIP (1, [ DROP 1 ])
        ])


  (* big map *)

  ; "BigMap.empty", (true,  0, fun ~loc:_ typ xs ->
        assert (xs = []);
        match typ.desc with
        | TyBigMap (ty1,ty2) -> [EMPTY_BIG_MAP (ty1, ty2)]
        | _ -> assert false)
  ; "BigMap.get", (true,  2, simple [ GET ] )
  ; "BigMap.mem", (true,  2, simple [MEM])
  ; "BigMap.update", (true,  3, simple [UPDATE])

  ; "Obj.pack", (true,  1, fun ~loc ty pre ->
        match args ty 1 with
        | [ aty ] ->
            if not & M.Type.is_packable ~legacy:true aty then
              errorf_primitive ~loc "Obj.pack cannot take a non packable type %a"
                M.Type.pp aty;
            pre @ [ PACK ]
        | _ -> assert false)

  ; "Obj.unpack", (true,  1, fun ~loc ty xs ->
      match ty.desc with
      | TyLambda (_, { desc= TyOption (None, ty) }) ->
          if not & M.Type.is_packable ~legacy:false ty then
            errorf_primitive ~loc "Obj.unpack cannot unpack to a non packable type %a"
              M.Type.pp ty;
          xs @ [ UNPACK ty ]
      | _ -> assert false)

  ; "String.slice", (true,  3, simple [ SLICE ])
  ; "Bytes.slice", (true,  3, simple [ SLICE ]) (* XXX not tested *)

  ; "Contract.contract", (false, 1, fun ~loc:_ ty xs ->
        match ty.desc with
        | TyLambda (_, { desc= TyOption (None, { desc= TyContract ty }) }) ->
            xs @ [ CONTRACT ty ]
        | _ -> assert false)

  ; "Contract.contract'", (false, 2, fun ~loc:_ ty xs ->
        match ty.desc with
        | TyLambda (_, { desc= TyLambda (_, { desc= TyOption (None, { desc= TyContract ty })})})  ->
            begin match xs with
              | [ M.Opcode.PUSH (_, M.Constant.String entry); address] ->
                  [address; CONTRACT' (ty, entry) ]
              | _ ->
                  Format.eprintf "bug %a@." (Format.list "; " Michelson.Opcode.pp) xs;
                  assert false
            end
        | _ -> assert false)

  ; "Contract.implicit_account", (false, 1, simple [ IMPLICIT_ACCOUNT ])
  ; "Contract.address", (true,  1, simple [ ADDRESS ])
  ; "Contract.self",   (true,  0, simple [SELF])

  ; "Operation.transfer_tokens", (true,  3, simple [ TRANSFER_TOKENS ])
  ; "Operation.set_delegate", (true,  1, simple [ SET_DELEGATE ])

  ; "Global.get_now"            , (true,  1, simple [ DROP 1; NOW ])
  ; "Global.get_amount"         , (true,  1, simple [ DROP 1; AMOUNT ])
  ; "Global.get_balance"        , (true,  1, simple [ DROP 1; BALANCE ])
  ; "Global.get_source"         , (true,  1, simple [ DROP 1; SOURCE ])
  ; "Global.get_sender"         , (true,  1, simple [ DROP 1; SENDER ])
  ; "Global.get_steps_to_quota" , (true,  1, simple [ DROP 1; STEPS_TO_QUOTA ])
  ; "Global.get_chain_id"       , (true,  1, simple [ DROP 1; CHAIN_ID ])

  ; "Global.get_level"          , (true,  1, protocol (fun x -> x >= (8,0))
                                             @@ simple [ DROP 1; LEVEL ])

  ; "Crypto.check_signature", (true,  3, simple [ CHECK_SIGNATURE ])
  ; "Crypto.blake2b", (true,  1, simple [ BLAKE2B ])
  ; "Crypto.sha256", (true,  1, simple [ SHA256 ])
  ; "Crypto.sha512", (true,  1, simple [ SHA512 ])
  ; "Crypto.hash_key", (true,  1, simple [ HASH_KEY ])

  ; "Error.failwith", (false, 1, simple [ FAILWITH ]) (* deprecated *)
  ; "failwith", (false, 1, simple [ FAILWITH ])

  ; "raise", (false, 1, simple [ FAILWITH ])

  ; "Timestamp.add",  (true,  2, simple [ADD])
  ; "Timestamp.sub",  (true,  2, simple [SUB])
  ; "Timestamp.diff", (true,  2, simple [SUB])

  ; "ediv_int_int", (true,  2, simple [EDIV])
  ; "ediv_int_nat", (true,  2, simple [EDIV])
  ; "ediv_nat_int", (true,  2, simple [EDIV])
  ; "ediv_nat_nat", (true,  2, simple [EDIV])
  ; "ediv_tz_tz", (true,  2, simple [EDIV])
  ; "ediv_tz_nat", (true,  2, simple [EDIV])

  ; "/", (false, 2, simple [EDIV; IF_NONE( [PUSH (tyInt, Int Z.zero); FAILWITH],
                                    [CAR])])
  ; "/^", (false, 2, simple [EDIV; IF_NONE( [PUSH (tyNat, Int Z.zero); FAILWITH],
                                     [CAR])])
  ; "/$", (false, 2, simple [EDIV; IF_NONE( [PUSH (tyMutez, Int Z.zero); FAILWITH],
                                     [CAR])])
  ; "/$^", (false, 2, simple [EDIV; IF_NONE( [PUSH (tyNat, Int Z.zero); FAILWITH],
                                      [CAR])])

  ; "Option.value", (true,  2, simple [IF_NONE ([], [DIP (1, [DROP 1])])])
  ; "Option.get", (false, 1, simple [IF_NONE ([PUSH (tyString, String "Option.get"); FAILWITH], [])])

  ; "Sum.get_left", (false, 1, simple [IF_LEFT ([], [PUSH (tyString, String "Sum.get-left"); FAILWITH])])
  ; "Sum.get_right", (false, 1, simple [IF_LEFT ([PUSH (tyString, String "Sum.get-left"); FAILWITH], [])])
  ]

let contract' entry ~loc:_ ty xs =
  match ty.desc with
  | TyLambda (_, { desc= TyLambda (_, { desc= TyOption (None, { desc= TyContract ty })})})  ->
      xs @ [ CONTRACT' (ty, entry) ]
  | _ -> assert false
