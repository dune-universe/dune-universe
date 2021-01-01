
module MyIdent = struct 
  type t = GT.string [@@deriving gt ~options:{ fmt }]
end 
(* type logic_op = Conj | Disj [@@deriving gt ~options:{ fmt }]
type op = | Plus | Minus | LT | LE | GT | GE | Eq [@@deriving gt ~options:{ fmt }] *)

type api = (MyIdent.t * term) GT.list
and term = LI of heap GT.option * MyIdent.t
          | CInt of GT.int
          | BinOp of term * term
          | Unit
          | Call of term * term
          | Union of (pf * term) GT.list
          | Lambda of { lam_argname: MyIdent.t GT.option
                      ; lam_api   : api
                      ; lam_eff   : heap
                      ; lam_body  : term
                      ; lam_is_rec: GT.bool
                      }

(* TODO: it should be path here *)
and t = HAssoc of (MyIdent.t * term) GT.list
      | HMerge of (pf * t) GT.list
      | HCmps of heap * heap
      | HCall of term * term
      | HEmpty

and pf  = LogicBinOp of pf * pf
        | Not of pf
        | EQident of MyIdent.t * MyIdent.t
        | PFTrue
        | PFFalse
        | Term of term
and heap = t [@@deriving gt ~options:{ fmt }]

let api1 = 
  [ ("gl", Lambda { lam_api=[]; lam_argname=None; lam_eff=HEmpty; lam_body=(CInt 5); lam_is_rec=true })
  ; ("gl", Lambda { lam_api=[]; lam_argname=None; lam_eff=HEmpty; lam_body=(CInt 5); lam_is_rec=true })
  ; ("gl", Lambda { lam_api=[]; lam_argname=None; lam_eff=HEmpty; lam_body=(CInt 5); lam_is_rec=true })
  ; ("gl", Lambda { lam_api=[]; lam_argname=None; lam_eff=HEmpty; lam_body=(CInt 5); lam_is_rec=true })
  ]

let () = 
  (GT.fmt api) Format.std_formatter api1


