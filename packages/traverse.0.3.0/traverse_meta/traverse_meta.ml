let variable_count = 32

let mklid s =
  Metapp.mkloc (Longident.Lident s)

let rec compose_n n f unit =
  if n > 0 then
    let i = pred n in
    compose_n i f (f i unit)
  else
    unit

let compose f unit =
  compose_n variable_count f unit

let type_of_string s =
  Ppxlib.Ast_helper.Typ.constr (mklid s) []

let mk_t f =
  Ppxlib.Ast_helper.Typ.constr (mklid "t")
    [compose (fun i acc ->
      let ti, ti_t = f i in
      Ppxlib.Ast_helper.Typ.tuple [ti; ti_t; acc])
      (type_of_string "unit")]

let newtypes f expr =
  compose (fun i acc ->
    Metapp.Exp.newtype (Metapp.mkloc (f i)) acc) expr

let ti i = Printf.sprintf "t%d" i

let ti_t i = Printf.sprintf "t%d_t" i

let eqi i = Printf.sprintf "eq%d" i
