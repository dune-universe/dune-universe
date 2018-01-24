
external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let parse stopat loc attrs s =
  let e = 
    match P_format.parse stopat loc s with
    | (`Const _ | `Fun _), Some rem ->
        let pos = String.length s - String.length rem in
        raise (Lexformat.Error (pos, pos + 1,
  		              Printf.sprintf "Unescaped special character %C found" rem.[0]))
    | `Const e, None -> 
        [%expr Qx.command [%e e] ]
    | `Fun (_abss, f), None -> 
        f [%expr
            Printf.ksprintf Qx.command
          ]
  in
  { e with pexp_attributes = attrs }

class command = object (self)

  inherit P_cformat.cformat as super

  method! expr e = 
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, Some "qx")) ->
        self#expr & parse [] e.pexp_loc e.pexp_attributes s
    | _ -> super#expr e
end
  
