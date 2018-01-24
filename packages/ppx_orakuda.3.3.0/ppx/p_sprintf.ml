open Ppxx.Utils

let parse stopat loc _attr (* CR TODO *) s =
  match P_format.parse stopat loc s with
  | (`Const _ | `Fun _), Some rem ->
      let pos = String.length s - String.length rem in
      raise (Lexformat.Error (pos, pos + 1,
		              Printf.sprintf "Unescaped special character %C found" rem.[0]))
  | `Const c, _ -> c
  | `Fun (_abss, f), _ -> f [%expr Printf.sprintf]

class sprintf = object (self)

  inherit P_regexp.regexp as super

  method! expr e = 
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, Some "qq")) ->
        self#expr & parse [] e.pexp_loc e.pexp_attributes s
    | _ -> super#expr e
end
