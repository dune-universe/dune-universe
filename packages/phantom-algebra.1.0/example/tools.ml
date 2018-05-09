open Phantom_algebra.Core

let epsilon = ref 1e-12
let set_epsilon x = epsilon := x
let negligible x = x < !epsilon


let failure = ref false
let test x expected name =(*
  let line ppf =
    Format.fprintf ppf
      "——————————————————————————————————————————————————————————————————————@,"
  in*)
  let x, out = clone_2 x in
  begin match expected with
  | None ->
    Format.printf
      "@[<hv>  \x1b[94m%s:\x1b[97m@ @;<0 8>@[%a@]@,@]@." name pp x
  | Some y -> if negligible @@ norm(x <-> y) then
      Format.printf "@[  \x1b[94m%s\x1b[97m: \x1b[32m[✔]\x1b[97m@]@." name
    else begin
      failure := true;
      Format.printf
        "@[<hv 8>  \x1b[94m%s\x1b[97m:\x1b[31m[✘]\x1b[97m@ @ \
         got:@,%a@,    ≠    @,expected:@,%a@,@]@."
        name pp x pp y end
  end
  ; out

let ( =? ) x expected name = test x (Some expected) name
let (:=) x f = f x
let ( |? ) x name = test x None name
