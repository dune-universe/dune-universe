(*
ocamlfind ocamlc -dsource -g -w A -warn-error +1..50-27 -g -annot -bin-annot -ppx ../ppx/ppx_monadic.opt -I . -c test_beginm.ml
*)

module Option = struct
  let bind e f = match e with
    | Some v -> f v
    | None -> None
  let return v = Some v
end

let t1 = 
  let open Option in
  begin %do
    x <-- return 1;
    return x
  end

let t2 = 
  begin %Option.do
    x <-- return 1;
    return x
  end

let t3 = [%Option.do
            x <-- return 1;
            return x
         ]
