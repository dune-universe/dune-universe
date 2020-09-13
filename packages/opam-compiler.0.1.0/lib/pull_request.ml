open! Import

type t = { user : string; repo : string; number : int }

let pp ppf { user; repo; number } =
  Format.fprintf ppf "{ user = %S; repo = %S; number = %d }" user repo number

let equal (x : t) y = x = y

let parse s =
  let word = Re.rep1 Re.wordc in
  let re_pr =
    Re.compile
      (Re.seq
         [
           Re.bos;
           Re.opt (Re.seq [ Re.group word; Re.char '/'; Re.group word ]);
           Re.char '#';
           Re.group (Re.rep1 Re.digit);
           Re.eos;
         ])
  in
  let open Let_syntax.Option in
  let+ g = Re.exec_opt re_pr s in
  let user, repo =
    (let+ user = re_group_get_opt g 1 and+ repo = re_group_get_opt g 2 in
     (user, repo))
    |> Option.value ~default:("ocaml", "ocaml")
  in
  let number = int_of_string (Re.Group.get g 3) in
  { user; repo; number }
