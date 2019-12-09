open Base

type t =
  | True
  | False
  | Branch of { var : Var.t; hi : t; lo: t; id: int }

let id (t : t) : int =
  match t with
  | False -> -2
  | True -> -1
  | Branch { id; _ } -> id

let index (d:t) : int =
  match d with
  | True | False -> Var.leaf_idx
  | Branch { var; _ } -> Var.index var


module Triple = struct
  type t = Var.t * int * int
    [@@deriving hash, compare, sexp]
end

type manager = {
  mutable next_id : int;
  branch_cache : (Triple.t, t) Hashtbl.t;
}

let manager () = {
  next_id = 0;
  branch_cache = Hashtbl.create (module Triple);
}

let ctrue = True
let cfalse = False

let branch (mgr : manager) (var : Var.t) (hi : t) (lo : t) : t =
  let triple = (var, id hi, id lo) in
  Hashtbl.find_or_add mgr.branch_cache triple ~default:(fun () ->
    let id = mgr.next_id in
    mgr.next_id <- id + 1;
    Branch { var; hi; lo; id; }
  )
  
let equal (t1 : t) (t2 : t) : bool =
  match t1, t2 with
  | True, True | False, False ->
    true
  | Branch { id=id1; _ }, Branch { id=id2; _ } ->
    id1 = id2
  | _ ->
    false

let rec to_string ?(var_name=Var.to_string) (t : t) : string =
  match t with
  | True -> "1"
  | False -> "0"
  | Branch { var; hi; lo; _ } ->
    Caml.Format.sprintf "(%s ? %s : %s)"
      (var_name var)
      (to_string ~var_name hi)
      (to_string ~var_name lo)


let to_dot ?(var_name=Var.to_string) (t : t) : string =
  let open Caml.Format in
  let buf = Buffer.create 200 in
  let fmt = formatter_of_buffer buf in
  let seen : int Hash_set.t = Hash_set.create (module Int) in
  let at_rank : (t list) Map.M(Var).t ref  = ref (Map.empty (module Var)) in
  pp_set_margin fmt (1 lsl 29);
  fprintf fmt "digraph \"decision diagram\" {@\n";
  let rec loop t =
    if not (Hash_set.mem seen (id t)) then begin
      Hash_set.add seen (id t);
      match t with
      | False ->
        fprintf fmt "%d [shape=box label=\"⊥\"];@\n" (id t)
      | True ->
        fprintf fmt "%d [shape=box label=\"⊤\"];@\n" (id t)
      | Branch { var; lo; hi; id=id_t } ->
        at_rank := Map.add_multi (!at_rank) ~key:var ~data:t;
        fprintf fmt "%d [label=\"%s\"];@\n" id_t (var_name var);
        fprintf fmt "%d -> %d;@\n" id_t (id hi);
        fprintf fmt "%d -> %d [style=\"dashed\"];@\n" id_t (id lo);
        loop hi;
        loop lo;
    end
  in
  loop t;
  Map.iter (!at_rank) ~f:(fun ts ->
    fprintf fmt "{rank=same; ";
    List.iter ts ~f:(fun t -> fprintf fmt "%d " (id t));
    fprintf fmt ";}@\n");
  fprintf fmt "}@.";
  Buffer.contents buf

let compile_dot ?(format="pdf") ?(engine="dot") ?(title=engine) data : string =
  let output_file =
    Caml.Filename.(temp_file (title ^ "_") ("." ^ format))
    |> String.tr ~target:' ' ~replacement:'-'
  in
  let to_dot =
    Unix.open_process_out (Printf.sprintf "dot -T%s -o %s" format output_file)
  in
  Caml.output_string to_dot data;
  Caml.close_out to_dot;
  ignore ((Unix.close_process_out to_dot) : Unix.process_status);
  output_file

let render ?var_name ?(format="pdf") ?(title="Decision Diagram") t =
  ignore (to_dot ?var_name t
          |> compile_dot ~format ~title
          |> Open.in_default_app : bool)
  
