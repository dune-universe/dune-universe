open Printf

module IntSet = BatSet.Int

type t = { typ: string; (* atom's MOL2 type *)
           succs: IntSet.t } (* indexes of its direct successors (it is bonded to them) *)

let create typ succs =
  { typ; succs }

let dummy = create "" IntSet.empty

let add_succ (n: t) (succ: int): t =
  create n.typ (IntSet.add succ n.succs)

let to_string (n: t): string =
  sprintf "%s %s" n.typ (MyList.to_string string_of_int (IntSet.to_list n.succs))

let get_succs (n: t): IntSet.t =
  n.succs

let get_typ (n: t): string =
  n.typ
