(** Top-level loop for the chase *)

(** The chase computes a tree of structures.  Each structure is stored
   in a frame.  A frame contains the information required to recreate
   the tree.  Each frame is identified by a label.  Labels are
   integers that are allocated sequentially starting from zero.  When
   the frame is not at the root of the tree, a frame records its
   parent and the formula that was used to create the stucture in the
   frame. *)

(** Status of a frame *)
type status = Sat | Unsat | Aborted

type frame = {
    label : int;
    parent : int option;
    cause: int option;          (** Formula that created this frame *)
    mutable status : status;
    rules : Formula.form list;
    structure : Structure.structure;
  }

(** [solve just_one input_order bound limit pr axioms] runs the chase
   for the theory [axioms].  It prints results with [pr].  When
   [just_one] is true, [solve] halts when it finds one model,
   otherwise, [solve] finds a set of support.  When [input_order] is
   true, the formulas are tried in the order they are given in the
   input source file, otherwise, the order is dynamically changed to
   prevent starvation of some rules.  The size bound it uses is
   [bound] and the step limit it uses is [limit]. *)
val solve : bool -> bool -> int -> int -> (frame -> unit) ->
            Formula.axioms -> unit
