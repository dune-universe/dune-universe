(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)
open Fmlib
open Container
open Term




module Result_type: sig

  type t
  val empty:        t
  val normal:       t -> t
  val make_func:    type_term -> t
  val make_ghost:   type_term -> t
  val make_proc:    type_term -> t
  val make:         type_term -> bool -> bool -> t
  val has_result:   t -> bool
  val result:       t -> type_term
  val is_procedure: t -> bool
  val is_ghost:     t -> bool
  val to_ghost:     t -> t
  val up_from:      int -> int -> t -> t
  val up:           int -> t -> t
  val map:    (type_term->type_term) -> t -> t
  val fold:   ('a->type_term->'a) -> 'a -> t -> 'a
end = struct

  type t = (type_term * bool * bool) option
  let empty = None
  let make_func (tp:type_term): t = Some (tp,false,false)
  let make_ghost(tp:type_term): t = Some (tp,false,true)
  let make_proc (tp:type_term): t = Some (tp,true,false)
  let make (tp:type_term) (proc:bool) (ghost:bool): t = Some (tp,proc,ghost)

  let normal (rt:t): t =
    match rt with
      None -> rt
    | Some(tp,_,_) -> Some(tp,false,false)

  let has_result (rt:t): bool = Option.has rt

  let result(rt:t): type_term =
    assert (has_result rt);
    match rt with
      None -> assert false
    | Some (tp,_,_) -> tp

  let  is_procedure (rt:t): bool =
    match rt with
      None            -> true
    | Some (_,proc,_) -> proc

  let is_ghost (rt:t): bool =
    match rt with
      None             -> false
    | Some (_,_,ghost) -> ghost

  let to_ghost (rt:t): t =
    assert (has_result rt);
    match rt with
      None -> assert false
    | Some (tp,proc,ghost) -> Some (tp,proc,true)


  let map (f:type_term->type_term) (rt:t): t =
    match rt with
      None -> None
    | Some (tp,proc,ghost) ->
        Some(f tp, proc, ghost)

  let fold (f:'a->type_term->'a) (a:'a) (rt:t): 'a =
    match rt with
      None -> a
    | Some (t,_,_) ->
        f a t

  let up_from (n:int) (start:int) (rt:t): t =
    map (fun tp -> Term.up_from n start tp) rt

  let up (n:int) (rt:t): t = up_from n 0 rt

end (* Result_type *)





module Sign: sig
  type t
  val empty:       t
  val make:        type_term array -> Result_type.t -> t
  val make_func:   type_term array -> type_term -> t
  val make_ghost:  type_term array -> type_term -> t
  val make_proc:   type_term array -> type_term -> t
  val make_const:  type_term -> t
  val make_args:   type_term array -> t
  val normal:      t -> t
  val to_string:   t -> string
  val arity:       t -> int
  val is_constant: t -> bool
  val arguments:   t -> type_term array
  val arg_type:    int -> t -> type_term
  val argument:    int -> t -> t
  val result_type: t -> Result_type.t
  val has_result:  t -> bool
  val is_binary:   t -> bool
  val is_unary:    t -> bool
  val result:      t -> type_term
  val is_procedure:t -> bool
  val is_ghost:    t -> bool
  val to_ghost:    t -> t
  val map:   (type_term->type_term) -> t -> t
  val fold:        ('a -> type_term -> 'a) -> 'a -> t -> 'a
  val up_from:     int -> int -> t -> t
  val up:          int -> t -> t
  val up2:         int -> int -> int -> t -> t
  val down_from:   int -> int -> t -> t
  val to_function: int -> t -> t
  val involved_classes_arguments: Tvars.t -> t -> IntSet.t
  val involved_classes: Tvars.t -> t -> IntSet.t
  val anchor: Tvars.t -> t -> int * int
end = struct

  type t = {args: type_term array;
            rt:   Result_type.t}

  let empty: t = {args = [||]; rt = Result_type.empty (*result = None*)}

  let make (args: type_term array) (rt:Result_type.t): t =
    {args = args; rt = rt}

  let make_func (args: type_term array) (result:type_term): t =
    {args = args; rt = Result_type.make_func result}

  let make_ghost (args: type_term array) (result:type_term): t =
    {args = args; rt = Result_type.make_ghost result}

  let make_args (args: type_term array): t =
    {args = args; rt = Result_type.empty}

  let make_const (result:type_term): t =
    {args = [||]; rt = Result_type.make_func result}

  let make_proc (args: type_term array) (result:type_term): t =
    {args = args; rt = Result_type.make_proc result}

  let normal (s:t): t = make s.args (Result_type.normal s.rt)

  let arity (s:t): int = Array.length s.args

  let is_constant (s:t): bool = (arity s) = 0

  let arguments (s:t): type_term array = s.args

  let arg_type (i:int) (s:t): type_term =
    assert (i < (arity s));
    s.args.(i)

  let argument (i:int) (s:t): t =
    assert (i < (arity s));
    make_func [||] s.args.(i)

  let result_type (s:t): Result_type.t = s.rt

  let has_result (s:t): bool = Result_type.has_result s.rt

  let is_binary (s:t): bool = (has_result s) && ((arity s) = 2)
  let is_unary  (s:t): bool = (has_result s) && ((arity s) = 1)

  let result (s:t): type_term =
    assert (has_result s);
    Result_type.result s.rt

  let is_procedure (s:t): bool = Result_type.is_procedure s.rt

  let is_ghost     (s:t): bool = Result_type.is_ghost     s.rt

  let to_ghost (s:t): t =
    assert (has_result s);
    {s with rt = Result_type.to_ghost s.rt}

  let to_string (s:t): string =
    let argsstr =
      if (arity s) = 0 then ""
      else
        "("
        ^ (String.concat
             ","
             (List.map Term.to_string (Array.to_list s.args)))
        ^ ")"
        ^ (if has_result s then ":" else "")
    and retstr =
      if has_result s then Term.to_string (result s)
      else ""
    in
    argsstr ^ retstr

  let map (f:type_term->type_term) (s:t): t =
    let args = Array.map f s.args
    and rt   = Result_type.map f s.rt in
    make args rt

  let fold (f:'a->type_term->'a) (a:'a) (s:t): 'a =
    let a = Array.fold_left f a s.args in
    Result_type.fold f a s.rt

  let up_from (n:int) (start:int) (s:t): t =
    (** Shift all types up by [n] starting from [start].
     *)
    map (fun t -> Term.up_from n start t) s


  let up (n:int) (s:t): t =
    (** Shift all types up by [n].
     *)
    up_from n 0 s


  let up2 (n1:int) (start:int) (n2:int) (s:t): t =
    (** Shift all types up by [n1] starting from type [start] and then
        shift all types up by [n2] i.e. the operation creates a hole
        of [n1] starting from [start+n2] and a hole of [n2] starting from
        0.
     *)
    up n2 (up_from n1 start s)



  let down_from (n:int) (start:int) (s:t): t =
    (* Shift all types above [start] down by [n].
     *)
    map (fun t -> Term.down_from n start t) s


  let to_function (nargs:int) (s:t): t =
    (** Convert the constant signature [s] into a function signature with
        [nargs] arguments. The [nargs] argument types are fresh type variables.
     *)
    assert (has_result s);
    assert (is_constant s);
    {s with args   = Array.init nargs (fun i -> Variable i)}


  let involved_classes_arguments (tvs:Tvars.t) (s:t): IntSet.t =
    Array.fold_left
      (fun set tp ->
        Tvars.add_involved_classes tp tvs set)
      IntSet.empty
      s.args


  let involved_classes (tvs:Tvars.t) (s:t): IntSet.t =
    fold (fun set tp -> Tvars.add_involved_classes tp tvs set) IntSet.empty s


  let anchor (tvs:Tvars.t) (s:t): int * int =
    (* The anchor class of the signature [s] *)
    let nlocs = Tvars.count_local tvs
    and nall  = Tvars.count_all tvs
    in
    let used =
      fold
        (fun set t ->
          match t with
            Variable i when nlocs <= i && i < nall ->
              IntSet.add i set
          | _ ->
              set)
        IntSet.empty
        s
    in
    if IntSet.cardinal used <> 1 then
      -1, -1
    else
      let tv = IntSet.min_elt used
      and involved =
        fold
          (fun set t ->
            (Term.fold
               (fun set i ->
                 if i < nlocs then
                   assert false
                 else if nall <= i then
                   set
                 else
                   IntSet.add i set)
               set
               t))
          IntSet.empty
          s
      in
      if IntSet.cardinal involved <> 1 || IntSet.min_elt involved <> tv then
        -1, -1
      else begin
        assert (nlocs <= tv);
        assert (tv < nall);
        tv, Tvars.principal_class (Variable tv) tvs
      end
end (* Sign *)
