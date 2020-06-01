open Base

module Hlist = Hlist
include Record_builder_intf

module Make_internal(F : Partial_applicative_S2) = struct
  (** An internal type which is a special case of [F] constructing an [Hlist]. *)
  module Hlist_F = struct
    type ('a, 'e) t =
      | Nil : (Hlist.nil, 'e) t
      | Cons : (('x, 'xs) Hlist.cons Hlist.t, 'e) F.t -> (('x, 'xs) Hlist.cons, 'e) t

    let cons (type a b)
          (left : (a, _) F.t) (right : (b, _) t)
      : ((a, b) Hlist.cons, _) t
      = match right with
      | Nil -> Cons (F.map left ~f:(fun x -> Hlist.cons x Hlist.empty))
      | Cons right -> Cons (F.both left right)
    ;;

    let unpack ((Cons x) : ((_, _) Hlist.cons, _) t) = x
  end

  module Make_creator_types = struct
    type ('out, 'all_fields, 'extra) accum =
      (('out, 'extra) Hlist_F.t -> ('all_fields, 'extra) Hlist_F.t)
      * ('all_fields, 'out) Hlist.Suffix_index.t

    type ('field, 'head, 'tail, 'all_fields, 'extra) fold_step =
      ('head, 'all_fields, 'extra) accum
      -> ('all_fields -> 'field) * ('tail, 'all_fields, 'extra) accum

    type ('field, 'tail, 'all_fields, 'extra) handle_one_field =
      ('field, ('field, 'tail) Hlist.cons, 'tail, 'all_fields Hlist.nonempty, 'extra) fold_step

    type ('record, 'all_fields, 'extra) handle_all_fields =
      ('record, 'all_fields Hlist.nonempty, Hlist.nil, 'all_fields Hlist.nonempty, 'extra) fold_step
  end

  let field applicative _field (build_hlist, suffix) =
    let build_hlist tail =
      build_hlist (Hlist_F.cons applicative tail)
    and get_field =
      let index = Hlist.Element_index.(within ~suffix first_element) in
      fun hlist -> Hlist.nth hlist index
    and suffix = Hlist.Suffix_index.tail_of suffix
    in
    get_field, (build_hlist, suffix)
  ;;

  let build_for_record folding =
    let from_values, (build_up, _) =
      folding (Fn.id, Hlist.Suffix_index.whole_list)
    in
    let built = Hlist_F.(unpack (build_up Nil)) in
    F.map built ~f:from_values
  ;;
end

module Make(F : Partial_applicative_S) = struct
  type 'a applicative = 'a F.t

  include Make_internal(struct
      type ('a, 'e) t = 'a applicative
      let map = F.map
      let both = F.both
    end)
end

module Make_2(F : Partial_applicative_S2) = struct
  type ('a, 'e) applicative = ('a, 'e) F.t

  include Make_internal(F)
end
