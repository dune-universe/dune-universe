module Atomic = struct
  module Arity = struct
    type ('a, 'a_t, 'result, 'is_empty) t =
      | O : ('a, 'a_t, 'a_t, [`Empty]) t
      | S : ('a, 'a_t, 'result, _) t ->
          ('a, 'a_t, 'a -> 'result, [`Not_empty]) t
  end

  module Make (Applicative: Modules.Applicative.S) = struct
    let rec traverse_aux : type a result is_empty .
      a -> (a, a Applicative.t, result, is_empty) Arity.t -> result =
    fun reference_value arity ->
      match arity with
      | Arity.O -> Applicative.pure reference_value
      | Arity.S arity ->
          function value ->
            if value = reference_value then
              traverse_aux reference_value arity
            else
              raise Modules.StructuralMismatch

    let traverse (arity : ('a, 'a_t, 'result, [`Not_empty]) Arity.t) value =
      let Arity.S arity = arity in
      traverse_aux value arity
  end
end

module type UnaryTypeS = sig
  type 'a t
end

module UnaryArity (T : UnaryTypeS) = struct
  type ('a, 'a_t, 'f, 'result, 'is_empty) t =
    | O : ('a, 'a_t, 'a, 'a_t, [`Empty]) t
    | S : ('a, 'a_t, 'f, 'result, _) t ->
        ('a, 'a_t, 'x -> 'f, 'x T.t -> 'result, [`Not_empty]) t
end

module Lazy = struct
  module Arity = UnaryArity (Lazy)

  module Make (Applicative: Modules.Applicative.S) = struct
    let rec traverse : type a f result is_empty .
      (a Applicative.t, a Lazy.t Applicative.t, f, result, is_empty) Arity.t ->
        f -> result =
    fun arity f ->
      match arity with
      | Arity.O -> Applicative.map Lazy.from_val f
      | Arity.S arity ->
          function (lazy x) ->
            traverse arity (f x)
  end
end

module List = struct
  module Arity = UnaryArity (List)

  module Make (Applicative: Modules.Applicative.S) = struct
    let rec traverse_nil : type a f result is_empty .
      (a Applicative.t, a List.t Applicative.t, f, result, is_empty) Arity.t ->
        result =
    fun arity ->
      match arity with
      | Arity.O -> Applicative.pure []
      | Arity.S arity ->
          function
            | [] -> traverse_nil arity
            | _ :: _ -> raise Modules.StructuralMismatch

    let rec traverse_cons : type a f result is_empty .
      (a Applicative.t, a List.t Applicative.t, f, result, is_empty) Arity.t ->
        f -> (unit -> result) -> result =
    fun arity traverse_'a traverse_tl ->
      match arity with
      | Arity.O ->
          Applicative.apply (Applicative.map List.cons traverse_'a) traverse_tl
      | Arity.S arity ->
          function
            | [] -> raise Modules.StructuralMismatch
            | hd :: tl ->
                traverse_cons arity (traverse_'a hd)
                  (fun () -> traverse_tl () tl)

    let rec traverse : type a f result .
          (a Applicative.t, a List.t Applicative.t, f, result, [`Not_empty])
          Arity.t -> f -> result =
    fun arity traverse_'a ->
      let Arity.S arity' = arity in
      function
      | [] -> traverse_nil arity'
      | hd :: tl ->
          traverse_cons arity' (traverse_'a hd)
            (fun () -> traverse arity traverse_'a tl)
  end
end

module Array = struct
  module Arity = UnaryArity (Array)

  module Make (Applicative: Modules.Applicative.S) = struct
    module List' = List.Make (Applicative)

    type ('a, 'is_empty, 'f, 'array_result) traverse_aux =
      | Traverse_aux : {
          list_arity :
            ('a Applicative.t, 'a Stdlib.List.t Applicative.t, 'f, 'list_result,
              'is_empty) List.Arity.t;
          apply_arguments : ('list_result -> 'array_result) } ->
            ('a, 'is_empty, 'f, 'array_result) traverse_aux

    let rec traverse_aux : type a f array_result is_empty .
      (a Applicative.t, a Array.t Applicative.t, f, array_result,
        is_empty) Arity.t ->
      (a, is_empty, f, array_result) traverse_aux =
    fun arity ->
      match arity with
      | Arity.O ->
          Traverse_aux {
            list_arity = O;
            apply_arguments = fun f -> Applicative.map Array.of_list f;
          }
      | Arity.S arity ->
          let Traverse_aux { list_arity; apply_arguments } =
            traverse_aux arity in
          Traverse_aux {
            list_arity = S list_arity;
            apply_arguments =
              fun f array -> apply_arguments (f (Array.to_list array)) }

    let traverse (arity : ('a, 'a_t, 'f, 'result, [`Not_empty]) Arity.t)
        traverse_'a : 'result =
      let Traverse_aux { list_arity; apply_arguments } = traverse_aux arity in
      apply_arguments (List'.traverse list_arity traverse_'a)
  end
end

module Option = struct
  module Arity = UnaryArity (Option)

  module Make (Applicative: Modules.Applicative.S) = struct
    let rec traverse_none : type a f result is_empty .
      (a Applicative.t, a Option.t Applicative.t, f, result, is_empty)
        Arity.t -> result =
    fun arity ->
      match arity with
      | Arity.O -> Applicative.pure None
      | Arity.S arity ->
          function
            | None -> traverse_none arity
            | Some _ -> raise Modules.StructuralMismatch

    let rec traverse_some : type a f result is_empty .
      (a Applicative.t, a Option.t Applicative.t, f, result, is_empty)
        Arity.t -> f -> result =
    fun arity traverse_'a ->
      match arity with
      | Arity.O -> Applicative.map Option.some traverse_'a
      | Arity.S arity ->
          function
            | None -> raise Modules.StructuralMismatch
            | Some x -> traverse_some arity (traverse_'a x)

    let rec traverse : type a f result .
          (a Applicative.t, a Option.t Applicative.t, f, result, [`Not_empty])
            Arity.t -> f -> result =
    fun arity traverse_'a ->
      let Arity.S arity' = arity in
      function
      | None -> traverse_none arity'
      | Some x -> traverse_some arity' (traverse_'a x)
  end
end

module Ref (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  module Arity = UnaryArity (struct type 'a t = 'a ref end)

  module Make (Applicative: Modules.Applicative.S) = struct
    let rec traverse : type a f result is_empty .
      (a Applicative.t, a ref Applicative.t, f, result, is_empty) Arity.t ->
        f -> result =
    fun arity f ->
      match arity with
      | Arity.O -> Applicative.map ref f
      | Arity.S arity ->
          function x ->
            traverse arity (f !x)
  end
end

module type BinaryTypeS = sig
  type ('a, 'b) t
end

module BinaryArity (T : BinaryTypeS) = struct
  type ('a, 'b, 'ab_t, 'f, 'g, 'result, 'is_empty) t =
    | O : ('a, 'b, 'ab_t, 'a, 'b, 'ab_t, [`Empty]) t
    | S : ('a, 'b, 'ab_t, 'f, 'g, 'result, _) t ->
        ('a, 'b, 'ab_t, 'x -> 'f, 'y -> 'g, ('x, 'y) T.t -> 'result,
          [`Not_empty]) t
end

module Result (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  module Arity = BinaryArity (Result)

  module Make (Applicative: Modules.Applicative.S) = struct
    let rec traverse_ok : type a b f g result is_empty .
      (a Applicative.t, b Applicative.t, (a, b) Result.t Applicative.t, f, g,
        result, is_empty) Arity.t -> f -> result =
    fun arity traverse_'a ->
      match arity with
      | Arity.O -> Applicative.map Result.ok traverse_'a
      | Arity.S arity ->
          function
            | Ok x -> traverse_ok arity (traverse_'a x)
            | Error _ -> raise Modules.StructuralMismatch

    let rec traverse_error : type a b f g result is_empty .
      (a Applicative.t, b Applicative.t, (a, b) Result.t Applicative.t, f, g,
        result, is_empty) Arity.t -> g -> result =
    fun arity traverse_'b ->
      match arity with
      | Arity.O -> Applicative.map Result.error traverse_'b
      | Arity.S arity ->
          function
            | Ok _ -> raise Modules.StructuralMismatch
            | Error x -> traverse_error arity (traverse_'b x)

    let rec traverse : type a b f g result .
      (a Applicative.t, b Applicative.t, (a, b) Result.t Applicative.t, f, g,
        result, [`Not_empty]) Arity.t -> f -> g -> result =
    fun arity traverse_'a traverse_'b ->
      let Arity.S arity' = arity in
      function
      | Ok x -> traverse_ok arity' (traverse_'a x)
      | Error x -> traverse_error arity' (traverse_'b x)
  end
end

module Seq = struct
  module Arity = UnaryArity (Seq)

  module Make (Applicative: Modules.Applicative.S) = struct
    let rec traverse_nil : type a f result is_empty .
      (a Applicative.t, a Seq.t Applicative.t, f, result, is_empty) Arity.t ->
        result =
    fun arity ->
      match arity with
      | Arity.O -> Applicative.pure Seq.empty
      | Arity.S arity ->
          function s ->
            match s () with
            | Nil -> traverse_nil arity
            | Cons _ -> raise Modules.StructuralMismatch

    let rec traverse_cons : type a f result is_empty .
      (a Applicative.t, a Seq.t Applicative.t, f, result, is_empty) Arity.t ->
        f -> (unit -> result) -> result =
    fun arity traverse_'a traverse_tl ->
      match arity with
      | Arity.O ->
          Applicative.apply (Applicative.map Seq.cons traverse_'a)
            traverse_tl
      | Arity.S arity ->
          function s ->
            match s () with
            | Nil -> raise Modules.StructuralMismatch
            | Cons (hd, tl) ->
                traverse_cons arity (traverse_'a hd)
                  (fun () -> traverse_tl () tl)

    let rec traverse : type a f result .
      (a Applicative.t, a Seq.t Applicative.t, f, result, [`Not_empty])
        Arity.t -> f -> result =
    fun arity traverse_'a ->
      let Arity.S arity' = arity in
      function s ->
        match s () with
        | Nil -> traverse_nil arity'
        | Cons (hd, tl) ->
            traverse_cons arity' (traverse_'a hd)
              (fun () -> traverse arity traverse_'a tl)
  end
end

module Classes (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let traverse_atomic reference_value =
    Arity.Pred.destruct Zero
      (fun value ->
        if reference_value = value then
          []
        else
          raise Modules.StructuralMismatch)
      (let open Arity.Pred.ArrowSequence in
        function [] -> Applicative.pure reference_value)

  let rec traverse_list : 'a . ('a, 'a Applicative.t) Arity.t ->
    ('a list, 'a list Applicative.t) Arity.t = fun visit_'a l ->
    match l with
    | [] ->
        Arity.Pred.destruct Zero
          (function
            | [] -> []
            | _ :: _ -> raise Modules.StructuralMismatch)
          (let open Arity.Pred.ArrowSequence in
          function [] -> Applicative.pure (let open Stdlib.List in []))
    | hd :: tl ->
        Arity.Pred.destruct (Succ (Succ Zero))
          (function
            | [] -> raise Modules.StructuralMismatch
            | hd :: tl -> [hd; tl])
          (let open Arity.Pred.ArrowSequence in
          function [hd'; tl'] ->
            Applicative.apply
              (Applicative.map Stdlib.List.cons (hd' (visit_'a hd)))
              (fun () -> (tl' (traverse_list visit_'a tl))))

  class virtual ['self] traverse =
    object (self : 'self)
      method visit_array :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a Stdlib.Array.t, 'a Stdlib.Array.t Applicative.t) Arity.t =
      fun traverse_'a ->
        Arity.destruct (Succ Zero)
          (fun a -> [Stdlib.Array.to_list a])
          (let open Arity.ArrowSequence in
            function [fx] ->
              Applicative.map Stdlib.Array.of_list
                (fx (traverse_list traverse_'a)))

      method visit_bool : (bool, bool Applicative.t) Arity.t =
        traverse_atomic

      method visit_bytes : (bytes, bytes Applicative.t) Arity.t =
        traverse_atomic

      method visit_char : (char, char Applicative.t) Arity.t =
        traverse_atomic

      method visit_float : (float, float Applicative.t) Arity.t =
        traverse_atomic

      method visit_int : (int, int Applicative.t) Arity.t =
        traverse_atomic

      method visit_int32 : (int32, int32 Applicative.t) Arity.t =
        traverse_atomic

      method visit_int64 : (int64, int64 Applicative.t) Arity.t =
        traverse_atomic

      method visit_lazy_t :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a Stdlib.Lazy.t, 'a Stdlib.Lazy.t Applicative.t) Arity.t =
      fun traverse_'a ->
        Arity.destruct (Succ Zero)
          (fun (lazy value) -> [value])
          (let open Arity.ArrowSequence in
            function [fx] ->
              Applicative.map Stdlib.Lazy.from_val (fx traverse_'a))

      method visit_list :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a list, 'a list Applicative.t) Arity.t = fun visit_'a l ->
        match l with
        | [] ->
            Arity.Pred.destruct Zero
              (function
                | [] -> []
                | _ :: _ -> raise Modules.StructuralMismatch)
              (let open Arity.Pred.ArrowSequence in
                function [] -> Applicative.pure (let open Stdlib.List in []))
        | hd :: tl ->
            Arity.Pred.destruct (Succ (Succ Zero))
              (function
                | [] -> raise Modules.StructuralMismatch
                | hd :: tl -> [hd; tl])
              (let open Arity.Pred.ArrowSequence in
                function [hd'; tl'] ->
                  Applicative.apply
                    (Applicative.map Stdlib.List.cons (hd' (visit_'a hd)))
                    (fun () -> (tl' (self#visit_list visit_'a tl))))

      method visit_nativeint : (nativeint, nativeint Applicative.t) Arity.t =
        traverse_atomic

      method visit_option :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a option, 'a option Applicative.t) Arity.t =
      fun traverse_'a o ->
        match o with
        | None ->
            Arity.Pred.destruct Zero
              (function
                | None -> []
                | Some _ -> raise Modules.StructuralMismatch)
              (let open Arity.Pred.ArrowSequence in
                function [] -> Applicative.pure None)
        | Some x ->
            Arity.Pred.destruct (Succ Zero)
              (function
                | None -> raise Modules.StructuralMismatch
                | Some x -> [x])
              (let open Arity.Pred.ArrowSequence in
                function [fx] ->
                  Applicative.map Stdlib.Option.some (fx (traverse_'a x)))

      method visit_ref :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a ref, 'a ref Applicative.t) Arity.t =
      fun traverse_'a ->
        Arity.destruct (Succ Zero)
          (fun x -> [!x])
          (let open Arity.ArrowSequence in
            function [fx] -> Applicative.map ref (fx traverse_'a))

      method visit_result :
        'a . ('a, 'a Applicative.t) Arity.t -> ('b, 'b Applicative.t) Arity.t ->
          (('a, 'b) result, ('a, 'b) result Applicative.t) Arity.t =
      fun traverse_'a traverse_'b o ->
        match o with
        | Ok x ->
            Arity.Pred.destruct (Succ Zero)
              (function
                | Ok x -> [x]
                | Error _ -> raise Modules.StructuralMismatch)
              (let open Arity.Pred.ArrowSequence in
                function [fx] ->
                  Applicative.map Stdlib.Result.ok (fx (traverse_'a x)))
        | Error x ->
            Arity.Pred.destruct (Succ Zero)
              (function
                | Ok _ -> raise Modules.StructuralMismatch
                | Error x -> [x])
              (let open Arity.Pred.ArrowSequence in
                function [fx] ->
                  Applicative.map Stdlib.Result.error (fx (traverse_'b x)))

      method visit_seq :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a Stdlib.Seq.t, 'a Stdlib.Seq.t Applicative.t) Arity.t =
      fun visit_'a s ->
        match s () with
        | Stdlib.Seq.Nil ->
            Arity.Pred.destruct Zero
              (function s ->
                match s () with
                | Stdlib.Seq.Nil -> []
                | Stdlib.Seq.Cons _ -> raise Modules.StructuralMismatch)
              (let open Arity.Pred.ArrowSequence in
                function [] -> Applicative.pure Stdlib.Seq.empty)
        | Stdlib.Seq.Cons (hd, tl) ->
            Arity.Pred.destruct (Succ (Succ Zero))
              (function s ->
                match s () with
                | Stdlib.Seq.Nil -> raise Modules.StructuralMismatch
                | Stdlib.Seq.Cons (hd, tl) -> [hd; tl])
              (let open Arity.Pred.ArrowSequence in
                function [hd'; tl'] ->
                  Applicative.apply
                    (Applicative.map Stdlib.Seq.cons (hd' (visit_'a hd)))
                    (fun () -> (tl' (self#visit_seq visit_'a tl))))

      method visit_string : (string, string Applicative.t) Arity.t =
        traverse_atomic

      method visit_unit : (unit, unit Applicative.t) Arity.t =
        traverse_atomic
    end
end
