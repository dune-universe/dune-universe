open Containers
module Array = Vector

module MakeCompactor (T : Set.OrderedType) = struct
  type t = {
    mutable numCompactions : int;
    alternate : bool;
    mutable elts : T.t Vector.vector;
  }

  let create ?(alternate = true) ?(elts = Vector.create ()) () =
    { numCompactions = 0; alternate; elts }

  let length { elts; _ } = Vector.length elts

  let iter f { elts; _ } = Vector.iter f elts

  let push t v = Vector.push t.elts v

  let extend t elts = Vector.append t.elts elts

  let compact t =
    let odd =
      if t.alternate then t.numCompactions mod 2 = 1 else Random.bool ()
    in
    Vector.sort' T.compare t.elts;
    let lastItem =
      if Vector.length t.elts mod 2 = 1 then Vector.pop t.elts else None
    in
    let newElts = Vector.create () in
    Vector.iteri
      (fun i v -> if Bool.equal odd (i mod 2 = 1) then Vector.push newElts v)
      t.elts;
    Vector.clear t.elts;
    Option.iter (Vector.push t.elts) lastItem;
    t.numCompactions <- succ t.numCompactions;
    newElts
end

module type S = sig
  type elt

  type t

  val create :
    ?k:int -> ?c:float -> ?lazy_mode:bool -> ?alternate:bool -> unit -> t

  val update : t -> elt -> unit

  val update_n : t -> elt -> int -> unit

  val cdf : t -> (elt * float) array

  val pp_cdf : elt Fmt.t -> Format.formatter -> (elt * float) array -> unit
end

module Make (T : Set.OrderedType) : S with type elt := T.t = struct
  module Compactor = MakeCompactor (T)

  type t = {
    k : int;
    c : float;
    lazy_mode : bool;
    alternate : bool;
    compactors : Compactor.t Vector.vector;
    mutable size : int;
    mutable maxSize : int;
  }

  let update_size t =
    t.size <- Vector.fold (fun a c -> a + Compactor.length c) 0 t.compactors

  let capacity t heigth =
    let depth = Vector.length t.compactors - heigth - 1 in
    succ (int_of_float (ceil (float t.k *. (t.c ** float depth))))

  (* [grow t] adds an additional empty compactor to [t] and update
     [maxSize]. *)
  let grow t =
    Vector.push t.compactors (Compactor.create ());
    let _, newMaxSize =
      Vector.fold
        (fun (i, a) _ -> (succ i, a + capacity t i))
        (0, 0) t.compactors
    in
    t.maxSize <- newMaxSize

  let create ?(k = 128) ?(c = 2. /. 3.) ?(lazy_mode = true) ?(alternate = true)
      () =
    let t =
      {
        k;
        c;
        lazy_mode;
        alternate;
        compactors = Vector.create ();
        size = 0;
        maxSize = 0;
      }
    in
    grow t;
    t

  let compress_aux t =
    Vector.iteri
      (fun i c ->
        if Compactor.length c >= capacity t i then (
          if succ i >= Vector.length t.compactors then grow t;
          let newElts = Compactor.compact t.compactors.(i) in
          Compactor.extend t.compactors.(succ i) newElts;
          update_size t;
          if t.lazy_mode then raise_notrace Exit ))
      t.compactors

  let compress t = try compress_aux t with Exit -> ()

  let update t v =
    Compactor.push t.compactors.(0) v;
    t.size <- succ t.size;
    if t.size >= t.maxSize then (
      compress t;
      assert (t.size < t.maxSize) )

  let update_n t v n =
    for _ = 0 to n - 1 do
      update t v
    done

  let cdf t =
    let itemsAndWeights = Vector.create () in
    Vector.iteri
      (fun i c ->
        Compactor.iter (fun e -> Vector.push itemsAndWeights (e, 2 lsl i)) c)
      t.compactors;
    let totWeight = Vector.fold (fun a (_, w) -> a + w) 0 itemsAndWeights in
    Vector.sort' Stdlib.compare itemsAndWeights;
    let res = Vector.create () in
    let _ =
      Vector.fold
        (fun cw (e, w) ->
          let cw = cw + w in
          Vector.push res (e, float cw /. float totWeight);
          cw)
        0 itemsAndWeights
    in
    Vector.to_array res

  let pp_cdf pp ppf t =
    let pp_line ppf (v, p) = Fmt.pf ppf "%a %f" pp v p in
    Fmt.pf ppf "%a@." (Fmt.array ~sep:Format.pp_print_newline pp_line) t
end
