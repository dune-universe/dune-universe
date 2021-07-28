open Core
open Util
open My_dict

module type PrefData = sig
  include Data
  val preferred : t -> t -> bool
end

module DisjointSetWithSetDataOf
    (DA : PrefData)
    (DS : Data)
    (EF : Container with type t = DA.t -> DS.t)
    (MF : Container with type t = DS.t -> DS.t -> DS.t) =
struct
  module D = DictOf(DA)(RefOf(PairOf(DA)(DS)))

  type elt = DA.t
  type sdata = DS.t

  type t = D.t
  [@@deriving ord, show, hash]

  let empty = D.empty

  let rec find_representative (ds:t) (e:elt)
    : (elt * sdata) =
    begin match D.lookup ds e with
      | None -> (e,EF.v e)
      | Some pref ->
        let (erep,srep) = !pref in
        if is_equal (DA.compare e erep) then
          (erep,srep)
        else
          let rep = find_representative ds erep in
          pref := rep;
          rep
    end

  let union_elements (ds:t) (e1:elt) (e2:elt) : t =
    let (e1rep,s1v) = find_representative ds e1 in
    let (e2rep,s2v) = find_representative ds e2 in
    if (is_equal (DA.compare e1rep e2rep)) then
      ds
    else
      let mv = MF.v s1v s2v in
      let reference =
        if DA.preferred e1rep e2rep then
          (ref (e1rep,mv))
        else
          (ref (e2rep,mv))
      in
      let ds =
        D.insert
          ds
          e1rep
          reference
      in
      let ds =
        D.insert
          ds
          e2rep
          reference
      in
      ds

  let create_from_equivalences
      (equivs:(elt * elt) list) : t =
    List.fold_left
      ~f:(fun acc (e1,e2) ->
          union_elements acc e1 e2)
      ~init:(empty)
      equivs

  let create_from_list
      ~(equiv:elt -> elt -> bool)
      (es:elt list)
    : t =
    let (equivs,_) =
      List.fold
        ~f:(fun (equivs,reps) e ->
            let found_rep_o =
              List.find
                ~f:(equiv e)
                reps
            in
            begin match found_rep_o with
              | None ->
                (equivs,e::reps)
              | Some r ->
                ((e,r)::equivs,reps)
            end)
        ~init:([],[])
        es
    in
    create_from_equivalences
      equivs
end


module DisjointSetOf(DA : PrefData) =
struct
  module EF = struct type t = DA.t -> unit let v = func_of () end
  module MF = struct type t = unit ->unit -> unit let v = func_of (func_of ()) end
  module DS = DisjointSetWithSetDataOf(DA)(UnitModule)(EF)(MF)

  type elt = DA.t

  type t = DS.t
  [@@deriving ord, show, hash]

  let empty = DS.empty

  let find_representative (ds:t) (e:elt) : elt =
    fst (DS.find_representative ds e)

  let union_elements (ds:t) (e1:elt) (e2:elt) : t =
    DS.union_elements ds e1 e2

  let create_from_equivalences
      (equivs:(elt * elt) list) : t =
    DS.create_from_equivalences equivs

  let create_from_list
      ~(equiv:elt -> elt -> bool)
      (list:elt list)
    : t =
    DS.create_from_list ~equiv list
end
