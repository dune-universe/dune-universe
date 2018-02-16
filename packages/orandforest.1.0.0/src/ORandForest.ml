
(******************************************************************************
 * ORandForest
 * A pure OCaml implementation of a random forest classifier based on OC4.5.
 *
 * By Théophile Bastian <contact@tobast.fr>
 * and Noémie Fong (aka. Minithorynque), 2016.
 ******************************************************************************
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *****************************************************************************)

module type S = sig
  type c45_data
  type c45_category
  type c45_trainSet
  type randomForest

  val genRandomForest: ?ncores:int -> int -> c45_trainSet -> randomForest
  val classify: randomForest -> c45_data -> c45_category
  val classify_raw: randomForest -> c45_data -> (c45_category * float) list

  val save_to_file: string -> randomForest -> unit
  val restore_from_file: string -> randomForest
end

module Utils = struct
  type filename = string

  let with_out_file (fn: filename) (f: out_channel -> 'a): 'a =
    let output = open_out_bin fn in
    let res = f output in
    close_out output;
    res

  let with_in_file (fn: filename) (f: in_channel -> 'a): 'a =
    let input = open_in_bin fn in
    let res = f input in
    close_in input;
    res

  (* marshal to file *)
  let save (fn: filename) (x: 'a): unit =
    with_out_file fn (fun out ->
        Marshal.to_channel out x [Marshal.No_sharing]
      )

  (* unmarshal from file *)
  let restore (fn: filename): 'a =
    with_in_file fn (fun input ->
        Marshal.from_channel input
      )
end

(* An IMap with a constant time cardinal function
   (and just the IMap operations we need) *)
module CardIMap = struct

  (* the IMap module is just visible here; it should not be used outside *)
  module IMap = Map.Make(
    struct
      type t = int
      (* optimized integer comparison; not generic compare *)
      let compare (x: int) (y: int): int =
        if x > y then 1
        else if x < y then -1
        else 0
    end)

  type t = { card: int;
             map: int IMap.t}

  let add k v m =
    let new_card =
      if IMap.mem k m.map then
        m.card
      else
        m.card + 1 in
    { card = new_card;
      map = IMap.add k v m.map }

  let cardinal m =
    m.card

  let empty =
    { card = 0;
      map = IMap.empty }

  let find k m =
    IMap.find k m.map

  let find_opt k m =
    try Some (IMap.find k m.map)
    with Not_found -> None

  let fold f m init =
    IMap.fold f m.map init

  let iter f m =
    IMap.iter f m.map

  let mem k m =
    IMap.mem k m.map
end

module Make(X: Oc45.S) = struct
  (***************** DATA TYPES ********************************************)
  type c45_data = X.data
  type c45_category = X.category
  type c45_trainSet = X.trainSet

  type featureMap = CardIMap.t
  type randomForest = (X.decisionTree * featureMap) array
  (************* END DATA TYPES ********************************************)
  open X

  let () = Random.self_init ()

  let (<|>) a b =
    (** a|b : generates the list [a ; a+1 ; ... ; b-1] *)
    let rec span b cur =
      if a = b then a::cur
      else span (b-1) (b::cur)
    in span (b-1) []

  let randPick l =
    let card = List.length l in
    let elt = Random.int card in
    List.nth l elt

  let remapData featMap data =
    let out = Array.make (CardIMap.cardinal featMap) data.(0) in
    CardIMap.iter (fun from dest ->
	out.(dest) <- data.(from)) featMap ;
    out

  let majorityVote (l : int list) =
    (** Returns the most present value in l. If the maximum is not unique,
	returns an arbitrary value among the possible ones. *)
    let counts = List.fold_left
	(fun map x -> CardIMap.add x
	    ((try CardIMap.find x map with Not_found -> 0) + 1) map)
	CardIMap.empty l in
    let cMax,maxarg = CardIMap.fold (fun arg v (cMax,cArg) ->
	if v > cMax then
	  (v,[arg])
	else if v = cMax then
	  (v,arg::cArg)
	else
	  (cMax,cArg))
	counts (-1,[]) in
    assert (maxarg <> []) ;
    randPick maxarg

  let vote_frequencies (l: int list): (c45_category * float) list =
    (** Returns the list of classes (categories) along with
        the percentage of votes each one got. *)
    let total = ref 0 in
    let vote_counts =
      List.fold_left (fun map categ ->
          let nb_votes = match CardIMap.find_opt categ map with
            | None -> 0
            | Some n -> n in
          incr total;
          CardIMap.add categ (nb_votes + 1) map
        ) CardIMap.empty l in
    let total_votes = float !total in
    CardIMap.fold (fun categ nb_votes acc ->
        (categ, float nb_votes /. total_votes) :: acc
      ) vote_counts []

  let classify' defuzz (forest: randomForest) data =
    let unknow_category = min_int in
    let votesList = Array.fold_left (fun acc (tree, ftMap) ->
        let vote =
          try
            let v = X.classify tree (remapData ftMap data) in
            (* this category is not supposed to be used by users *)
            assert(v <> unknow_category);
            v
          with X.DiscreteFeatOutOfBounds (_, _) -> unknow_category
        in
        vote :: acc
      ) [] forest
    in
    defuzz votesList

  let classify =
    classify' majorityVote

  let classify_raw =
    classify' vote_frequencies

  let genRandomForest ?(ncores = 1) nbTrees (trainset : X.trainSet) : randomForest =
    let trainDataArray = Array.of_list (X.getSet trainset) in
    let randSubsetOf superSize subSize =
      let rec sel selected = function
	| 0 -> selected
	| k ->
	  let el = Random.int superSize in
	  if CardIMap.mem el selected then
	    sel selected k
	  else
	    sel (CardIMap.add el (k-1) selected) (k-1)
      in
      sel CardIMap.empty subSize
    in
    let selectFeatureSubset (trList : X.trainVal list) featCont =
      let subsize = int_of_float (sqrt (float_of_int
				          (X.getNbFeatures trainset))) in
      let selected = randSubsetOf (X.getNbFeatures trainset)
	  subsize in
      (List.fold_left (fun cur x ->
	   { x with data = remapData selected x.data}::cur )
	  [] trList),
      (remapData selected featCont),
      selected
    in
    let generateTree () =
      let nTrainList = List.fold_left (fun cur _ ->
	  let sample = Random.int (Array.length trainDataArray) in
	  (trainDataArray.(sample)) :: cur)
	  [] (0<|> (Array.length trainDataArray)) in
      let trainList, nCont, featMap = selectFeatureSubset nTrainList
	  (X.getFeatContinuity trainset) in

      let nTrainSet = List.fold_left (fun cur x -> X.addData x cur)
	  (X.emptyTrainSet
	     (Array.length ((List.hd trainList).data))
	     (X.getNbCategories trainset)
	     nCont)
	  trainList in
      let ftMaxArray = X.getFeatureMax trainset in
      CardIMap.iter (fun ft dest ->
          X.setFeatureMax dest ftMaxArray.(ft) nTrainSet) featMap;
      X.c45 nTrainSet, featMap
    in
    if ncores > 1 then
      let units = Array.make nbTrees () in
      Parmap.array_parmap
        ~init:(fun _child_rank -> Random.self_init ())
        ~ncores ~chunksize:1 generateTree units
    else
      Array.init nbTrees (fun i -> generateTree ())

  let save_to_file fn model =
    Utils.save fn model

  let restore_from_file fn =
    Utils.restore fn

end

module IntRandForest = Make(Oc45.IntOc45)
module FloatRandForest = Make(Oc45.FloatOc45)
