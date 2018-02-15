
(******************************************************************************
 * OC4.5
 * A pure OCaml implementation of C4.5 algorithm
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

(*********************** DATA TYPES ******************************************)

exception InvalidArgument of string

module type Comparable = sig
	type t
	val compare : t -> t -> int
	val avg : t -> t -> t
end

module type S = sig
	exception InvalidArgument of string
	type feature = int
	exception BadContinuity of feature
    exception DiscreteFeatOutOfBounds of feature*int
	type category = int
	type contData
	type dataVal = Discrete of int | Continuous of contData
	type data = dataVal array
	type trainVal = {
		data : data ;
		category : category
	}
	type trainSet

	type decisionTree
	val c45 : trainSet -> decisionTree
	val classify : decisionTree -> data -> category
	val emptyTrainSet : int -> int -> bool array -> trainSet
	val addData : trainVal -> trainSet -> trainSet
	val addDataList : trainVal list -> trainSet -> trainSet
	val getSet : trainSet -> trainVal list
	val setFeatureMax : int -> int -> trainSet -> unit
	val getNbFeatures : trainSet -> int
	val getFeatureMax : trainSet -> int array
	val getFeatContinuity : trainSet -> bool array
	val getNbCategories : trainSet -> int
	val getSetSize : trainSet -> int
	val toDot : Format.formatter -> (Format.formatter -> contData -> unit)
		-> decisionTree -> unit
	val toDotStdout : (Format.formatter -> contData -> unit) -> decisionTree
		-> unit
end

module Make(X: Comparable) = struct
	exception InvalidArgument of string
	type feature = int
	exception BadContinuity of feature
    exception DiscreteFeatOutOfBounds of feature*int
	type category = int
	type contData = X.t
	type dataVal = Discrete of int | Continuous of contData
	type data = dataVal array
	type trainVal = {
		data : data ;
		category : category
	}
	type trainSet = {
		set : trainVal list ;
		nbFeatures : int ;
		featureMax : int array; (* Max value for the discrete feature a *)
		featContinuity : bool array ;
		nbCategories : int ;
		setSize : int (* number of training values in the training set *)
	}
	type workingSet = {
		(** The set c45 maintains while generating a tree *)
		selectedFeat : bool array ;
			(** Which features have already been selected? *)
		origSet : trainVal list ;
			(** The original data set *)
		maxTreeDepth : int
			(** Do not grow the tree deeper than this value. \infty : -1 *) 
	}
	module DVMap = Map.Make (struct type t = dataVal let compare = compare end)

    type decisionConstructionTree =
        | CDecisionLeaf of category
        | CDecisionEmptyLeaf
		| CDecisionDiscreteNode of feature * decisionConstructionTree DVMap.t
		| CDecisionContinuousNode of feature * contData (* threshold *) *
				decisionConstructionTree (* lower *) *
                decisionConstructionTree (* upper *)

	type decisionTree =
        | DecisionLeaf of category
		| DecisionDiscreteNode of feature * decisionTree DVMap.t
		| DecisionContinuousNode of feature * contData (* threshold *) *
				decisionTree (* lower *) * decisionTree (* upper *)

	(* Note that featureMax will be inferred *)
	let emptyTrainSet nbFeatures nbCategories featContinuity  = 
		if nbFeatures <> (Array.length featContinuity) then
			raise (InvalidArgument ("featContinuity must have length"^
					" nbFeatures."));
		{set = [] ;
		nbFeatures = nbFeatures ;
		featureMax = Array.make nbFeatures 0 ;
		featContinuity = featContinuity ;
		nbCategories = nbCategories ;
		setSize = 0
		}
		
	let addData trainVal trainSet =
		for feat = 0 to (trainSet.nbFeatures - 1) do
			(match trainSet.featContinuity.(feat), trainVal.data.(feat)  with
			| true, Discrete(_) | false, Continuous(_) ->
				raise (BadContinuity feat)
			| true, Continuous(_) -> ()
			| false,Discrete(v) ->
				trainSet.featureMax.(feat) <-
					max trainSet.featureMax.(feat) v
			)
		done;
		{set = trainVal :: trainSet.set ;
		nbFeatures = trainSet.nbFeatures ;
		featureMax = trainSet.featureMax ;
		featContinuity = trainSet.featContinuity ;
		nbCategories = trainSet.nbCategories ;
		setSize = trainSet.setSize + 1
		}

	let rec addDataList trainList trainSet = match trainList with
	| [] -> trainSet
	| hd::tl -> addDataList tl (addData hd trainSet)

	let setFeatureMax feat maxVal trainSet =
		trainSet.featureMax.(feat) <- maxVal

	(* get functions *)
	let getSet trainSet =
		trainSet.set

	let getNbFeatures trainSet = 
		trainSet.nbFeatures 

	let getFeatureMax trainSet =
		trainSet.featureMax

	let getFeatContinuity trainSet =
		trainSet.featContinuity

	let getNbCategories trainSet =
		trainSet.nbCategories 

	let getSetSize trainSet =
		trainSet.setSize 

	(* graph generation *)
	let toDot fmt contPPrint (tree : decisionTree) =
		let prettyPrintData fmt = function
		| Continuous(x) -> Format.fprintf fmt "%a" contPPrint x
		| Discrete(x) -> Format.fprintf fmt "%d" x
		in

		let cId = ref 0 in
		let incr r = r := !r + 1 in
		let rec printTree = function
		| DecisionLeaf cat ->
			Format.fprintf fmt "%d [label=\"Cat. %d\"]@\n" !cId cat;
			incr cId;
			!cId - 1
		| DecisionDiscreteNode(feat,children) ->
			Format.fprintf fmt "%d [shape=box,label=\"Feat %d\"]@\n" !cId feat;
			let cellId = !cId in
			incr cId;
			DVMap.iter (fun key child ->
				let ccid = printTree child in
				Format.fprintf fmt "%d -> %d [label=\"=%a\"]@\n"
					cellId ccid prettyPrintData key)
				children;
			cellId
		| DecisionContinuousNode(feat, thres, low, high) ->
			let cellId = !cId in
			incr cId;
			Format.fprintf fmt "%d [shape=box,label=\"Feat %d\"]@\n"
				cellId feat ;
			let lowId = printTree low and highId = printTree high in
			Format.fprintf fmt "%d -> %d [label=\"< %a\"]@\n"
				cellId lowId contPPrint thres ;
			Format.fprintf fmt "%d -> %d [label=\">= %a\"]@\n"
				cellId highId contPPrint thres;
			cellId
		in
		Format.open_hovbox 4 ;
		Format.fprintf fmt "digraph decisionTree {@\n";
		let _ = printTree tree in
		Format.close_box () ;
		Format.fprintf fmt "@\n}@."

	let toDotStdout = toDot Format.std_formatter
	(******************* END DATA TYPES *************************************)

	let majorityCasesThreshold = 5
	let epsilonGain = 0.000001
		
	let (<|>) a b =
		(** a|b : generates the list [a ; a+1 ; ... ; b-1] *)
		let rec span b cur =
			if a = b then a::cur
				else span (b-1) (b::cur)
		in span (b-1) []
	let bxor a b = match a,b with
	| true,true | false,false -> false
	| _,_ -> true

	module IMap = Map.Make(struct type t=int let compare = compare end)

	let majorityVote l =
		(** Returns the most present value in l. If the maximum is not unique,
			returns an arbitrary value among the possible ones. *)
		let counts = List.fold_left
			(fun map x -> IMap.add x
				((try IMap.find x map with Not_found -> 0) + 1) map)
			IMap.empty l in
		let _,maxarg = IMap.fold (fun arg v (cMax,cArg) ->
			if v > cMax then (v,arg) else (cMax,cArg)) counts (-1,-1) in
		maxarg

	(* classify data based on a decision tree *)
    let rec classify tree data = match tree with
		| DecisionLeaf category -> category
		| DecisionDiscreteNode (feat, decisionTreeMap) ->
            (try
                classify (DVMap.find data.(feat) decisionTreeMap) data
            with Not_found ->
                let v = (match data.(feat) with
                    | Discrete a -> a
                    | Continuous _ -> raise (BadContinuity feat)) in
                raise (DiscreteFeatOutOfBounds (feat,v)))
		| DecisionContinuousNode (feat, thresh, lowerTree, upperTree) ->
			(match data.(feat) with
			| Discrete(_) -> raise (BadContinuity feat)
			| Continuous(x) ->
				if x < thresh 
					then classify lowerTree data
					else classify upperTree data 
			)

	let rec closestUnder curClosest bound l = match l, curClosest with
	| [],None -> bound
	| [],Some a -> a
	| hd::tl,Some closest when hd > closest && hd <= bound ->
		closestUnder (Some hd) bound tl
	| hd::tl,None when hd <= bound ->
		closestUnder (Some hd) bound tl
	| _::tl,_ -> closestUnder curClosest bound tl
		

	let rec do_c45 trainset workSet depth =
		let fsum = List.fold_left (fun cur x -> cur +. x) 0. in
		let log2 x = (log x) /. (log 2.) in

		let countFilter filter = List.fold_left
			(fun cur x -> if filter x then (cur+1) else cur) 0 in

		let entropy filter =
			let catCount = Array.make (trainset.nbCategories) 0 in
			let nbTrainVal = ref 0 in
			List.iter (fun tv -> if filter tv then begin
				nbTrainVal := !nbTrainVal + 1 ;
				catCount.(tv.category) <- catCount.(tv.category) + 1 end)
				trainset.set ;
			-1. *. fsum (List.map (fun k ->
						let x = (float_of_int k) /.
							(float_of_int !nbTrainVal) in
						(match k with
                            | 0 -> 0.
                            | _ -> x *. (log2 x)))
					(Array.to_list catCount))
		in

		let contGains = Array.make (trainset.nbFeatures) 0. in
		let findContThreshold ft =
			let contVal ft = function
			| Discrete(_) -> raise (BadContinuity ft)
			| Continuous(x) -> x
			in

			let sorted=ref (List.sort
				(fun tv1 tv2 -> X.compare (contVal ft tv1.data.(ft))
					(contVal ft tv2.data.(ft))) trainset.set) in
			let leftCard = ref trainset.setSize in
			let leftFreq = Array.make (trainset.nbCategories) 0
			and rightFreq= Array.make (trainset.nbCategories) 0 in
			List.iter (fun x -> leftFreq.(x.category) <-
					leftFreq.(x.category)+1)
				trainset.set;
			let entropyWithTab tab card =
				let fcard = float_of_int card in
				let rat = fun a -> (float_of_int a) /. fcard in
				Array.fold_left (fun cur a -> (match a with
					| 0 -> cur
					| a -> cur -. (rat a) *. log2 (rat a))) 0. tab
			in
			let totInfo = entropyWithTab leftFreq !leftCard in
			let addCell tab id v = tab.(id) <- tab.(id) + v in
			let splitVal card =
				let oneSide c = (match c with
				| 0 -> 0.
				| c ->
					let fcard = float_of_int c in
					let rat = fcard /. (float_of_int trainset.setSize) in
					-. (rat *. (log2 rat))
				) in
				(oneSide card) +. (oneSide (trainset.setSize - card))
			in

			let rec nextInfoGain () = (match !sorted with
				| _::[] | [] -> raise Not_found
				| head::(hd2::_ as tl) ->
					sorted := tl ;
					let catChanged = head.category in
					addCell leftFreq catChanged (-1) ;
					addCell rightFreq catChanged 1 ;
					leftCard := !leftCard - 1 ;
					
					if head.data.(ft) = hd2.data.(ft) then
						nextInfoGain ()
					else begin
						let gain = totInfo -.
							(entropyWithTab leftFreq !leftCard)-.
							(entropyWithTab rightFreq
									(trainset.setSize - !leftCard)) in
						let gainRat = gain /. (splitVal !leftCard) in

						X.avg (contVal ft hd2.data.(ft))
							(contVal ft head.data.(ft)),
							gainRat
					end
				)
			in
			let rec bestPiv curMax (curMaxPiv: contData option) =
				(try
					let piv,entr = nextInfoGain () in
					if entr > curMax then
						bestPiv entr (Some piv)
					else bestPiv curMax curMaxPiv
				with Not_found ->
					curMaxPiv,curMax)
			in

			let piv,gain = bestPiv (-1.) None in
			contGains.(ft) <- gain ;
			piv
		in

		let featureGainRatio ft =
			let rec gainLoss curLoss curSplit = function
			| -1 -> curLoss,curSplit
			| v ->
				let filter = (fun tv -> tv.data.(ft) = (Discrete v)) in
				let count = countFilter filter trainset.set in
				let fcountrat = (float_of_int count) /.
					(float_of_int trainset.setSize) in
                let lfcountrat = (match count with
                    | 0 -> 0.
                    | _ -> log2 fcountrat) in
				let entr = entropy filter in
				gainLoss
					(curLoss +. fcountrat *. entr)
					(curSplit +. fcountrat *. lfcountrat)
					(v-1)
			in

			(match trainset.featContinuity.(ft) with
			| true -> contGains.(ft)
			| false -> (match workSet.selectedFeat.(ft) with
				| true -> 0. (* This discrete feature has previously been
					selected for a split. *)
				| false ->
					let wholeEntr = entropy (fun _ -> true) in
					let loss,spl = gainLoss 0. 0. (trainset.featureMax.(ft)) in
					(wholeEntr -. loss) /. (-.spl)
				)
			)
		in

		let majorityLeaf () =
			(* In case there is no majority, the result is an
			abritrary choice. *)
			CDecisionLeaf(majorityVote (List.map
				(fun tv -> tv.category) trainset.set))
		in

		if (trainset.setSize < majorityCasesThreshold) ||
				(* Only a few test cases remain in this trainset *)
				(depth > workSet.maxTreeDepth)
				(* Or the tree has grown beyond reasonable depth *)
				then
            (match trainset.setSize with
            | 0 -> CDecisionEmptyLeaf (* Eliminated afterwards. *)
            | _ -> majorityLeaf () (* Majority vote to insert a leaf *)
            )
        else begin
			let contThresholds = Array.init (trainset.nbFeatures)
				(fun x -> match trainset.featContinuity.(x) with
                    | false -> None
                    | true -> findContThreshold x) in
			let commonClass = List.fold_left
				(fun cur x -> if x.category = cur then cur else -1)
				((List.hd trainset.set).category)
				(List.tl trainset.set) in

			if commonClass >= 0 then
				(* Each trainVal has the same category: insert a leaf *)
				CDecisionLeaf(commonClass)
			else begin
				let maxGainFeature,maxGain = List.fold_left
					(fun (i,x) (j,y) ->
						(* Format.eprintf "%f " y ; (*DEBUG*) *)
						if y > x then (j,y) else (i,x))
					(-1,-1.)
					(List.map (fun i -> i,featureGainRatio i)
						(0 <|> trainset.nbFeatures))
					in
				(* Format.eprintf " -- sel. %d - %f ; %d elts@."
					maxGainFeature maxGain (getSetSize trainset); (*DEBUG*) *)

				if maxGain < epsilonGain then
					majorityLeaf ()
				else if trainset.featContinuity.(maxGainFeature) then begin
					let avgThreshold = (match contThresholds.(maxGainFeature)
						with
						| Some x -> x
						| None -> raise (InvalidArgument ("Selected a feature"^
							" without suitable threshold."))) in
					let threshold = closestUnder None avgThreshold
						(List.map
							(fun x -> match x.data.(maxGainFeature) with
								| Continuous(v) -> v
								| Discrete(_) ->
									raise (BadContinuity maxGainFeature))
							workSet.origSet) in

					let emptyset = { trainset with set = [] ; setSize = 0 } in
					let lower, upper = List.fold_left (fun (lset,uset) tv ->
						if tv.data.(maxGainFeature) <=
								Continuous(threshold) then
							(* NOTE Here it is important to keep <= and not <
							Indeed, the specification of X.avg is that, for
							a < b, a <= X.avg a b < b. Thus, to separate a
							from b, <= is needed. Else, we could hang forever
							*)
							{ lset with
								set = tv::lset.set ;
								setSize = lset.setSize+1 }, uset
						else
							lset, {uset with
								set = tv::uset.set ;
								setSize = uset.setSize+1 }
						) (emptyset,emptyset) trainset.set in
					CDecisionContinuousNode
						(maxGainFeature, threshold,
							do_c45 lower workSet (depth+1),
							do_c45 upper workSet (depth+1))
				end else begin
					workSet.selectedFeat.(maxGainFeature) <- true ;
					let submap = List.fold_left (fun map v ->
						let sset = List.filter
							(fun tv -> tv.data.(maxGainFeature) = Discrete(v))
							trainset.set in
						DVMap.add (Discrete v) (do_c45 { trainset with
									set = sset ;
									setSize = List.length sset
								}
								workSet
								(depth+1)) map)
							DVMap.empty
							(0<|>(trainset.featureMax.(maxGainFeature)+1)) in
					workSet.selectedFeat.(maxGainFeature) <- false ;
					CDecisionDiscreteNode (maxGainFeature,submap)
				end
			end
		end

    let refineConstructionTree tree nbCat =
        let sum2Arrays a1 a2 =
            assert (Array.length a1 = Array.length a2);
            Array.init (Array.length a1) (fun i -> a1.(i)+a2.(i))
        in
        let sumArrays l =
            List.fold_left sum2Arrays (List.hd l) (List.tl l) in
        let majority arr =
            snd (
                Array.fold_left (fun (pos,cur) x ->
                    if arr.(cur) < x then
                        (pos+1,pos)
                    else (pos+1,cur)) (0,0) arr
                )
        in
        let onlyOneVal arr =
            let module SweepTyp = struct
                type t = NotYet | Single of int | Multiple end in
            let res = fst 
                (Array.fold_left (fun (cur,pos) x -> (match cur,x with
                        | _,0 | SweepTyp.Multiple,_ -> (cur, pos+1)
                        | SweepTyp.NotYet,_ -> (SweepTyp.Single pos, pos+1)
                        | SweepTyp.Single _,_ -> (SweepTyp.Multiple, pos+1)))
                    (SweepTyp.NotYet, 0) arr) in
            (match res with
                | SweepTyp.Single n -> Some n
                | _ -> None)
        in
        let factorToLeaf votes elseNode = match onlyOneVal votes with
        | None -> elseNode
        | Some a -> DecisionLeaf a
        in

        let rec doRefine = function
        | CDecisionLeaf c ->
            Some (DecisionLeaf c,
                (Array.init nbCat (fun i -> if i=c then 1 else 0)))
        | CDecisionEmptyLeaf -> None 
        | CDecisionDiscreteNode(ft,map) ->
            let children = DVMap.fold (fun k v cur ->
                let nv = doRefine v in
                DVMap.add k nv cur) map (DVMap.empty) in
            let votes = sumArrays (DVMap.fold (fun _ v cur ->
                match v with
                | None -> cur
                | Some a -> (snd a)::cur) children []) in
            let majorityCat = majority votes in
            let childrenMap = (DVMap.fold (fun k v cur ->
                DVMap.add k (match v with
                    | None -> DecisionLeaf majorityCat
                    | Some a -> fst a) cur) children DVMap.empty) in
            let node = factorToLeaf votes
                (DecisionDiscreteNode(ft,childrenMap)) in
            Some (node,votes)
        | CDecisionContinuousNode(ft,dat,cleft,cright) ->
            let left = doRefine cleft
            and right= doRefine cright in
            let cList = [left; right] in
            let votes = sumArrays (List.fold_left (fun cur v -> match v with
                    | None -> cur
                    | Some a -> (snd a)::cur)
                [] cList) in
            let majorityCat = majority votes in
            let majLeaf = DecisionLeaf majorityCat in
            let replaceNone repl = function
            | None -> repl
            | Some a -> fst a
            in
            let node = factorToLeaf votes
                    (DecisionContinuousNode(ft,dat,
                        replaceNone majLeaf left,
                        replaceNone majLeaf right)) in
            Some (node,votes)
        in
        (match doRefine tree with
        | None -> assert false
        | Some a -> fst a)

	let c45 trainset =
		let defaultDepthBound ts =
			(** By default, the max bound of a tree depth is
				nbDiscreteFeatures + nbContinuousFeatures * nbCategories
				because the algorithm won't split more than once on a
				discrete feature, and it seems reasonable to consider that
				it should not split more than nbCategories times per
				continuous feature. *)
			let nbDiscreteFeat, nbContinuousFeat = Array.fold_left
				(fun (discFt,contFt) x -> match x with
					| true -> (discFt,contFt+1)
					| false ->(discFt+1,contFt))
				(0,0) ts.featContinuity in
			nbDiscreteFeat + nbContinuousFeat * ts.nbCategories
		in
		let workSet = {
			selectedFeat = Array.make (trainset.nbFeatures) false ;
			origSet = trainset.set ;
			maxTreeDepth = defaultDepthBound trainset } in
        let constructionTree = do_c45 trainset workSet 0 in
        refineConstructionTree constructionTree (trainset.nbCategories)
end

module IntOc45 = Make(struct 
		type t = int
		let compare = Pervasives.compare
		let avg a b = (a+b) asr 1 (* Round down towards -infty *)
	end)

module FloatOc45 = Make(struct
		type t = float
		let compare = Pervasives.compare
		let avg a b = (a+.b) /. 2.
	end)
