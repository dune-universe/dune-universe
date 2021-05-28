(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* This file sets up a selection mechanism that allows focusing the testing
   process on a subset of operations. *)

type op =

  (* Ephemeral sequences. *)

  | ECreate
  | EMake
  | EInit
  | EDefault
  | ELength
  | EIsEmpty
  | EClear
  | ECopy
  | EAssign
  | EPush
  | EPop
  | EPeek
  | EGet
  | ESet
  | EConcat
  | EAppend
  | ESplit
  | ECarve
  | ETake
  | EDrop
  | ESub
  | EIter
  | EIteri
  | EToList
  | EToArray
  | EToSeq
  | EOfListSegment
  | EOfList
  | EOfArraySegment
  | EOfArray
  | EOfSeqSegment
  | EOfSeq
  | EFind
  | EFindMap
  | EForall
  | EExists
  | EMem
  | EMap
  | EMapi
  | ERev
  | EFilter
  | EFilterMap
  | EPartition
  | EFlattenMap
  | EIter2
  | EMap2
  | EForall2
  | EExists2
  | EEqual
  | ECompare
  | ESort
  | EUniq
  | EMerge
  | EFill
  | EBlit

  (* Persistent sequences. *)

  | PCreate
  | PMake
  | PInit
  | PDefault
  | PLength
  | PIsEmpty
  | PPush
  | PPop
  | PPeek
  | PGet
  | PSet
  | PConcat
  | PSplit
  | PTake
  | PDrop
  | PSub
  | PIter
  | PIteri
  | PToList
  | PToArray
  | PToSeq
  | POfListSegment
  | POfList
  | POfArraySegment
  | POfArray
  | POfSeqSegment
  | POfSeq
  | PFind
  | PFindMap
  | PForall
  | PExists
  | PMem
  | PMap
  | PMapi
  | PRev
  | PFilter
  | PFilterMap
  | PPartition
  | PFlattenMap
  | PIter2
  | PMap2
  | PForall2
  | PExists2
  | PEqual
  | PCompare
  | PSort
  | PUniq
  | PMerge

  (* Conversions. *)

  | ESnapshot
  | EEdit

  (* Iterators. *)

  | EICreate | PICreate
  | IReset
  | ICopy
  | ILength
  | IIndex
  | IFinished
  | IGet
  | IGetSegment
  | IGetSegmentAndJump
  | IMove
  | IJump
  | IReach
  | IGetAndMove
  | IIsValid
  | ISet
  | ISetAndMove

(* -------------------------------------------------------------------------- *)

(* [testing op] determines whether the operation [op] may appear in fuzzing
   scenarios. By default, all operations are included. For targeted fuzz
   testing, rebind [testing] to a subset of the operations, e.g.,

     let testing op = List.mem op [ OpPush; OpPop; OpLetMake ]. *)

(* To test everything, use this: *)

let _everything _op =
  true

(* Let us define shorthands for groups of operations. *)

let _core_ephemeral_sequences = function
  | ECreate
  | ECopy
  | EPush
  | EPop
  | ESet
  | EAppend
  | ECarve
  | EOfArray
      -> true
  | _ -> false

let _ephemeral_sequences = function
  | ECreate
  | EMake
  | EInit
  | EDefault
  | ELength
  | EIsEmpty
  | EClear
  | ECopy
  | EAssign
  | EPush
  | EPop
  | EPeek
  | EGet
  | ESet
  | EConcat
  | EAppend
  | ESplit
  | ECarve
  | ETake
  | EDrop
  | ESub
  | EIter
  | EIteri
  | EToList
  | EToArray
  | EToSeq
  | EOfListSegment
  | EOfList
  | EOfArraySegment
  | EOfArray
  | EOfSeqSegment
  | EOfSeq
  | EFind
  | EFindMap
  | EForall
  | EExists
  | EMem
  | EMap
  | EMapi
  | ERev
  | EFilter
  | EFilterMap
  | EPartition
  | EFlattenMap
  | EIter2
  | EMap2
  | EForall2
  | EExists2
  | EEqual
  | ECompare
  | ESort
  | EUniq
  | EMerge
  | EFill
  | EBlit
      -> true
  | _ -> false

let _core_persistent_sequences = function
  | PCreate
  | PPush
  | PPop
  | PSet
  | PConcat
  | PSplit
  | POfArray
      -> true
  | _ -> false

let _persistent_sequences = function
  | PCreate
  | PMake
  | PInit
  | PDefault
  | PLength
  | PIsEmpty
  | PPush
  | PPop
  | PPeek
  | PGet
  | PSet
  | PConcat
  | PSplit
  | PTake
  | PDrop
  | PSub
  | PIter
  | PIteri
  | PToList
  | PToArray
  | PToSeq
  | POfListSegment
  | POfList
  | POfArraySegment
  | POfArray
  | POfSeqSegment
  | POfSeq
  | PFind
  | PFindMap
  | PForall
  | PExists
  | PMem
  | PMap
  | PMapi
  | PRev
  | PFilter
  | PFilterMap
  | PPartition
  | PFlattenMap
  | PIter2
  | PMap2
  | PForall2
  | PExists2
  | PEqual
  | PCompare
  | PSort
  | PUniq
  | PMerge
      -> true
  | _ -> false

let conversions = function
  | ESnapshot
  | EEdit
      -> true
  | _ -> false

let _iterators = function
  | EICreate | PICreate
  | IReset
  | ICopy
  | ILength
  | IIndex
  | IFinished
  | IGet
  | IGetSegment
  | IGetSegmentAndJump
  | IMove
  | IJump
  | IReach
  | IGetAndMove
  | IIsValid
  | ISet
  | ISetAndMove
      -> true
  | _ -> false

let _ephemeral_iterators op =
  _core_ephemeral_sequences op ||
  _iterators op

let _persistent_iterators op =
  _core_persistent_sequences op ||
  _iterators op

let _core op =
  _core_ephemeral_sequences op ||
  _core_persistent_sequences op

let _core_and_iterators op =
  _core op ||
  _iterators op

(* -------------------------------------------------------------------------- *)

(* Here is our current choice. *)

let testing =
  _everything
