(* Top-level loop for the chase *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Formula
open Structure
open Step

(* The chase computes a tree of structures.  Each structure is stored
   in a frame.  A frame contains the information required to recreate
   the tree.  Each frame is identified by a label.  Labels are
   integers that are allocated sequentially starting from zero.  When
   the frame is not at the root of the tree, a frame records its
   parent and the formula that was used to create the stucture in the
   frame. *)

(* Fairness is address by maintaining a list of formulas within a
   frame.  When a new frame is created, the formula list is rotated by
   moving the first formula to the end of the list. *)

(* Status of a frame *)
type status = Sat | Unsat | Aborted

type frame = {
    label: int;
    parent: int option;
    cause: int option;          (* Formula that created this frame *)
    mutable status: status;
    rules: form list;
    structure: structure;
  }

(* The initial frame contains all of the constants equated to
   themselves. *)
let origin {consts; forms} =
  let f x = E (F (x, []), C x) in
  let stc = augment_struct
              (mt_struct ())
              (List.map f consts) in
  {
    label = 0;
    parent = None;
    cause = None;
    status = Unsat;
    rules = forms;
    structure = stc;
  }

(* Function used to rotate a list of formulas *)
let rotate = function
  | [] -> []
  | x :: xs -> xs @ [x]

(* When a limit is exceeded, mark the elements of the queue as being
   borted and print them. *)
let abort pr q fr msg =
  let rec loop () =
    if Queue.is_empty q then
      failwith msg
    else
      let fr = Queue.take q in
      fr.status <- Aborted;
      pr fr;
      loop () in
  match fr with
  | None -> loop ()
  | Some fr ->                  (* fr was removed from the queue *)
     fr.status <- Aborted;      (* but not yet printed *)
     pr fr;
     loop ()

(* This is the main loop. *)
let rec loop one bound limit pr q i =
  if Queue.is_empty q then
    ()                          (* Success *)
  else if i > limit then        (* Check step limit *)
    abort pr q None "Step limit exceeded"
  else
    let fr = Queue.take q in
    if size fr.structure > bound then (* Check size bound *)
      abort pr q (Some fr) "Structure size bound exceeded"
    else                        (* Take a step *)
      rules one bound limit pr q i fr fr.rules

(* Try lightweight rules *)
and rules one bound limit pr q i fr = function
  | [] ->                       (* Try heavyweight rules *)
     rest one bound limit pr q i fr fr.rules
  | r :: rs when not r.enabled || not @@ is_lightweight r ->
     rules one bound limit pr q i fr rs (* Skip formula *)
  | r :: rs ->            (* Use only enabled, lightweight formulas *)
     (match step fr.structure r with
      | None ->                 (* Formula satisfied *)
         if no_antec r then     (* Disable a fact after *)
           r.enabled <- false;  (* it has been tried once *)
         rules one bound limit pr q i fr rs
      | Some stcs ->            (* Chase step found *)
         pr fr;                 (* Print structure *)
         if no_antec r then     (* Disable a fact after *)
           r.enabled <- false;  (* it has been used once *)
         structs one bound limit pr q (rotate fr.rules) fr.label r.tag i stcs)

(* Try heavyweight rules *)
and rest one bound limit pr q i fr = function
  | [] ->                       (* Structure satisfies all formulas *)
     fr.status <- Sat;
     pr fr;                     (* Print model *)
     if one then
       ()                       (* Success *)
     else
       loop one bound limit pr q i
  | r :: rs when not r.enabled || is_lightweight r ->
     rest one bound limit pr q i fr rs (* Skip formula *)
  | r :: rs ->            (* Use only enabled, heavyweight formulas *)
     (match step fr.structure r with
      | None ->                 (* Formula satisfied *)
         if no_antec r then     (* Disable a fact after *)
           r.enabled <- false;  (* it has been tried once *)
         rest one bound limit pr q i fr rs
      | Some stcs ->            (* Chase step found *)
         pr fr;                 (* Print structure *)
         if no_antec r then     (* Disable a fact after *)
           r.enabled <- false;  (* it has been used once *)
         structs one bound limit pr q (rotate fr.rules) fr.label r.tag i stcs)

(* Add new structues to the queue *)
and structs one bound limit pr q rules parent tag i = function
  | [] ->
     loop one bound limit pr q i
  | stc :: stcs ->
     let fr = {
         label = i;
         parent = Some parent;
         cause = Some tag;
         status = Unsat;
         rules = rules;
         structure = stc } in
     Queue.add fr q;
     structs one bound limit pr q rules parent tag (i + 1) stcs

(* Module entry point *)
let solve one bound limit pr axioms =
  let q = Queue.create () in
  Queue.add (origin axioms) q;
  loop one bound limit pr q 1
