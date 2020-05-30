(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Node
open Cursor
open Result

(* The result is unspecified or unpredictable if the tree is modified
   during the traverse *)
let traverse (acc, dests, segs, c) f =
  let c, v = view c in
  let rec next (acc, dests, segs, c) = match dests with
    | `Exit -> Ok (acc, `Exit, segs, c)
    | `Up (segs, dests) -> 
        go_up c >>= fun c -> 
        next (acc, dests, segs, c)
    | `Right dests -> 
        go_side Right c >>| fun c -> 
        (acc, `Up (segs, dests), Segment.Segs.add_side segs Right, c)
  in
  let rec exit (acc, dests, segs, c) = match dests with
    | `Exit -> Ok (acc, `Exit, segs, c)
    | `Up (segs, dests) ->
        go_up c >>= fun c -> exit (acc, dests, segs, c)
    | `Right dests -> exit (acc, dests, segs, c)
  in
  match f acc segs c with
  | Error e -> Error e
  | Ok (`Exit, acc) -> exit (acc, dests, segs, c)
  | Ok (`Up, acc) -> next (acc, dests, segs, c)
  | Ok (`Continue, acc) ->
      match v with
      | Leaf _ | Bud (None, _, _) -> next (acc, dests, segs, c)
      | Bud (Some _, _, _) ->
          begin
            go_below_bud c >>= function
            | None -> assert false
            | Some c -> Ok (acc, `Up (segs, dests), Segment.Segs.push_bud segs, c)
          end
      | Internal (_, _, _, _) ->
          go_side Left c >>= fun c ->
          Ok (acc, `Up (segs, `Right dests), Segment.Segs.add_side segs Left, c)
      | Extender (seg, _, _, _) ->
          go_down_extender c >>= fun c ->
          Ok (acc, `Up (segs, dests), Segment.Segs.append_seg segs seg, c)

let fold ~init segs c f = 
  let debug_pwd = Cursor.segs_of_cursor c in
  let rec aux (acc, dests, segs, c) =
    traverse (acc, dests, segs, c) f >>= function
    | (acc, `Exit, _, c) -> Ok (acc, c)
    | x -> aux x
  in
  aux (init, `Exit, segs, c) >>| fun (acc, c) ->
  let debug_pwd' = Cursor.segs_of_cursor c in
  if try not @@ List.for_all2 Segment.equal debug_pwd debug_pwd' with _ -> true then begin
    Log.fatal "fold : %S %S" (Segment.string_of_segments debug_pwd) (Segment.string_of_segments debug_pwd');
  end;
  assert (List.for_all2 Segment.equal debug_pwd debug_pwd');
  (acc, c)

(* If [c] points to a bud, then it returns itself,
   rather than returning its contents.
 *)
let ls c =
  let get_seg segs =
    match Segment.Segs.finalize segs with
    | [seg] -> seg
    | _ -> assert false
  in
  let f acc segs c =
    let _, v = Cursor.view c in
    match v with
    | Leaf _ -> Ok (`Continue, (get_seg segs, v)::acc)
    | Bud _ -> Ok (`Up, (get_seg segs, v)::acc)
    | _ -> Ok (`Continue, acc)
  in
  fold ~init:[] Segment.Segs.empty c f

let ls_rec c =
  let f acc segs c =
    let _, v = Cursor.view c in
    match v with
    | Leaf _ -> Ok (`Continue, (Segment.Segs.finalize segs, View v)::acc)
    | _ -> Ok (`Continue, acc)
  in
  fold ~init:[] Segment.Segs.empty c f

module GenTraverse : sig
  type ('acc, 'data) visitor
  (** Traversal handle *)

  type dir = [`Bud | `Extender | `Side of Segment.side]

  val prepare
    : 'acc
    -> 'data
    -> Cursor.t
    -> ('acc (* accumulator *)
        -> 'data (* Data attached to the node *)
        -> Cursor.t (* The cursor *)
        -> (Cursor.t (* Updated cursor *)
            * ('acc
               * ('data (* Data attached to the new nodes *)
                  * dir (* Location of new nodes *)) list option),
            Error.t) Result.t)
    -> ('acc, 'data) visitor

  val step : ('acc, 'data) visitor 
    -> ([`Finished of Cursor.t * 'acc | `Continue of ('acc, 'data) visitor], Error.t) Result.t 
  (** Run one step of traversal. *)

  val fold
    : 'acc -> 'data -> Cursor.t
    -> ('acc -> 'data -> Cursor.t -> (Cursor.t * ('acc * ('data * dir) list option), Error.t) Result.t)
    -> (Cursor.t * 'acc, Error.t) Result.t
  (** Traversal in one call.  Not interleavable. *)
      
  val ls : Cursor.t -> (Cursor.t * (Segment.t * Node.t) list, Error.t) Result.t 
  val ls_rec : Cursor.t -> (Cursor.t * (Segment.t list * Node.t) list, Error.t) Result.t 

end = struct
  type dir = [`Bud | `Extender | `Side of Segment.side]
  type 'data jobs = 
    | Up of 'data jobs
    | Next of ('data * dir) list * 'data jobs
    | End

    (*
      let rec pp_jobs ppf = function
        | End -> Format.fprintf ppf "End"
        | Parent (ds, jobs) ->
            Format.fprintf ppf "@[<2>Parent [%s] %a@]"
              (String.concat "; " ds)
              pp_jobs jobs

      let _pp_jobs = pp_jobs (* debugging *)
    *)

  type ('acc, 'data) visitor =
    { acc : 'acc
    ; f : ('acc -> 'data -> Cursor.t -> (Cursor.t * ('acc * ('data * dir) list option), Error.t) Result.t)
    ; data : 'data
    ; cursor : Cursor.t
    ; jobs : 'data jobs
    }

  (* The result is unspecified or unpredictable if the tree is modified
     during the traverse *)
  let step { acc; f; data; cursor=c; jobs } =
    let rec exit jobs c = 
      (* Pick a next job and update the state for it *)
      match jobs with
      | End -> return c
      | Up jobs -> go_up c >>= fun c -> exit jobs c
      | Next (_, jobs) -> exit jobs c
    in
    let rec next acc jobs c = 
      (* Pick a next job and update the state for it *)
      match jobs with
      | End -> return (`Finished (c, acc))
      | Up jobs -> go_up c >>= fun c -> next acc jobs c
      | Next ([], jobs) -> next acc jobs c
      | Next ((data,`Side s)::ds, jobs) ->
          go_side s c >>| fun c ->
          `Continue { acc; f; data; cursor=c; jobs= Up (Next (ds, jobs)) }
      | Next ((data,`Extender)::ds, jobs) ->
          go_down_extender c >>| fun c ->
          `Continue { acc; f; data; cursor=c; jobs= Up (Next (ds, jobs)) }
      | Next ((data,`Bud)::ds, jobs) ->
          go_below_bud c >>= function 
          | None -> next acc (Next (ds, jobs)) c
          | Some c -> 
              return (`Continue { acc; f; data; cursor=c; jobs= Up (Next (ds, jobs)) })
    in
    (* compute something on the directory *)
    f acc data c >>= fun (c, (acc, todo)) ->
    match todo with
    | None -> 
        (* Abort. Roll back the cursor to the original position *)
        exit jobs c >>= fun c -> return (`Finished (c, acc))
    | Some ds ->
        let jobs = Next (ds, jobs) in
        next acc jobs c

  let prepare acc data cursor f = { acc; f; jobs= End; data; cursor }

  let fold init data cursor f =
    let rec loop tr = step tr >>= function
      | `Finished a -> return a
      | `Continue tr -> loop tr
    in
    loop @@ prepare init data cursor f

  let ls c =
   let f acc rev_seg c =
     let c, v = Cursor.view c in
     match v with
     | Leaf _ -> Ok (c, ( (Segment.of_sides @@ List.rev rev_seg, View v)::acc, Some [] ))
     | Bud _ -> Ok (c, ( (Segment.of_sides @@ List.rev rev_seg, View v)::acc, Some []))
     | Internal (_, _, _, _) ->
         Ok (c, ( acc, Some [ Segment.Left :: rev_seg, `Side Segment.Left
                            ; Segment.Right :: rev_seg, `Side Segment.Right]))
     | Extender (seg, _, _, _) ->
         Ok (c, ( acc, Some [ List.rev_append (Segment.to_sides seg) rev_seg, `Extender]))
   in
   fold [] [] c f
     
  let ls_rec c =
    let f acc segs c =
      let c, v = Cursor.view c in
      match v with
      | Leaf _ -> 
          Ok (c, (( Segment.Segs.finalize segs, View v)::acc, Some []))
      | Bud _ -> 
          Ok (c, ( acc, Some [ Segment.Segs.push_bud segs, `Bud ]))
      | Internal (_, _, _, _) ->
          Ok (c, (acc, Some [ Segment.Segs.add_side segs Segment.Left, `Side Segment.Left
                            ; Segment.Segs.add_side segs Segment.Right, `Side Segment.Right
                            ]))
      | Extender (seg, _, _, _) ->
          Ok (c, (acc, Some [ Segment.Segs.append_seg segs seg, `Extender ]))
    in
    fold [] Segment.Segs.empty c f
end
