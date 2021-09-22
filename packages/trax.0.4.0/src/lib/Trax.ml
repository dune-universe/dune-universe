(*
   Capture stack trace when catching an exception and make sure to
   not lose it.
*)

open Printf

type location = Text of string
              | Raw_backtrace of Printexc.raw_backtrace

exception Traced of exn * location list

let wrap loc e =
  match e with
  | Traced (e, locs) -> Traced (e, loc :: locs)
  | e -> Traced (e, [loc])

let unwrap = function
  | Traced (e, _) -> e
  | e -> e

let wrap_with_stack_trace e =
  wrap (Raw_backtrace (Printexc.get_raw_backtrace ())) e

let raise_at loc e =
  raise (wrap loc e)

let raise txt_loc e =
  raise_at (Text txt_loc) e

let reraise_with_stack_trace e =
  raise_at (Raw_backtrace (Printexc.get_raw_backtrace ())) e

(*
   Ideally, this should be in its own file but dune doesn't allow it
   because the library name matches the main module name ('trax')
   and we don't want to rename it for backward compatibility.
*)
module Dedup = struct
(*
   Deduplicate stack traces for better readability
*)

open Printf

  type dedup_entry =
    (* number of entries skipped. 1 entry = multiple slots *)
    | Ellipsis of int
    (* original position, entry *)
    | Entry of int * Printexc.raw_backtrace_entry

  (*
     Skip groups of duplicate entries and replace them by an ellipsis.
  *)
  let dedup_entries entries =
    let already_seen = Hashtbl.create 100 in
    let rec dedup_entries entries acc num_skipped =
      match entries with
      | [] ->
          let acc =
            if num_skipped > 0 then
              Ellipsis num_skipped :: acc
            else
              acc
          in
          List.rev acc
      | (entry_pos, k) :: entries ->
          if Hashtbl.mem already_seen k then
            dedup_entries entries acc (num_skipped + 1)
          else (
            Hashtbl.add already_seen k ();
            let acc =
              if num_skipped > 0 then
                Ellipsis num_skipped :: acc
              else
                acc
            in
            let acc = (Entry (entry_pos, k) :: acc) in
            dedup_entries entries acc 0
          )
    in
    dedup_entries entries [] 0

  let string_of_dedup_entries (l : dedup_entry list) =
    let buf = Buffer.create 1000 in
    l |>
    List.iter (function
      | Ellipsis n ->
          bprintf buf "... (skipping %i duplicate%s)\n"
            n (if n = 1 then "" else "s")
      | Entry (entry_pos, entry) ->
          (match Printexc.backtrace_slots_of_raw_entry entry with
           | None -> ()
           | Some a ->
               Array.iteri (fun slot_pos slot ->
                 (*
                    0 -> print 'Raised by ...'
                    1 -> print 'Called from ...'
                 *)
                 let zero_of_one =
                   if entry_pos = 0 && slot_pos = 0 then 0
                   else 1
                 in
                 match Printexc.Slot.format zero_of_one slot with
                 | None -> ()
                 | Some s -> bprintf buf "%s\n" s
               ) a
          )
    );
    Buffer.contents buf

  let raw_backtrace_to_string raw =
    Printexc.raw_backtrace_entries raw
    |> Array.mapi (fun i x -> (i, x))
    |> Array.to_list
    |> dedup_entries
    |> string_of_dedup_entries
end

let raw_backtrace_to_string = Dedup.raw_backtrace_to_string

let add_loc buf loc =
  match loc with
  | Text s ->
      bprintf buf "\n%s" s
  | Raw_backtrace x ->
      let s = raw_backtrace_to_string x in
      let len = String.length s in
      if len > 0 then
        let n =
          if s.[len-1] = '\n' then len - 1
          else len
        in
        Buffer.add_char buf '\n';
        Buffer.add_substring buf s 0 n

let to_string_aux with_exn e =
  match e with
  | Traced (e, locs) ->
      let buf = Buffer.create 500 in
      if with_exn then
        bprintf buf "%s" (Printexc.to_string e);
      List.iter (add_loc buf) (List.rev locs);
      Buffer.contents buf
  | e ->
      if with_exn then
        Printexc.to_string e
      else
        ""

let to_string e = to_string_aux true e
let get_trace e = to_string_aux false e

let print oc e =
  output_string oc (to_string e)
