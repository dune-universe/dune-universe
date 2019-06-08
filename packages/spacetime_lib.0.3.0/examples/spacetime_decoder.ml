open Spacetime_lib

let compare_entries entry1 entry2 =
  let words1 = Allocation_entry.words entry1 in
  let words2 = Allocation_entry.words entry2 in
  compare words2 words1

let () =
  if Array.length Sys.argv <> 3 then begin
    failwith "Syntax: spacetime_decoder <executable> <pathname of profile>"
  end;
  let series = Series.create ~executable:Sys.argv.(1) Sys.argv.(2) in
  let snapshots = Series.snapshots series in
  let no_of_snapshots = List.length snapshots in
  let has_calls = Series.has_call_counts series in
  let has_calls_str =
    if has_calls then "includes call counts"
    else "does not include call counts"
  in
  Printf.printf "Series has %i snapshots and %s\n" no_of_snapshots has_calls_str;
  List.iteri (fun index snapshot ->
    let time = Snapshot.time snapshot in
    Printf.printf "Snapshot %d (%fs):\n" index time;
    let entries = Snapshot.allocation_entries snapshot in
    let sorted_entries = List.sort compare_entries entries in
    List.iter (fun entry ->
      let blocks = Allocation_entry.blocks entry in
      let words = Allocation_entry.words entry in
      let backtrace = Allocation_entry.backtrace entry in
      Printf.printf "%d words in %d blocks allocated in " words blocks;
      let first = ref true in
      List.iter (fun location ->
        let address = Location.address location in
        let symbol = Location.symbol location in
        let position = Location.position location in
        let foreign = Location.foreign location in
        if !first then begin
          first := false
        end else begin
          Printf.printf "  called from "
        end;
        let symbol_str =
          match symbol with
          | None -> ""
          | Some symbol -> Printf.sprintf " (symbol %s)" symbol
        in
        let origin_str =
          if foreign then "non-OCaml"
          else "OCaml"
        in
        match position with
        | [] ->
          Printf.printf "%s code at 0x%Lx%s\n" origin_str address symbol_str
        | position :: _ ->
          let filename = Position.filename position in
          let line = Position.line_number position in
          let start_char = Position.start_char position in
          let end_char = Position.end_char position in
          Printf.printf "%s source file %s" origin_str filename;
          if not (line = 0 && start_char = 0 && end_char = 0) then
          begin
            Printf.printf ":%d characters %d--%d\n" line
              start_char end_char
          end else begin
            Printf.printf " (exact location unknown)\n"
          end)
        backtrace)
      sorted_entries)
    snapshots;
  Printf.printf "/* vim";  (* split so it doesn't apply to this file! *)
  Printf.printf ": set fdm=indent sw=2: */"
