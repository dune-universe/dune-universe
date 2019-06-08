(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Raw_spacetime_lib

type context =
  { executable : Elf_locations.t option;
    frame_table : Frame_table.t;
    shape_table : Shape_table.t; }

module Position = struct

  type t = Printexc.location

  let filename { Printexc.filename; _ } = filename

  let line_number { Printexc.line_number; _ } = line_number

  let start_char { Printexc.start_char; _ } = start_char

  let end_char { Printexc.end_char; _ } = end_char

  let print ppf t =
    let open Printexc in
    Format.fprintf ppf "%s{%d:%d-%d}"
      t.filename t.line_number t.start_char t.end_char

end

module Location = struct

  type t =
    { address: Int64.t;
      symbol: string option;
      position : Position.t list;
      foreign : bool; }

  let address { address; _ } = address

  let symbol { symbol; _ } = symbol

  let position { position; _ } = position

  let foreign { foreign; _ } = foreign

  let create_ocaml ~ctx pc =
    let address = Program_counter.OCaml.to_int64 pc in
    let foreign = false in
    let symbol =
      match ctx.executable with
      | None -> None
      | Some elf_locations ->
        Elf_locations.function_at_pc elf_locations ~program_counter:address
    in
    let position =
      try
        let slots = Frame_table.find_exn pc ctx.frame_table in
        List.fold_right (fun slot acc ->
            match Printexc.Slot.location slot with
            | Some location -> location::acc
            | None -> acc)
          slots
          []
      with Not_found -> []
    in
    { address; symbol; position; foreign; }

  let create_foreign ~ctx pc =
    let program_counter = Program_counter.Foreign.to_int64 pc in
    let position =
      match ctx.executable with
      | None -> []
      | Some elf_locations ->
        match Elf_locations.resolve elf_locations ~program_counter with
        | None -> []
        | Some (filename, line_number) ->
          let location =
            { Printexc.
              filename;
              line_number;
              start_char = -1;
              end_char = -1;
            }
          in
          [location]
    in
    let symbol =
      match ctx.executable with
      | None -> None
      | Some elf_locations ->
        Elf_locations.function_at_pc elf_locations ~program_counter
    in
    { address = program_counter;
      symbol;
      position;
      foreign = true;
    }

  let print ppf t =
    match t.position with
    | [] ->
      begin match t.symbol with
      | Some symbol -> Format.fprintf ppf "%s" symbol
      | None -> Format.fprintf ppf "%Ld" t.address
      end
    | locations ->
      Format.fprintf ppf "%a" (Format.pp_print_list Position.print) locations

end

module Backtrace = struct

  type t = Location.t list

  let rec print ppf = function
    | [] -> ()
    | [loc] -> Location.print ppf loc
    | loc :: res ->
      Format.fprintf ppf "%a %a"
        Location.print loc
        print res

end

module Small_count : sig

  type t

  val max_allocations : int

  val max_words : int

  val max_blocks : int

  val create : allocations:int -> words:int -> blocks:int -> t

  val zeros : t

  val allocations : t -> int

  val words : t -> int

  val blocks : t -> int

end = struct

  type t = int

  let allocations_size = Sys.int_size / 2
  let words_size = (Sys.int_size - allocations_size) / 2
  let blocks_size = Sys.int_size - allocations_size - words_size

  let max_allocations = ((1 lsl allocations_size) - 1)
  let max_words = ((1 lsl words_size) - 1)
  let max_blocks = ((1 lsl blocks_size) - 1)

  let blocks_shift = 0
  let words_shift = blocks_shift + blocks_size
  let allocations_shift = words_shift + words_size

  let allocations_mask = max_allocations lsl allocations_shift
  let words_mask = max_words lsl words_shift
  let blocks_mask = max_blocks lsl blocks_shift

  let create ~allocations ~words ~blocks =
    allocations lsl allocations_shift
    lor words lsl words_shift
    lor blocks lsl blocks_shift

  let zeros = 0

  let allocations t =
    (t land allocations_mask) lsr allocations_shift

  let words t =
    (t land words_mask) lsr words_shift

  let blocks t =
    (t land blocks_mask) lsr blocks_shift

end

module Allocation_entry = struct

  type t =
    | Alloc of { backtrace : Backtrace.t;
                 allocations : int; }
    | Small of { backtrace : Backtrace.t;
                 counts : Small_count.t; }
    | Large of { backtrace : Backtrace.t;
                 blocks : int;
                 words : int;
                 allocations : int; }

  let backtrace = function
    | Alloc { backtrace; _ } -> backtrace
    | Small { backtrace; _ } -> backtrace
    | Large { backtrace; _ } -> backtrace

  let blocks = function
    | Alloc _ -> 0
    | Small { counts; _ } -> Small_count.blocks counts
    | Large { blocks; _ } -> blocks

  let words = function
    | Alloc _ -> 0
    | Small { counts; _ } -> Small_count.words counts
    | Large { words; _ } -> words

  let allocations = function
    | Alloc { allocations; _ } -> allocations
    | Small { counts; _ } -> Small_count.allocations counts
    | Large { allocations; _ } -> allocations

  let with_allocations t new_allocations =
    match t with
    | Alloc { backtrace; allocations } as entry ->
      if allocations = new_allocations then entry
      else Alloc { backtrace; allocations = new_allocations }
    | Small { backtrace; counts } as entry ->
      let allocations = Small_count.allocations counts in
      if allocations = new_allocations then entry
      else begin
        let blocks = Small_count.blocks counts in
        let words = Small_count.words counts in
        if allocations <= Small_count.max_allocations then begin
          let new_counts =
            Small_count.create ~blocks ~words ~allocations:new_allocations
          in
          Small { backtrace; counts = new_counts }
        end else begin
          Large { backtrace; blocks; words; allocations }
        end
      end
    | Large { backtrace; blocks; words; allocations } as entry ->
      if allocations = new_allocations then entry
      else Large { backtrace; blocks; words; allocations = new_allocations }

end

module Call_entry = struct

  type t =
    | Direct of { backtrace : Backtrace.t;
                  calls : int; }
    | Indirect of { backtrace : Backtrace.t;
                    calls : int; }

  let backtrace = function
    | Indirect { backtrace; _ } -> backtrace
    | Direct { backtrace; _ } -> backtrace

  let calls = function
    | Indirect { calls; _ } -> calls
    | Direct { calls; _ } -> calls

  let direct = function
    | Indirect _ -> false
    | Direct _ -> true

end

module Annotation_data = struct

  type updates =
    | Nil
    | UpdateAlloc of
        { index: int;
          allocations : int;
          next: updates }
    | UpdateSmall of
        { index: int;
          counts : Small_count.t;
          next: updates }
    | UpdateLarge of
        { index: int;
          blocks : int;
          words : int;
          allocations : int;
          next: updates }

  type t =
    { mutable updates: updates;
      mutable last: int; }

  let create () =
    { updates = Nil;
      last = -1; }

  let finish_updates t ~upto =
    if t.last < upto then begin
      let rec loop ~last ~prev_blocks ~prev_words = function
        | Nil ->
            if prev_blocks = 0 && prev_words = 0 then raise Exit
            else begin
              UpdateSmall { index = last + 1; counts = Small_count.zeros;
                            next = Nil; }
            end
        | UpdateAlloc _ -> assert false
        | UpdateSmall { index; counts; next } ->
            let blocks = Small_count.blocks counts in
            let words = Small_count.words counts in
            let next = loop ~last ~prev_blocks:blocks ~prev_words:words next in
            UpdateSmall { index; counts; next }
        | UpdateLarge { index; blocks; words; allocations; next } ->
            let next = loop ~last ~prev_blocks:blocks ~prev_words:words next in
            UpdateLarge { index; blocks; words; allocations; next }
      in
      let updates =
        try
          loop ~last:t.last ~prev_blocks:0 ~prev_words:0 t.updates
        with Exit -> t.updates
      in
      t.updates <- updates;
      t.last <- upto
    end

  let set_blocks_and_words t idx ~blocks ~words =
    let rec loop ~idx ~new_blocks ~new_words
              ~prev_blocks ~prev_words = function
      | Nil ->
        if new_blocks = prev_blocks && new_words = prev_words then begin
          raise Exit
        end else begin
          if new_blocks <= Small_count.max_blocks
             && new_words <= Small_count.max_words then begin
            let new_counts =
              Small_count.create
                ~blocks:new_blocks ~words:new_words ~allocations:0
            in
            UpdateSmall { index = idx; counts = new_counts; next = Nil }
          end else begin
            UpdateLarge { index = idx; blocks = new_blocks; words = new_words;
                          allocations = 0; next = Nil }
          end
        end
      | UpdateAlloc _ -> assert false
      | UpdateSmall { index; counts; next } ->
          assert (index < idx);
          let blocks = Small_count.blocks counts in
          let words = Small_count.words counts in
          let next =
            loop ~idx ~new_blocks ~new_words
              ~prev_blocks:blocks ~prev_words:words next
          in
          UpdateSmall { index; counts; next }
      | UpdateLarge { index; blocks; words; allocations = _; next } ->
          assert (index < idx);
          let next =
            loop ~idx ~new_blocks ~new_words
              ~prev_blocks:blocks ~prev_words:words next
          in
          UpdateLarge { index; blocks; words; allocations = 0; next }
    in
    finish_updates t ~upto:(idx - 1);
    t.last <- idx;
    match loop ~idx ~new_blocks:blocks ~new_words:words
            ~prev_blocks:0 ~prev_words:0 t.updates with
    | updates -> t.updates <- updates
    | exception Exit -> ()

  let set_allocations t idx ~num_snapshots ~allocations =
    let rec loop ~idx ~new_allocations ~prev_allocations = function
      | Nil ->
        if new_allocations = prev_allocations then begin
          raise Exit
        end else begin
          UpdateAlloc { index = idx;
                        allocations = new_allocations; next = Nil }
        end
      | UpdateAlloc { index; allocations; next } ->
        assert (index < idx);
        let next =
          loop ~idx ~new_allocations ~prev_allocations:allocations next
        in
        UpdateAlloc { index; allocations; next }
      | UpdateSmall { index; counts; next } as data ->
        if index = idx then begin
          let blocks = Small_count.blocks counts in
          let words = Small_count.words counts in
          if new_allocations <= Small_count.max_allocations then begin
            let new_counts =
              Small_count.create ~blocks ~words ~allocations:new_allocations
            in
            UpdateSmall { index = idx; counts = new_counts; next }
          end else begin
            UpdateLarge { index = idx; blocks; words;
                          allocations = new_allocations; next }
          end
        end else if index < idx then begin
          let allocations = Small_count.allocations counts in
          let next =
            loop ~idx ~new_allocations ~prev_allocations:allocations next
          in
          UpdateSmall { index; counts; next }
        end else if new_allocations = prev_allocations then begin
          raise Exit
        end else begin
          UpdateAlloc { index = idx;allocations = new_allocations;
                        next = data }
        end
      | UpdateLarge { index; blocks; words; allocations; next } as data ->
        if index = idx then begin
          UpdateLarge { index = idx; blocks; words;
                        allocations = new_allocations; next }
        end else if index < idx then begin
          let next =
            loop ~idx ~new_allocations ~prev_allocations:allocations next
          in
          UpdateLarge { index; blocks; words; allocations; next }
        end else if new_allocations = prev_allocations then begin
          raise Exit
        end else begin
          UpdateAlloc { index = idx; allocations = new_allocations;
                        next = data }
        end
    in
    finish_updates t ~upto:(num_snapshots - 1);
    match loop ~idx ~new_allocations:allocations
            ~prev_allocations:0 t.updates with
    | updates -> t.updates <- updates
    | exception Exit -> ()

  let store_entries ~backtrace ~data ~entries =
    let rec loop ~entries ~idx ~prev_entry data =
      if idx >= Array.length entries then ()
      else begin
        match data with
        | Nil ->
          entries.(idx) <- prev_entry :: entries.(idx);
          loop ~entries ~idx:(idx + 1) ~prev_entry data
        | UpdateAlloc { index; allocations; next } ->
          if index <= idx then begin
            let entry =
              Allocation_entry.with_allocations prev_entry allocations
            in
            loop ~entries ~idx ~prev_entry:entry next
          end else begin
            entries.(idx) <- prev_entry :: entries.(idx);
            loop ~entries ~idx:(idx + 1) ~prev_entry data
          end
        | UpdateSmall { index; counts; next } ->
          if index <= idx then begin
            let backtrace = Allocation_entry.backtrace prev_entry in
            let entry = Allocation_entry.Small { backtrace; counts } in
            loop ~entries ~idx ~prev_entry:entry next
          end else begin
            entries.(idx) <- prev_entry :: entries.(idx);
            loop ~entries ~idx:(idx + 1) ~prev_entry data
          end
        | UpdateLarge { index; blocks; words; allocations; next } ->
          if index <= idx then begin
            let backtrace = Allocation_entry.backtrace prev_entry in
            let entry =
              Allocation_entry.Large { backtrace; blocks; words; allocations }
            in
            loop ~entries ~idx ~prev_entry:entry next
          end else begin
            entries.(idx) <- prev_entry :: entries.(idx);
            loop ~entries ~idx:(idx + 1) ~prev_entry data
          end
      end
    in
    match data.updates with
    | Nil -> ()
    | UpdateAlloc { index; allocations; next } ->
      let entry = Allocation_entry.Alloc { backtrace; allocations } in
      loop ~entries ~idx:index ~prev_entry:entry next
    | UpdateSmall { index; counts; next } ->
      let entry = Allocation_entry.Small { backtrace; counts } in
      loop ~entries ~idx:index ~prev_entry:entry next
    | UpdateLarge { index; blocks; words; allocations; next } ->
      let entry =
        Allocation_entry.Large { backtrace; blocks; words; allocations }
      in
      loop ~entries ~idx:index ~prev_entry:entry next

end

module Stats = struct

  type t =
    { gc: Gc_stats.t;
      words_scanned : int;
      words_scanned_with_profinfo : int; }

  let minor_words t = Gc_stats.minor_words t.gc
  let promoted_words t = Gc_stats.promoted_words t.gc
  let major_words t = Gc_stats.major_words t.gc
  let minor_collections t = Gc_stats.minor_collections t.gc
  let major_collections t = Gc_stats.major_collections t.gc
  let heap_words t = Gc_stats.heap_words t.gc
  let heap_chunks t = Gc_stats.heap_chunks t.gc
  let compactions t = Gc_stats.compactions t.gc
  let top_heap_words t = Gc_stats.top_heap_words t.gc

  let words_scanned { words_scanned; _ } = words_scanned

  let words_scanned_with_profinfo { words_scanned_with_profinfo; _ } =
    words_scanned_with_profinfo

end

module Snapshot = struct

  type t =
    { time : float;
      stats : Stats.t option;
      allocation_entries : Allocation_entry.t list;
    }

  let time { time; _ } = time

  let stats { stats; _ } = stats

  let allocation_entries { allocation_entries; _ } = allocation_entries

  let create ~snapshot ~allocation_entries =
    let time, stats =
      match snapshot with
      | None -> 0.0, None
      | Some snapshot ->
        let time = Heap_snapshot.timestamp snapshot in
        let gc = Heap_snapshot.gc_stats snapshot in
        let words_scanned = Heap_snapshot.words_scanned snapshot in
        let words_scanned_with_profinfo =
          Heap_snapshot.words_scanned_with_profinfo snapshot
        in
        let stats =
          { Stats.gc; words_scanned; words_scanned_with_profinfo; }
        in
        time, Some stats
    in
    { time; stats; allocation_entries; }

end

module Series = struct

  type t =
    { snapshots : Snapshot.t list;
      call_entries : Call_entry.t list;
      has_call_counts : bool;
      final_time : float; }

  type 'a entry_kind =
    | Allocation : Annotation.t entry_kind
    | Direct_call : int entry_kind
    | Indirect_call : int entry_kind

  type iterator =
    { f: 'a. 'a entry_kind -> Backtrace.t -> 'a -> unit }

  let iter_opt f opt k =
    match opt with
    | None -> k ()
    | Some x -> f x k

  let rec iter_ocaml_indirect_calls ~ctx ~visited f backtrace callee k =
    let next = Trace.OCaml.Indirect_call_point.Callee.next callee in
    let k =
      match next with
      | None -> k
      | Some next ->
        fun () ->
          iter_ocaml_indirect_calls ~ctx ~visited f backtrace next k
    in
    let node = Trace.OCaml.Indirect_call_point.Callee.callee_node callee in
    let count = Versioned.indirect_call_count callee in
    let () =
      match count with
      | None -> ()
      | Some count -> f.f Indirect_call backtrace count
    in
    iter_node ~ctx ~visited f backtrace node k

  and iter_ocaml_field_classification ~ctx
        ~visited f backtrace classification k =
    match classification with
    | Trace.OCaml.Field.Allocation alloc ->
      let pc = Trace.OCaml.Allocation_point.program_counter alloc in
      let loc = Location.create_ocaml ~ctx pc in
      let annot = Trace.OCaml.Allocation_point.annotation alloc in
      f.f Allocation (loc :: backtrace) annot;
      k ()
    | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_ocaml call) ->
      let node = Trace.OCaml.Direct_call_point.callee_node call in
      let count = Versioned.direct_call_count call in
      let site = Trace.OCaml.Direct_call_point.call_site call in
      let loc = Location.create_ocaml ~ctx site in
      let backtrace = loc :: backtrace in
      let () =
        match count with
        | None -> ()
        | Some count -> f.f Direct_call backtrace count
      in
      iter_ocaml_node ~ctx ~visited f backtrace node k
    | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_foreign call) ->
      let site = Trace.OCaml.Direct_call_point.call_site call in
      let node = Trace.OCaml.Direct_call_point.callee_node call in
      let loc = Location.create_ocaml ~ctx site in
      let backtrace = loc :: backtrace in
      let count = Versioned.direct_call_count call in
      let () =
        match count with
        | None -> ()
        | Some count -> f.f Direct_call backtrace count
      in
      iter_foreign_node ~ctx ~visited f backtrace node k
    | Trace.OCaml.Field.Direct_call
        (Trace.OCaml.Field.To_uninstrumented _) -> k ()
    | Trace.OCaml.Field.Indirect_call call ->
      let site = Trace.OCaml.Indirect_call_point.call_site call in
      let callee = Trace.OCaml.Indirect_call_point.callees call in
      let loc = Location.create_ocaml ~ctx site in
      let backtrace = loc :: backtrace in
      iter_opt
        (iter_ocaml_indirect_calls ~ctx ~visited f backtrace)
        callee k

  and iter_ocaml_fields ~ctx ~visited f backtrace field k =
    let next = Trace.OCaml.Field.next field in
    let k =
      match next with
      | None -> k
      | Some next ->
        fun () ->
          iter_ocaml_fields ~ctx ~visited f backtrace next k
    in
    iter_ocaml_field_classification ~ctx ~visited
      f backtrace (Trace.OCaml.Field.classify field) k

  and iter_ocaml_node ~ctx ~visited f backtrace node k =
    if Trace.Node.Set.mem (Trace.Node.of_ocaml_node node) !visited then k ()
    else begin
      visited := Trace.Node.Set.add (Trace.Node.of_ocaml_node node) !visited;
      let shape_table = ctx.shape_table in
      let fields = Trace.OCaml.Node.fields node ~shape_table in
      let k =
        match fields with
        | None -> k
        | Some fields ->
          fun () ->
            iter_ocaml_fields ~ctx ~visited
              f backtrace fields k
      in
      iter_ocaml_node ~ctx ~visited f backtrace
        (Trace.OCaml.Node.next_in_tail_call_chain node) k
    end

  and iter_foreign_field_classification ~ctx
        ~visited f backtrace classification k =
    match classification with
    | Trace.Foreign.Field.Allocation alloc ->
      let pc = Trace.Foreign.Allocation_point.program_counter alloc in
      let loc = Location.create_foreign ~ctx pc in
      let annot = Trace.Foreign.Allocation_point.annotation alloc in
      f.f Allocation (loc :: backtrace) annot;
      k ()
    | Trace.Foreign.Field.Call call ->
      let site = Trace.Foreign.Call_point.call_site call in
      let loc = Location.create_foreign ~ctx site in
      let node = Trace.Foreign.Call_point.callee_node call in
      iter_node ~ctx ~visited f (loc :: backtrace) node k

  and iter_foreign_fields ~ctx ~visited f backtrace field k =
    let next = Trace.Foreign.Field.next field in
    let k =
      match next with
      | None -> k
      | Some next ->
        fun () ->
          iter_foreign_fields ~ctx ~visited f backtrace next k
    in
    iter_foreign_field_classification ~ctx
      ~visited f backtrace (Trace.Foreign.Field.classify field) k

  and iter_foreign_node ~ctx ~visited f backtrace node k =
    iter_opt
      (iter_foreign_fields ~ctx ~visited f backtrace)
      (Trace.Foreign.Node.fields node) k

  and iter_node ~ctx ~visited f backtrace node k =
    match Trace.Node.classify node with
    | Trace.Node.OCaml node ->
      iter_ocaml_node ~ctx ~visited f
        backtrace node k
    | Trace.Node.Foreign node ->
      iter_foreign_node ~ctx ~visited f backtrace node k

  let iter_trace ~ctx f trace k =
    match Trace.root trace with
    | None -> ()
    | Some node ->
      let visited = ref Trace.Node.Set.empty in
      iter_node ~ctx ~visited f [] node k

  let iter_traces ?executable ~series ~frame_table ~shape_table f =
    let ctx = { executable; frame_table; shape_table } in
    let num_threads = Heap_snapshot.Series.num_threads series in
    let rec loop n =
      if n >= num_threads then ()
      else begin
        let normal =
          Heap_snapshot.Series.trace series
            ~kind:Heap_snapshot.Series.Normal ~thread_index:n
        in
        iter_opt (iter_trace ~ctx f) normal ignore;
        let finaliser =
          Heap_snapshot.Series.trace series
            ~kind:Heap_snapshot.Series.Finaliser ~thread_index:n
        in
        iter_opt (iter_trace ~ctx f) finaliser ignore;
        loop (n + 1)
      end
    in
    loop 0

  let data_table ~num_snapshots ~snapshots =
    let tbl = Hashtbl.create 42 in
    List.iteri
      (fun idx snapshot ->
         let entries = Heap_snapshot.entries snapshot in
         let length = Heap_snapshot.Entries.length entries in
         for entry = 0 to length - 1 do
           let blocks = Heap_snapshot.Entries.num_blocks entries entry in
           let words =
             Heap_snapshot.Entries.num_words_including_headers entries entry
           in
           let annotation =
             Heap_snapshot.Entries.annotation entries entry
           in
           let data =
             match Hashtbl.find tbl annotation with
             | data -> data
             | exception Not_found ->
               let data = Annotation_data.create () in
               Hashtbl.add tbl annotation data;
               data
           in
           Annotation_data.set_blocks_and_words data idx ~blocks ~words
         done)
      snapshots;
    List.iteri
      (fun idx snapshot ->
         let rec loop next =
           match next with
           | None -> ()
           | Some allocation ->
             let allocations =
               Heap_snapshot.Total_allocation.num_words_including_headers
                 allocation
             in
             let annotation =
               Heap_snapshot.Total_allocation.annotation allocation
             in
             let data =
               match Hashtbl.find tbl annotation with
               | data -> data
               | exception Not_found ->
                 let data = Annotation_data.create () in
                 Hashtbl.add tbl annotation data;
                 data
             in
             Annotation_data.set_allocations
               data idx ~num_snapshots ~allocations;
             let next = Heap_snapshot.Total_allocation.next allocation in
             loop next
         in
         loop (Heap_snapshot.total_allocations snapshot))
      snapshots;
    tbl

  let create ?executable path =
    let series = Heap_snapshot.Series.read ~path in
    let has_call_counts = Versioned.has_call_counts series in
    let final_time = Heap_snapshot.Series.time_of_writer_close series in
    let executable =
      match executable with
      | None -> None
      | Some executable ->
        Some (Elf_locations.create ~elf_executable:executable)
    in
    let frame_table = Heap_snapshot.Series.frame_table series in
    let shape_table = Heap_snapshot.Series.shape_table series in
    let length = Heap_snapshot.Series.num_snapshots series in
    let snapshots =
      let rec loop acc n =
        if n < 0 then acc
        else begin
          let snapshot = Heap_snapshot.Series.snapshot series ~index:n in
          loop (snapshot :: acc) (n - 1)
        end
      in
      loop [] (length - 1)
    in
    let num_snapshots = List.length snapshots in
    let data_table = data_table ~num_snapshots ~snapshots in
    let allocation_entries = Array.make num_snapshots [] in
    let call_entries = ref [] in
    let accumulate (type a) (kind : a entry_kind) backtrace (annot : a) =
      match kind with
      | Allocation -> begin
          match Hashtbl.find data_table annot with
          | exception Not_found -> ()
          | data ->
            Annotation_data.store_entries ~backtrace ~data
              ~entries:allocation_entries
        end
      | Direct_call -> begin
          let calls = annot in
          let entry = Call_entry.Direct { backtrace; calls } in
          call_entries := entry :: !call_entries
        end
      | Indirect_call -> begin
          let calls = annot in
          let entry = Call_entry.Indirect { backtrace; calls } in
          call_entries := entry :: !call_entries
        end
    in
    iter_traces ?executable ~series ~frame_table ~shape_table
      {f = accumulate};
    let snapshots =
      List.map2 (fun snapshot allocation_entries ->
          Snapshot.create ~snapshot:(Some snapshot) ~allocation_entries)
        snapshots (Array.to_list allocation_entries)
    in
    let call_entries = !call_entries in
    { snapshots; call_entries; has_call_counts; final_time }

  let snapshots { snapshots; _ } = snapshots

  let call_entries { call_entries; _ } = call_entries

  let has_call_counts { has_call_counts; _ } = has_call_counts

  let final_time { final_time; _ } = final_time

end
