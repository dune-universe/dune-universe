
(* that should actually all be size_t *)
type mallinfo = {
  arena : int; (* non-mmapped space allocated from system *)
  ordblks : int;  (* number of free chunks *)
  (*smblks; /* always 0 */
    hblks; /* always 0 */ *)
  hblkhd : int; (* space in mmapped regions *)
  usmblks : int; (* maximum total allocated space *)
  (* fsmblks;  /* always 0 */ *)
  uordblks : int; (* total allocated space *)
  fordblks : int; (* total free space *)
  keepcost : int; (* releasable (via malloc_trim) space *)
}

external mallinfo : unit -> mallinfo = "stub_mallinfo"

let malloc_metrics ~tags =
  let open Metrics in
  let doc = "Malloc counters" in
  let data () =
    let stat = mallinfo () in
    Data.v
      [ uint "non-mmapped allocated bytes" stat.arena
      ; uint "free chunk count" stat.ordblks
      ; uint "mmapped region count" stat.hblkhd
      ; uint "maximum allocated space" stat.usmblks
      ; uint "total allocated space" stat.uordblks
      ; uint "total free space" stat.fordblks
      ; uint "maximum releasable bytes" stat.keepcost ]
  in
  Src.v ~doc ~tags ~data "malloc"
