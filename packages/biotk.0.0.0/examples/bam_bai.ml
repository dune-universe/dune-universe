open Core_kernel
open Biocaml_unix
open Biotk
open Rresult



let bai = R.get_ok @@ Bai.read (In_channel.create "data/range.bam.bai")
let ba2 = R.get_ok @@ Bai.read (In_channel.create "rien.bai")

let f () =
  let bam_ic = Bgzf.open_in "data/range.bam" in
  let _ = (ok_exn @@ Bam.read_header bam_ic : Bam.Header.t) in
  for i = 1 to 112 do
    printf "%Ld %d\n" (Bgzf.virtual_offset bam_ic) i;
    match Bam.read_alignment bam_ic with
    | Some _ -> ()
    | None -> failwith ""
  done ;
  bam_ic


(* let virtual_offset =
 *   bai.reference_sequences.(1).bins.(1).Bai.chunks.(0).Bai.chunk_beg
 *
 * let bam_ic = Bgzf.open_in "data/range.bam"
 *
 * let x =
 *   Bgzf.seek_in bam_ic virtual_offset ;
 *   ok_exn @@ Option.value_exn (Bam.read_alignment bam_ic) *)
