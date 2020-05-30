open Plebeia.Internal
open Utils.Open
(* open Node *)

module Entry = Roots

let () =
  let path = Sys.argv.(1) in
  let vc = Vc.open_ ~mode:Storage.Reader path in
  let roots = Vc.roots vc in (* XXX silly to load all *)

  let last_root = 
    from_Some @@
    Seq.fold_left (fun x root ->
        let y = match x with None -> Index.of_int 0 | Some r -> r.Entry.index in
        if y < root.Entry.index then Some root
        else x) None @@ Roots.to_seq roots 
  in
  
  Format.eprintf "last root index= %d@." @@ Index.to_int last_root.Entry.index;

  let segs = Hashtbl.create 0 in

  let c = from_Some @@ Vc.checkout vc last_root.Entry.hash in
  let extenders = 
    Cursor.fold ~init:0 c (fun extenders c ->
        match Cursor.view c with
        | _, Node.Extender (seg, _, _, _) ->
            begin match Hashtbl.find_opt segs seg with
              | None -> Hashtbl.add segs seg 1
              | Some n -> Hashtbl.replace segs seg (n+1)
            end;
            `Continue, extenders+1
        | _ -> `Continue, extenders)
  in
  Format.eprintf "extenders=%d@." extenders;
  let a = Array.init (28*8-2+1) (fun _ -> (0,0)) in
  Hashtbl.iter (fun k v ->
      let len = Segment.length k in
      if len > 28*8-2 then begin Format.eprintf "seglen=%d@." len; assert false; end;
      let (m,n) = Array.unsafe_get a len in
      Array.unsafe_set a len (m+1,n+v)) segs;
  Format.eprintf "--- extender statistics@.";
  Format.eprintf "length, distinct, total, dupe_ratio@.";
  for len = 0 to 28 * 8 - 2 do
    let (m,n) = Array.unsafe_get a len in
    Format.eprintf "%d, %d, %d, %.2f@." len m n (if m = 0 then 1.0 else float n /. float m)
  done
    
      

