(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)


(* let _ = *)
(*   Format.printf "counters: %d\ncomponents: %d\n%!" *)
(*     (Papi.counters ()) (Papi.components ()) *)


let (%) f g x = f (g x)
let pr fmt = Format.printf fmt

let events = Papi.[TOT_CYC; L1_DCM; L1_ICM; L1_TCM; L2_DCM; L2_ICM; L2_TCM]

let trap f = try f () with Papi.Error (e, n) as exn ->
  Format.printf "%s: %a\n%!" n Papi.pp_error e; raise exn

let _ = Papi.init ()

let create ?events:(es = []) () =
  let open Papi in
  let set = create () in
  match List.partition query es with
    ([], _) ->
      pr "* No events available.";
      cleanup set; destroy set;
      None
  | (es1, es2) ->
      List.iter (add set) es1;
      List.iter (pr "+ Added event: %s\n%!" % name) es1;
      List.iter (pr "* Event unavailable: %s\n%!" % name) es2;
      Some set

let _ = trap @@ fun () ->
  match create ~events () with
    Some set ->
      let res = Array.create_float (Papi.num_events set) in
      for i = 0 to Papi.num_events set - 1 do res.(i) <- 0. done;
      Papi.start set;
      for _ = 0 to 100 do
        Papi.read set res;
        Fmt.(pr "%a\n%!" (Dump.array float) res)
      done;
      Papi.(stop set; cleanup set; destroy set)
  | None -> ()
