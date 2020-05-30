open Plebeia.Internal

let () = 
  let tempfile = Filename.temp_file "plebeia" ".context" in
  Unix.unlink tempfile; (* tempfile is created *)

  let _lock1 = Lock.lock tempfile in
  prerr_endline "locked";

  match Unix.fork () with
  | 0 -> 
      begin try
        let _lock2 = Lock.lock tempfile in
        prerr_endline "locked again";
        exit 2
      with
      | Unix.Unix_error (Unix.EAGAIN, "lockf", _) -> 
          prerr_endline "properly locked";
          exit 0
      end
  | _ ->
      match Unix.wait () with
      | _, WEXITED 0 -> ()
      | _, WEXITED _ -> assert false
      | _ -> assert false

        
