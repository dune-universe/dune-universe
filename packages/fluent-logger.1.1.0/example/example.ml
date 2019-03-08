let () =
  Printexc.record_backtrace true;
  let module L = Fluent_logger in
  let logger = 
    if Array.length Sys.argv > 1 then
      (* use 'path' parameter for unix domain socket *)
      L.create_for_unix Sys.argv.(1)
    else
      (* inet domain socket *)
      L.create ()
  in
  let rec loop n i f = if i < n then (f i; loop n (i + 1) f) in
  loop 240 0 (fun i ->
    let result =
      L.post logger "production" (
        `FixMap [
          (L.of_string "id", L.uint8_of_int i);
          (L.of_string "name", L.of_string "foobar");
          (L.of_string "age", L.uint8_of_int 81);
          (L.of_string "pi", L.of_float 3.14)
        ]
      ) in
    if not result then prerr_endline "logger: failed to post";
    Unix.sleep 1
  );
  L.release logger
