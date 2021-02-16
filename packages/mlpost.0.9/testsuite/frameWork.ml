module Assert = struct
  exception Assert of string option

  let assert_failure s = raise (Assert s)

  let bool ?s cond = if cond then () else assert_failure s

  let eq ?s a b = if a = b then () else assert_failure s

  module F = File

  module File = struct
    let exists ?s f = bool ?s (File.exists f)

    let eq ?s ?ignore a b =
      let ignore = match ignore with None -> "" | Some s -> "-I " ^ s in
      let a = File.to_string a and b = File.to_string b in
      let r =
        Misc.call_cmd ~outv:true (Misc.sprintf "diff %s %s %s" ignore a b)
      in
      eq ?s r 0
  end

  module List = struct
    let non_empty l = bool (l <> [])
  end
end

module Test = struct
  let _ = Printexc.record_backtrace true

  let id_unit () = ()

  type t = {
    prepare : unit -> unit;
    run : unit -> unit;
    clean_up : unit -> unit;
    name : string;
  }

  let mk ?(prepare = id_unit) ?(clean_up = id_unit) ~name run =
    { prepare; clean_up; run; name }

  let run_one queue t =
    t.prepare ();
    ( try
        t.run ();
        Format.printf ".@?"
      with
    | Assert.Assert s ->
        Format.printf "!@?";
        queue := (t, s) :: !queue
    | e ->
        Format.printf "?@?";
        Format.eprintf "Error during test %s...@." t.name;
        Format.eprintf "%s@." (Printexc.to_string e);
        Printexc.print_backtrace Stdlib.stderr );
    try t.clean_up ()
    with e ->
      Format.eprintf "Error during cleanup of test %s...@." t.name;
      Format.eprintf "%s@." (Printexc.to_string e);
      Printexc.print_backtrace Stdlib.stderr;
      exit 1

  let run_many l =
    let queue = ref [] in
    List.iter (run_one queue) l;
    Format.printf "@.";
    let l = List.rev !queue in
    List.iter
      (fun (t, ass) ->
        Format.printf "failed test %s:@." t.name;
        match ass with
        | None -> ()
        | Some s -> Format.printf "assertion failed: %s@." s)
      l
end
