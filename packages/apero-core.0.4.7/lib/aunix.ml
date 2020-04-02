let inet_addrs () = 
  let rec recurse inchan l f = 
    try 
      String.(List.(
        let words = input_line inchan |> trim |> split_on_char ' ' in
        match words with 
        | _::flags::"mtu"::_ -> flags |> split_on_char '<' |> tl |> hd |>
                                split_on_char '>' |> hd |> split_on_char ',' |>
                                recurse inchan l
        | _::_::flags::"mtu"::_ -> flags |> split_on_char '<' |> tl |> hd |>
                                   split_on_char '>' |> hd |> split_on_char ',' |>
                                   recurse inchan l
        | "inet"::addr::_ -> let addr = addr |> split_on_char '/' |> hd in 
                             recurse inchan ((Unix.inet_addr_of_string addr, f)::l) f
        | _ -> recurse inchan l f
      ))
    with _ -> l
  in
  let stdout_chan, stdin_chan, stderr_chan  = Unix.open_process_full "ip a" (Unix.environment ()) in
  match recurse stdout_chan [] [] with 
  | [] -> (try let _ = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan) in () with _ -> ());
          let stdout_chan, stdin_chan, stderr_chan  = Unix.open_process_full "ifconfig" (Unix.environment ()) in
          let result = recurse stdout_chan [] [] in 
          (try let _ = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan) in () with _ -> ()); result
  | ls -> (try let _ = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan) in () with _ -> ()); ls

let inet_addrs_up_nolo = 
  Acommon.Infix.(String.(List.(
    inet_addrs 
    %> filter (fun (_, f) -> 
            (exists (equal "UP") f, exists (equal "LOOPBACK") f) |> function 
            | true, false -> true
            | _ -> false)
    %> map (fun (addr, _) -> addr)
  )))
