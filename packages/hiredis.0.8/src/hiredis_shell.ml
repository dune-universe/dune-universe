type config = string * string list

module Shell = struct

    module Server = struct
        type t = int

        let null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o0655

        let write_config ?temp_dir cf =
            let name, oc = Filename.open_temp_file ?temp_dir "redis" ".conf" in
            List.iter (fun (name, args) ->
                output_string oc name;
                output_char oc ' ';
                output_string oc (String.concat " " (List.map (fun x ->
                    if String.contains x ' ' then
                        "\"" ^ x ^ "\""
                    else x) args));
                output_char oc '\n') cf;
            close_out oc; name

        let start ?temp_dir ?config:(config=[]) port =
            Unix.create_process "redis-server"
                [| "redis-server"; write_config config;
                    "--daemonize"; "no";
                    "--port"; string_of_int port;
                |] null null null

        let stop x = Unix.kill x Sys.sigkill
    end (* End Server *)

    module Client = struct
        let interactive ?host:(host="127.0.0.1") port =
            try
                ignore (Unix.create_process "redis-cli"
                    [| "redis-cli";
                        "-p"; string_of_int port;
                        "-h"; host;
                    |] Unix.stdin Unix.stdout Unix.stderr
                |> Unix.waitpid [])
            with _ -> ()
    end (* End Client *)

end (* End Shell *)
