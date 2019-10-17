open BatPervasives
module List = BatList

type menu_item = {
  descr : string;
  action : unit -> unit
}

module Server_pool = struct
  include Resource_pooling.Server_pool.Make (struct
    type connection = string
    type server = string
    type serverid = string
    let serverid_to_string = identity
    let connect c = Lwt.return c
    let close _ = Lwt.return_unit
    let check_delay = 3.0
    let check_server _serverid _server = Lwt.return_true
  end)

  let show_servers : server_status list -> string = fun l ->
    let header =
      Array.of_list @@ List.map (fun s -> `Text s)
        ["name"; "desired"; "current"; "essential"; "suspended"]
    in
    let array_of_server = function
        {serverid; desired; current; essential; suspended; _} ->
      Array.of_list @@ List.map (fun s -> `Text s)
      [serverid; string_of_int desired; string_of_int current;
       string_of_bool essential; string_of_bool suspended]
    in
    PrintBox_text.to_string @@
      PrintBox.Simple.to_box @@
        `Table (Array.of_list @@ header :: List.map array_of_server l)

  let print_servers () =
    print_endline @@ show_servers @@ server_statuses ()
end

let () =
  Lwt_log.Section.set_level Resource_pooling.Server_pool.section Lwt_log.Debug

let num_conn = 2
let replica_num = ref 1

module Actions = struct
  let add_replica () =
    let serverid = Printf.sprintf "rr%d" !replica_num in
    replica_num := !replica_num + 1;
    Server_pool.add_one ~connect_immediately:true ~num_conn serverid serverid
  let use_pool ?exc () =
    Lwt.async @@ fun () ->
    let num_exceptions = ref 1 in
    try%lwt
      Server_pool.use @@ fun serverid ->
        Printf.printf "using server %s\n%!" serverid;
        let%lwt () = Lwt_unix.sleep 3.0 in
        let%lwt () = match exc with
          | None -> Lwt.return_unit
          | Some e -> if !num_exceptions > 0 then begin
              num_exceptions := !num_exceptions - 1;
              Lwt.fail e
          end
          else Lwt.return_unit
        in
        Printf.printf "used server %s\n%!" serverid;
        Lwt.return_unit
    with e ->
      print_endline @@ Printexc.to_string e;
      Lwt.return_unit
end

let resource_invalid safe =
  Resource_pooling.Resource_pool.Resource_invalid {safe}

let rec print_status () =
  Server_pool.print_servers ();
  let print_menu_item i (descr, _) =
    if i > 0 then print_string "  ";
    Printf.printf "%d) %s%!" (i + 1) descr
  in
  List.iteri print_menu_item menu;
  print_endline ""
and menu = [
  "print status",
    print_status;
  "add replica",
    Actions.add_replica;
  "use pool",
    Actions.use_pool ?exc:None;
  "use pool (exception, retry)",
    Actions.use_pool ~exc:(resource_invalid true);
  "use pool (exception, retry)",
    Actions.use_pool ~exc:(resource_invalid false)
]

let execute_action c =
  let i = int_of_char c - 48 in
  if i >= 0 then
    match List.at_opt menu (i - 1) with
    | None -> print_endline "!!!!!!!!!!!!!!"
    | Some item -> snd item ()

let rec print_loop () =
  print_status ();
  let%lwt () = Lwt_unix.sleep 1.0 in
  print_loop ()

let input_loop () =
  let chars = Lwt_io.read_chars Lwt_io.stdin in
  let rec loop () =
    let%lwt c = Lwt_stream.last_new chars in
    execute_action c;
    loop ()
  in loop ()

let main () =
  print_status ();
  input_loop ()

let () = Lwt_main.run @@ main ()
