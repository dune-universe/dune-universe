
open Core
open Async

type t = {
  name: string;
  username: string;
  host: string;
}

let create name username host = {
  name = name;
  username = username;
  host = host
}

let name t = t.name
let username t = t.username
let host t = t.host

let ssh_command t command =
  let ssh_command =
    sprintf "ssh %s@%s %s"
      (Filename.quote t.username)
      (Filename.quote t.host)
      (Filename.quote command)
  in
  Monitor.try_with ~rest:`Raise (fun () -> Unix.system ssh_command) >>| function
    | Error _ -> false
    | Ok status -> Result.is_ok status
;;

let running t = ssh_command t (sprintf "~/bin/check_%s" t.username)
let start t = ssh_command t (sprintf "~/bin/start_%s" t.username) >>| ignore
let shutdown t = ssh_command t (sprintf "~/bin/stop_%s" t.username) >>| ignore
let kill t = ssh_command t (sprintf "~/bin/kill_%s" t.username) >>| ignore
