

let main bits key_comment =
  let () = Nocrypto_entropy_unix.initialize () in
  let sock_path =
    match Sys.getenv "SSH_AUTH_SOCK" with
    | path -> path
    | exception Not_found -> failwith "$SSH_AUTH_SOCK not set" in
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let () = Unix.connect fd Unix.(ADDR_UNIX sock_path) in
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  let privkey = Ssh_agent.Privkey.Ssh_rsa (Nocrypto.Rsa.generate bits) in
  let req = Ssh_agent.Ssh_agentc_add_identity { privkey; key_comment } in
  match Ssh_agent_unix.request (ic, oc) req with
  | Ok Ssh_agent.Ssh_agent_success ->
    print_endline "Key successfully added!"
  | Ok Ssh_agent.Ssh_agent_failure ->
    print_endline "Ssh-agent reported failure"
  | Error e ->
    print_endline ("Error: " ^ e)

open Cmdliner

let bits =
  let doc = Arg.info ~doc:"Number of bits in the key to create" ["b"; "bits"] in
  Arg.(value & opt int 2048 doc)

let key_comment =
  let doc = Arg.info ~doc:"Key comment"  ["c"] in
  Arg.(value & opt string "" doc)

let () = 
  let term =
    Term.(const main $ bits $ key_comment),
    Term.info "ssh-add" ~version:"0.1" in
  match Term.eval term with
  | `Error _ -> exit 1
  | _ -> exit 0
