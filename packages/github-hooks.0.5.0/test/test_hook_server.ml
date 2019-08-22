open Printf
open Lwt.Infix

let src = Logs.Src.create "test_hook_server"
let () = Logs.Src.set_level src (Some Logs.Debug)
let () = Logs.(set_reporter (format_reporter ()))

module Hooks = Github_hooks_unix.Make(struct
    module Log = (val Logs.src_log src : Logs.LOG)

    let secret_prefix = "test_hook_server_"
    let tls_config = Some (fun port ->
      (`Crt_file_path "webhook.crt",
       `Key_file_path "webhook.key",
       `No_password,
       `Port port)
    )
  end)

let add_collaborator user repo collaborator =
  Github.Collaborator.add ~user ~repo ~name:collaborator ()

let check_status msg = function
  | Lwt_unix.WEXITED 0 -> Lwt.return_unit
  | _ -> Lwt.fail (Failure ("non-zero exit code process termination: "^msg))

let git_clone user repo =
  Lwt_unix.system
    (Printf.sprintf "git clone git@github.com:%s/%s.git" user repo)
  >>= check_status "git clone"

let remove_clone _user repo =
  Lwt_unix.system (Printf.sprintf "rm -rf \"./%s\"" repo)
  >>= check_status "rm -rf"

let git_push _user repo path content = Lwt.Infix.(
  Lwt_unix.(openfile (repo^"/"^path) [O_WRONLY; O_CREAT] 0o600)
  >>= fun fd ->
  Lwt.catch (fun () ->
    Lwt_unix.write_string fd content 0 (String.length content)
    >>= fun _written ->
    Lwt_unix.chdir repo
    >>= fun () ->
    Lwt.catch (fun () ->
      Lwt_unix.system ("git add " ^ path)
      >>= check_status "git add"
      >>= fun () ->
      Lwt_unix.system "git commit -m test_push"
      >>= check_status "git commit"
      >>= fun () ->
      Lwt_unix.system "git push origin master"
      >>= check_status "git push"
      >>= fun () ->
      Lwt_unix.chdir ".."
    ) (function
      | exn ->
        Lwt_unix.chdir ".."
        >>= fun () ->
        Lwt.fail exn
    )
  ) (function
    | exn ->
      Lwt_unix.close fd
      >>= fun () ->
      Lwt.fail exn
  )
)

let create_issue user repo title content = Github.Monad.(
  let issue = {
    Github_t.new_issue_title = title;
    new_issue_body = Some content;
    new_issue_assignee = None;
    new_issue_milestone = None;
    new_issue_labels = [];
  } in
  Github.Issue.create ~user ~repo ~issue ()
  >>~ fun issue -> return issue.Github_t.issue_number
)

let close_issue user repo num = Github.Monad.(
  let issue = {
    Github_t.update_issue_title = None;
    update_issue_state = Some `Closed;
    update_issue_body = None;
    update_issue_assignee = None;
    update_issue_milestone = None;
    update_issue_labels = None;
  } in
  Github.Issue.update ~user ~repo ~num ~issue ()
  >>= fun _ -> return ()
)

let delete_repo user repo =
  Github.Repo.delete ~user ~repo ()

type event_check =
  | CreateRepo
  | CreateBranch of string
  | AddMember
  | Push of string
  | OpenIssue of int
  | CloseIssue of int

let expected_events = [
  CreateRepo;
  AddMember;
  CreateBranch "test_push";
  Push "test_push";
  OpenIssue 1;
  CloseIssue 1;
]

let expected_hook_events = List.(rev (fold_left (fun acc -> function
  | CreateRepo -> acc
  | CreateBranch commit -> (Push commit)::(CreateBranch commit)::acc
  | (AddMember | Push _ | OpenIssue _ | CloseIssue _) as event -> event::acc
) [] expected_events))

module type CHECK_EVENT = sig
  type push

  val create_repo : [> `Create of Github_t.create_event ] -> unit
  val create_branch : [> `Create of Github_t.create_event ] -> unit
  val add_member : [> `Member of Github_t.member_event ] -> unit
  val push : string -> [> `Push of push ] -> unit
  val open_issue : int -> [> `Issues of Github_t.issues_event ] -> unit
  val close_issue : int -> [> `Issues of Github_t.issues_event ] -> unit
end

module Check_generic_event = struct
  open Github_t

  let create_repo = function
    | `Create { create_event_ref = `Repository; _ } -> ()
    | _ -> failwith "event stream missing create repository"

  let create_branch = function
    | `Create { create_event_ref = `Branch _; _ } -> ()
    | _ -> failwith "event stream missing create branch"

  let add_member = function
    | `Member { member_event_action = `Added; _ } -> ()
    | _ -> failwith "event stream missing add member"

  let open_issue num = function
    | `Issues { issues_event_action = `Opened;
                issues_event_issue = { issue_number; _ }; _
              } when issue_number = num -> ()
    | _ -> failwith "event stream missing open issue"

  let close_issue num = function
    | `Issues { issues_event_action = `Closed;
                issues_event_issue = { issue_number; _ }; _
              } when issue_number = num -> ()
    | _ -> failwith "event stream missing close issue"

end

module Check_poll_event
  : CHECK_EVENT with type push = Github_t.push_event = struct
  type push = Github_t.push_event

  include Check_generic_event
  open Github_t

  let push commit = function
    | `Push { push_event_commits = [
        { push_event_commit_message; _ }
      ]; _ } when push_event_commit_message = commit -> ()
    | _ -> failwith "event stream missing push"
end

module Check_hook_event
  : CHECK_EVENT with type push = Github_t.push_event_hook = struct
  type push = Github_t.push_event_hook

  include Check_generic_event
  open Github_t

  let push commit = function
    | `Push { push_event_hook_commits = [
        { push_event_hook_commit_message; _ }
      ]; _ } when push_event_hook_commit_message = commit -> ()
    | _ -> failwith "event stream missing push"
end

let check_events
    (type push) check_mod expected (events : [> `Push of push ] list) =
  let module Check_event =
    (val check_mod : CHECK_EVENT with type push = push)
  in
  List.iter2 (function
    | CreateRepo -> Check_event.create_repo
    | CreateBranch _ -> Check_event.create_branch
    | AddMember -> Check_event.add_member
    | Push commit -> Check_event.push commit
    | OpenIssue num -> Check_event.open_issue num
    | CloseIssue num -> Check_event.close_issue num
  ) expected events

let perform_test_actions user repo collaborator = Github.Monad.(
  add_collaborator user repo collaborator
  >>~ fun () ->
  embed (git_clone user repo)
  >>= fun () ->
  embed (git_push user repo "a_pushed_file" "Push that paper.")
  >>= fun () ->
  embed (git_push user repo "another_pushed_file" "bits and atoms")
  >>= fun () ->
  (* GitHub's push event delivery is not strictly ordered wrt issue events *)
  embed (Lwt_unix.sleep 2.)
  >>= fun () ->
  create_issue user repo "An issue"
    "There is an issue. It is serious. I won't tell you what it is."
  >>= fun issue_number ->
  close_issue user repo issue_number

  >>= fun () ->
  (* GitHub's issue events can be slow to propagate *)
  embed (Lwt_unix.sleep 2.)
  >>= fun () ->
  Github.Stream.to_list @@ Github.Event.for_repo ~user ~repo ()
  >>= fun event_stream ->
  print_endline "\nPolled events:\n";
  List.iter (fun ev ->
    print_endline (Github_j.string_of_event ev)
  ) event_stream;
  check_events (module Check_poll_event) expected_events
    (List.rev_map (fun ev ->
       ev.Github_t.event_payload
     ) event_stream);
  return ()

  >>= fun () ->
  delete_repo user repo
  >>~ fun () ->
  embed (remove_clone user repo)
)

let rec wait_for_events ~timeout server =
  let open Lwt.Infix in
  let current_events = Hooks.events server in
  let event_count = List.length current_events in
  let k = List.length expected_hook_events in
  if event_count < k
  then if timeout = 0
    then begin
      Printf.eprintf "Timed out waiting for hook events\n%!";
      List.iter (fun ev ->
        print_endline (Github_j.string_of_event_hook_constr (snd ev))
      ) current_events;
      Lwt.return []
    end
    else begin
      Printf.eprintf "Collected %d / %d events (%ds remaining)\n%!"
        event_count k timeout;
      Lwt_unix.sleep 1.
      >>= fun () ->
      wait_for_events ~timeout:(timeout - 1) server
    end
  else Lwt.return current_events

let check_hook_events received =
  print_endline "\nReceived events:\n";
  List.iter (fun ev ->
    print_endline (Github_j.string_of_event_hook_constr ev)
  ) received;
  check_events (module Check_hook_event) expected_hook_events received;
  []

let main () =
  if Array.length Sys.argv < 4
  then failwith
      (sprintf "usage: %s server_uri user repo collaborator" Sys.argv.(0))
  else
    let uri = Sys.argv.(1) in
    let user = Sys.argv.(2) in
    let repo = Sys.argv.(3) in
    let collaborator = Sys.argv.(4) in
    Github_cookie_jar.init ()
    >>= fun jar ->
    Github_cookie_jar.get jar ~name:"test"
    >>= function
    | None ->
      failwith "could not find cookie 'test' in git jar"
    | Some auth ->
      let token = Github.Token.of_auth auth in
      let uri = Uri.of_string uri in
      let server = Hooks.create token uri in
      Github.(Monad.(run (
        Github.API.set_token token
        >>= fun () ->
        Repo.create ~repo:{
          Github_t.new_repo_name = repo;
          new_repo_homepage = "";
          new_repo_description = "A web hook test repo";
          new_repo_private = false;
          new_repo_has_issues = true;
          new_repo_has_wiki = true;
          new_repo_has_downloads = true;
          new_repo_team_id = 0;
          new_repo_auto_init = false;
          new_repo_license_template = None;
          new_repo_gitignore_template = None;
        } ()
        >>= fun _test_repo ->
        embed (Hooks.watch server ~events:[`All] (user, repo))
        >>= fun () ->
        Lwt.async (fun () -> Hooks.run server);
        perform_test_actions user repo collaborator
        >>= fun () ->
        embed (wait_for_events ~timeout:10 server)
        >>= fun events ->
        match check_hook_events (List.map snd events) with
        | [] -> embed (Lwt_io.printf "Everything passed.\n")
        | messages ->
          embed (
            Lwt_io.printf "There were some problems:\n%s"
              (String.concat "\n" messages)
          )
      )))
;;

Lwt_main.run (main ())
