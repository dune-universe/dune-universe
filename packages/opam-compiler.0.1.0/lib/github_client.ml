open! Import

type pr_info = { source_branch : Branch.t; title : string }

type t = { pr_info : Pull_request.t -> (pr_info, [ `Unknown ]) result }

let pr_info t = t.pr_info

module Real = struct
  let pull_source_user branch =
    match branch.Github_t.branch_user with
    | Some user -> Ok user.user_login
    | None -> Error `Unknown

  let pull_source_repo branch =
    match branch.Github_t.branch_repo with
    | Some repo -> Ok repo.repository_name
    | None -> Error `Unknown

  let get_pr { Pull_request.user; repo; number } =
    let open Github.Monad in
    Github.Pull.get ~user ~repo ~num:number () >|= Github.Response.value

  let pr_info pr =
    let open Rresult.R in
    let open Let_syntax.Result in
    let* { Github_t.pull_head; pull_title = title; _ } =
      get_pr pr |> Github.Monad.run |> Lwt_result.catch |> Lwt_main.run
      |> reword_error (fun (_ : exn) -> `Unknown)
    in
    let* user = pull_source_user pull_head in
    let+ repo = pull_source_repo pull_head in
    let branch = pull_head.branch_ref in
    let source_branch = { Branch.user; repo; branch } in
    { source_branch; title }
end

let real =
  let open Real in
  { pr_info }
