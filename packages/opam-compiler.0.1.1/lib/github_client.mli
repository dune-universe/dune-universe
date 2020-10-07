type pr_info = { source_branch : Branch.t; title : string }

type t = { pr_info : Pull_request.t -> (pr_info, [ `Unknown ]) result }

val pr_info : t -> Pull_request.t -> (pr_info, [ `Unknown ]) result

val real : t
