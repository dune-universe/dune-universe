let debug fmt = Format.ksprintf (fun str -> Logs_lwt.debug @@ fun m -> m "%s" str) fmt
let info fmt = Format.ksprintf (fun str -> Logs_lwt.info @@ fun m -> m "%s" str) fmt
let error fmt = Format.ksprintf (fun str -> Logs_lwt.err @@ fun m -> m "%s" str) fmt
let verbose = ref false
let trace list =
  if !verbose
  then info "%s" (Sexplib.Sexp.to_string (Shared_block.S.sexp_of_traced_operation_list list))
  else Lwt.return ()
