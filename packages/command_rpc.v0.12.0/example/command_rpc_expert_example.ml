open Core
open Async

type state = { mutable sum : int }

module Send_summand = struct
  type query    = int  [@@deriving bin_io, sexp]
  type response = unit [@@deriving bin_io, sexp]
  type nonrec state = state

  let rpc =
    Rpc.Rpc.create ~name:"send-summand" ~version:1 ~bin_query ~bin_response

  let implementation state query =
    state.sum <- state.sum + query;
    return ()
end

module Get_sum = struct
  type query    = unit [@@deriving bin_io, sexp]
  type response = int  [@@deriving bin_io, sexp]
  type nonrec state = state

  let rpc =
    Rpc.Rpc.create ~name:"get-sum" ~version:1 ~bin_query ~bin_response

  let implementation state () =
    return state.sum
end

let rpcs : state Command_rpc.Command.Stateful.t list =
  [ `Plain (module Send_summand)
  ; `Plain (module Get_sum)
  ]

let callee_command =
  let open Command.Let_syntax in
  Command.async ~summary:"rpc interface"
    [%map_open
      let main = Command_rpc.Command.Expert.param ()
      in fun () ->
        let state = { sum = 0 } in
        let rpcs =
          List.map rpcs ~f:(fun rpc ->
            Command_rpc.Command.Stateful.lift rpc
              ~f:(fun (_ : Command_rpc.Command.Invocation.t) -> state))
        in
        main (Command_rpc.Command.stateful rpcs)
    ]

let caller_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"calculate the sum of A and B"
    [%map_open
      let a = anon ("A" %: int)
      and b = anon ("B" %: int)
      in fun () ->
        let open Deferred.Or_error.Let_syntax in
        Command_rpc.Connection.with_close ~prog:(Sys.argv.(0)) ~args:["callee"]
          (fun connection ->
             let%bind () = Rpc.Rpc.dispatch Send_summand.rpc connection a in
             let%bind () = Rpc.Rpc.dispatch Send_summand.rpc connection b in
             let%bind sum = Rpc.Rpc.dispatch Get_sum.rpc connection () in
             printf "%d\n" sum;
             return ())
    ]

let command =
  Command.group ~summary:"command_rpc expert demo"
    [ "callee", callee_command
    ; "caller", caller_command
    ]

let () = Command.run command
