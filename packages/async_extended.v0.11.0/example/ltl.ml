open Core
open Async
open Async_extended.Std

(* Also see the tech blog post titled "Linear temporal logic for queries and
   monitoring".
*)

module Kind = struct
  type t = [ `Request | `Response ]
  [@@deriving compare, sexp]

  let hash = Hashtbl.hash
end

module State = struct
  type t =
    { kind : Kind.t
    ; time : Time_ns.t
    ; tag : int
    } [@@deriving sexp, fields]

  let to_string t =
    Sexp.to_string (sexp_of_t t)

  let time t = Some (time t)

  let request ~tag ~time =
    { kind = `Request; tag; time = Time.of_span_since_epoch (Time.Span.of_sec (Float.of_int time)) |> Time_ns.of_time }

  let response ~tag ~time =
    { kind = `Response; tag; time = Time.of_span_since_epoch (Time.Span.of_sec (Float.of_int time)) |> Time_ns.of_time }
end

include Ltl.Make (State)

open O

module Fields = struct
  let time = Field.time

  let kind =
    Field.create ~name:"kind"
      ~get:(fun state -> Some state.kind)
      (Hashtbl.Hashable.of_key (module Kind))

  let tag =
    Field.create ~name:"tag"
      ~get:(fun state -> Some state.tag)
      Int.hashable
end

let request =
  field_predicate Fields.kind (fun kind -> kind = `Request)

(* Could be done the same as request, but done differently just to show the more
   general [predicate] primitive. *)
let response =
  predicate ~description:"response"
    (Expression.map
       (Expression.field Fields.kind)
       ~f:(fun kind -> kind = `Response))

(*------------------------------------------
  Sample sequences
--------------------------------------------*)

let bad =
  [ State.request ~tag:1 ~time:1
  ; State.request ~tag:2 ~time:1
  ; State.response ~tag:1 ~time:1
  ]

let good_but_late =
  [ State.request ~tag:1 ~time:1
  ; State.request ~tag:2 ~time:1
  ; State.response ~tag:1 ~time:1
  ; State.response ~tag:2 ~time:1000
  ]

(*------------------------------------------
  Is every request followed by a response?
--------------------------------------------*)

(* This formula is not great - it succeeds on [bad]. *)
let liveness1 =
  always (request ==> eventually response)

let check1 () =
  eval liveness1 (Pipe.of_list bad)
  |> Or_error.ok_exn
  >>= fun result ->
  assert (result = true);
  return ()

(*-------------------------------------------------------------
  Is every request followed by a response with the same tag?
--------------------------------------------------------------*)

let tag = Variable.create "tag" ~sexp_of:Int.sexp_of_t

let liveness2 =
  always ((request && tag == Fields.tag)
          ==> eventually (response && tag == Fields.tag))

(* The set of tag values that satisfy the liveness formula is implicit, but
   the set of tag values that invalidate it is explicit (see [Ltl.validate]).
*)
let check2 () =
  eval (not liveness2) (Pipe.of_list bad)
  |> Or_error.ok_exn
  >>= fun result ->
  assert (result = true);
  return ()

(*----------------------------------------------------------------
  Which requests are not followed by a response within 5 seconds?
------------------------------------------------------------------*)

let time = Variable.create "time" ~sexp_of:Time_ns.sexp_of_t

let sec x =
  sec x |> Time_ns.Span.of_span

let liveness3 =
  always ((request
           && tag == Fields.tag
           && time == Fields.time)
          ==> eventually (response
                          && tag == Fields.tag
                          && before_var ~add:(sec 5.) time))

let check3 () =
  query (not liveness3) (Pipe.of_list good_but_late)
  |> Or_error.ok_exn
  |> Pipe.to_list
  >>= fun results ->
  List.iter results ~f:(fun result ->
    Print.printf "%d\n%!" (Assignment.find_exn result tag)); (* prints 2 *)
  return ()

(*------------------------------------------
  Run
--------------------------------------------*)

let main () =
  check1 () >>= fun () ->
  check2 () >>= fun () ->
  check3 () >>= fun () ->
  Shutdown.shutdown 0;
  return ()

let run () =
  don't_wait_for (main ());
  never_returns (Scheduler.go ());

;;

run ()
