open Core
open Async

type 'a t = {
  mutable data: 'a list;
  mutable new_data: unit Ivar.t;
  msg_to_sexp : ('a -> Sexp.t) option;
}

let sexp_of_t sexp_of_a t =
  [%sexp_of: a list] (List.rev t.data)

module Filter = struct
  type ('a,'b) t = {
    name   : string;
    select : 'a -> 'b option;
  }

  let create name select = { name; select }

  (* To be ignored when composing filter names *)
  let synthetic = "<synthetic>"

  (* compose name, dropping synthetic names *)
  let compose_name ~sep n1 n2 =
    if n1 = synthetic then
      n2
    else if n2 = synthetic then
      n1
    else
      sprintf "(%s %s %s)" n1 sep n2
  ;;

  let arr f = {
    name = synthetic;
    select = fun x -> Some (f x);
  }

  let ( &&& ) f1 f2 =
    { name = compose_name ~sep:"&&&" f1.name f2.name;
      select = fun x ->
        match (f1.select x, f2.select x) with
        | (Some v1, Some v2) -> Some (v1, v2)
        | _ -> None
    }

  let ( >>> ) f1 f2 =
    { name = compose_name ~sep:">>>" f1.name f2.name;
      select = fun x -> Option.find_map (f1.select x) ~f:f2.select}

  let to_predicate t =
    fun x -> Option.is_some (t.select x)

end

let create_gen ?to_sexp iter =
  let t = { data = []; new_data = Ivar.create (); msg_to_sexp = to_sexp } in
  don't_wait_for
    (iter (fun m ->
       (* Be careful when changing this - we expect that at any given point,
          t.data is ordered from newest to oldest. *)
       t.data <- m :: t.data;
       Ivar.fill t.new_data ();
       t.new_data <- Ivar.create ();
       Deferred.unit
     ));
  t

let of_stream ?to_sexp s = create_gen ?to_sexp (fun f -> Stream.iter' s ~f)
let of_pipe   ?to_sexp p = create_gen ?to_sexp (fun f -> Pipe.iter    p ~f)

let create ?to_sexp next =
  let rec iter write =
    next ()
    >>= function
      | None -> Deferred.unit
      | Some a -> write a >>= fun () -> iter write
  in
  create_gen ?to_sexp iter

let new_data t = Ivar.read t.new_data

let describe_in_sexp t =
  match t.msg_to_sexp with
  | Some msg_to_sexp ->
    let msgs = List.sexp_of_t msg_to_sexp t.data in
    Sexp.List [Sexp.Atom "Current messages"; msgs]
  | None ->
    let n = List.length t.data in
    let tip =
      if n > 0 then
        " (create Mailbox with ~to_sexp to get a better error)"
      else
        ""
    in
    Sexp.Atom (sprintf "There are %d messages%S" n tip)

module Timed_out_waiting_for = struct
  type t =
    { filter : string;
      debug : string sexp_option;
      mailbox : Sexp.t;
    } [@@deriving sexp]
end

exception Timed_out_waiting_for of Timed_out_waiting_for.t [@@deriving sexp]

(* There are now two modes of operation:  swallow or not.  swallow means
   that every element prior to the last element found that satisfies our
   postcondition will be thrown away.  This is useful for implementing
   ordering guarantees across successive calls to [receive].

   To accomplish swallow mode, on each loop iteration we check elements
   from newest to oldest, because once we find an element that satisfies
   the filter we can throw away everything preceding it.  Everything before
   that will be replaced in its original order. *)
let receive ?debug ?(timeout = Clock.after (sec 10.)) ?(swallow=false) t
      ~filter:{Filter. name = filter_name; select = filter} ~postcond =
  Deferred.create (fun ivar ->
    let rec loop () =
      let found_one = ref false in
      (* From newest to oldest, check each element.  Once we find something
         that passes the filter, if [swallow] is true then we can
         throw away all further non-passing elemnents. *)
      let res, remains =
        List.fold ~init:([], []) t.data ~f:(fun (res, remains) m ->
          match filter m with
          | None ->
            if swallow && !found_one then
              res, remains
            else
              res, m::remains
          | Some x ->
            found_one := true;
            x::res, remains)
      in
      (* Arrange all of the elements, newest to oldest, in case [postcond] cares
         about the order. *)
      let results = List.rev res in

      if postcond results then begin
        (* This will give us every element that didn't pass the filter, respecting
           [swallow], in the order that they were received. *)
        t.data <- (List.rev remains);
        Ivar.fill ivar results;
      end else begin
        upon
          (choose [
             choice timeout (fun () -> `Timeout);
             choice (new_data t) (fun () -> `Loop);
           ])
          (function
            | `Timeout ->
              raise (
                Timed_out_waiting_for
                  { Timed_out_waiting_for.
                    filter = filter_name;
                    debug;
                    mailbox = describe_in_sexp t;
                  })
            | `Loop ->
              loop ())
      end
    in
    loop ())

module Matched_more_than_expected = struct
  type t =
    { filter : string;
      debug : string sexp_option;
      expected : int;
      received : int;
      mailbox : Sexp.t;
    } [@@deriving sexp]
end

exception Matched_more_than_expected of Matched_more_than_expected.t [@@deriving sexp]

let many ?debug ?timeout ?swallow t n f =
  receive ?debug ?timeout ?swallow t ~filter:f ~postcond:(fun l ->
    let n_received = List.length l in
    if n_received > n then
      raise (
        Matched_more_than_expected
          { Matched_more_than_expected.
            filter = f.Filter.name;
            debug;
            expected = n;
            received = n_received;
            mailbox = describe_in_sexp t;
          })
    else
      n_received = n)

let two ?debug ?timeout ?swallow t f =
  many ?debug ?timeout ?swallow t 2 f
  >>| function
    | [a; b] -> (a, b)
    | _ -> failwith "mailbox.ml bug"

let one ?debug ?timeout ?swallow t f =
  many ?debug ?timeout ?swallow t 1 f >>| List.hd_exn

let not_empty ?debug ?timeout t f =
  receive ?debug ?timeout t ~filter:f ~postcond:(Fn.non List.is_empty)

let peek t {Filter.select = f; _} =
  List.map t.data ~f |> List.filter_map ~f:Fn.id |> List.rev

let filter t {Filter.select = f; _} =
  t.data <- List.filter t.data ~f:(fun x -> Option.is_none (f x))

let zero ?(debug = "") t f =
  match peek t f with
  | [] -> ()
  | _ -> failwithf
           !"Mailbox: Not Zero. Filter '%s'. Debug: '%s'.\n%{Sexp}"
           f.Filter.name
           debug
           (describe_in_sexp t)
           ()

let clear ?(to_remove=const true) t =
  t.data <- List.filter t.data ~f:(Fn.compose not to_remove)

let check_clear t =
  if List.is_empty t.data then
    Ok ()
  else
    Or_error.error "Unconsumed msgs" (describe_in_sexp t) Fn.id

let%test_module _ = (module struct

  module Dummy = struct
    type nonrec 'a t =
      { mailbox : 'a t;
        writer : 'a Pipe.Writer.t;
        mutable ctr : int;
      }

    let write t amt =
      let rec aux n =
        if n > amt then
          Deferred.unit
        else
          Pipe.write t.writer (t.ctr + n)
          >>= fun () -> aux (n+1)
      in
      aux 1
      >>| fun () ->
      t.ctr <- t.ctr + amt

    let create () =
      let reader, writer = Pipe.create () in
      { mailbox = of_pipe reader;
        writer;
        ctr = 0;
      }
  end


  let filter n =
    { Filter.
      name = sprintf "Next: %d" n;
      select = fun x -> if x = n then Some x else None;
    }

  let loop ~swallow n =
    let dummy = Dummy.create () in
    let rec aux ct =
      if ct = n then begin
        Or_error.ok_exn (check_clear dummy.mailbox);
        Deferred.unit
      end else begin
        Dummy.write dummy 1
        >>= fun () ->
        one ~swallow dummy.mailbox (filter ct)
        >>= fun _ ->
        aux (ct+1)
      end
    in
    aux 1

  let%test_unit _ =
    Thread_safe.block_on_async_exn (fun () -> loop 100 ~swallow:true)

  let%test_unit _ =
    Thread_safe.block_on_async_exn (fun () -> loop 100 ~swallow:false)

  let%test_unit _ =
    Thread_safe.block_on_async_exn (fun () ->
      let dummy = Dummy.create () in
      Dummy.write dummy 3
      >>= fun () ->
      many ~swallow:true dummy.mailbox 0 (filter 4)
      >>= fun _ ->
      many ~swallow:false dummy.mailbox 0 (filter 4)
      >>= fun _ ->
      one ~swallow:false dummy.mailbox (filter 3)
      >>= fun n ->
      assert (n = 3);
      one ~swallow:true  dummy.mailbox (filter 2)
      >>= fun n ->
      assert (n = 2);
      Monitor.try_with (fun () -> one ~swallow:true dummy.mailbox (filter 1))
      >>| fun res ->
      match res with
      | Ok _ -> assert false
      | _ -> Or_error.ok_exn (check_clear dummy.mailbox))

end)
