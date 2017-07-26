open Core
open Async

module Durable = struct
  type 'a t =
  | Void
  | Building of 'a Deferred.Or_error.t
  | Built of 'a
end

type 'a t =
  { mutable durable : 'a Durable.t
  ; to_create : (unit -> 'a Deferred.Or_error.t)
  ; is_broken : ('a -> bool)
  ; to_rebuild : ('a -> 'a Deferred.Or_error.t) option
  }

let create ~to_create ~is_broken ?to_rebuild () =
  { durable = Durable.Void
  ; to_create
  ; is_broken
  ; to_rebuild
  }
;;

let create_or_fail ~to_create ~is_broken ?to_rebuild () =
  let t = create ~to_create ~is_broken ?to_rebuild () in
  t.to_create ()
  >>=? fun dur ->
  if t.is_broken dur
  then return (Or_error.error_string "Initial durable value is broken.")
  else
    begin
      t.durable <- Durable.Built dur;
      return (Ok t)
  end
;;

let get_durable t =
  let build building =
    let building =
      building >>| fun result ->
      assert (match t.durable with Building _ -> true | _ -> false);
      t.durable <-
        begin match result with
        (* Errors that show up here will also be returned by [get_durable]. We aren't
           losing any information *)
        | Error _    -> Durable.Void
        | Ok durable -> Durable.Built durable
        end;
      result
    in
    t.durable <- Durable.Building building;
    building
  in
  match t.durable with
  | Durable.Void -> build (t.to_create ())
  | Durable.Building durable -> durable
  | Durable.Built durable ->
    if t.is_broken durable
    then build (match t.to_rebuild with
      | None            -> t.to_create ()
      | Some to_rebuild -> to_rebuild durable)
    else return (Ok durable)
;;

let with_ t ~f =
  get_durable t
  >>=? fun durable ->
  if t.is_broken durable
  then
    return (Or_error.error_string
      "Durable value was broken immediately after being created or rebuilt.")
  else f durable
;;

let%test_module _ = (module struct

  let go () = Async_kernel_private.Scheduler.run_cycles_until_no_jobs_remain ()

  let create_counter = ref 0
  let fix_counter = ref 0

  module Fragile = struct
    type t = { mutable is_broken : bool }
    let is_broken t = t.is_broken
    let break t = t.is_broken <- true

    let create_ () =
      return (Ok { is_broken = false })
    ;;

    let create () =
      create_counter := !create_counter + 1;
      create_ ()
    ;;

    let fix _t =
      fix_counter := !fix_counter + 1;
      create_ ()
    ;;
  end

  let reset () =
    create_counter := 0;
    fix_counter := 0
  ;;

  let create ~use_fix ~now =
    let to_rebuild = if use_fix then Some Fragile.fix else None in
    if now
    then
      create_or_fail ~to_create:Fragile.create ~is_broken:Fragile.is_broken ?to_rebuild ()
      >>| Or_error.ok_exn
    else
      return (
        create ~to_create:Fragile.create ~is_broken:Fragile.is_broken ?to_rebuild ())
  ;;

  let poke t = ignore (with_ t ~f:(fun _t -> return (Ok ())))

  let%test_unit _ =
    let pass = ref false in
    begin
      create ~use_fix:false ~now:true
      >>> fun t ->
      match t.durable with
      | Durable.Built _ -> pass := true
      | _ -> ()
    end;
    go ();
    assert !pass
  ;;

  let build_break_poke ~use_fix ~now =
    reset ();
    begin
      create ~use_fix ~now
      >>> fun t ->
      with_ t ~f:(fun fragile ->
        Fragile.break fragile;
        return (Ok ()))
      >>> fun result ->
      Or_error.ok_exn result;
      poke t;
      poke t;
      poke t
    end;
    go ()
  ;;

  let%test_unit _ =
    build_break_poke ~use_fix:true ~now:true;
    assert (!create_counter = 1);
    assert (!fix_counter = 1)
  ;;

  let%test_unit _ =
    build_break_poke ~use_fix:true ~now:false;
    assert (!create_counter = 1);
    assert (!fix_counter = 1)
  ;;

  let%test_unit _ =
    build_break_poke ~use_fix:false ~now:true;
    assert (!create_counter = 2);
    assert (!fix_counter = 0)
  ;;

  let%test_unit _ =
    build_break_poke ~use_fix:false ~now:false;
    assert (!create_counter = 2);
    assert (!fix_counter = 0)
  ;;

end)
