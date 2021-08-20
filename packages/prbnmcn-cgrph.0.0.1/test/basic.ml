open Cgraph

let internal = ref None

let snapshot graph =
  internal := Some (ex graph) ;
  graph

module Bind_shallow_remove_dyn = struct
  let v = Var.create 0

  let graph = bind (var v) @@ fun i -> snapshot @@ return (i + 1)

  let bind_uid = Internal.uid (ex graph)

  let () = Var.set v 0

  let () = assert (!internal = None)

  let () = assert (get graph = 1)

  let () = assert (Option.is_some !internal)

  let prev = Option.get !internal

  let prev_id = Internal.uid prev

  let () =
    assert (
      match (Internal.used_by prev, Internal.upstream prev) with
      | ([n], []) -> Internal.uid n = bind_uid
      | _ -> false)

  let () = Var.set v 1

  let () = assert (get graph = 2)

  let () = assert (Option.is_some !internal)

  let new_ = Option.get !internal

  let new_id = Internal.uid new_

  let () = assert (Internal.used_by prev = [] && Internal.upstream prev = [])

  let () =
    assert (
      match (Internal.used_by new_, Internal.upstream new_) with
      | ([n], []) -> Internal.uid n = bind_uid
      | _ -> false)

  let () = assert (prev_id < new_id)
end

module Bind_static_body = struct
  let v = Var.create 0

  let ret = return 0

  let ret_id = Internal.uid (ex ret)

  let () = internal := None

  let graph = bind (var v) @@ fun _i -> snapshot ret

  let () = assert (!internal = None)

  let () = Var.set v 0

  let () = assert (get graph = 0)

  let () = assert (Option.is_some !internal)

  let prev_id = Internal.uid @@ Option.get !internal

  let () = Var.set v 1

  let () = assert (get graph = 0)

  let () = assert (Option.is_some !internal)

  let new_id = Internal.uid @@ Option.get !internal

  let () = assert (prev_id = new_id)
end

module Bind_deep_remove_dyn = struct
  let v = Var.create 0

  let s = Var.create 0

  let () = internal := None

  let graph =
    bind (var v) @@ fun i ->
    snapshot @@ map (map (var s) (fun j -> i + j)) (fun k -> k)

  let bind_uid = Internal.uid (ex graph)

  let () = Var.set v 0

  let () = Var.set s 0

  let () = assert (!internal = None)

  let () = assert (get graph = 0)

  let () = assert (Option.is_some !internal)

  let prev = Option.get !internal

  let prev_id = Internal.uid prev

  let () = Var.set v 1

  let () = Var.set s 1

  let () = assert (!internal <> None)

  let () = assert (get graph = 2)

  let () = assert (Option.is_some !internal)
end

module Bind_deep_nested_remove_dyn = struct
  let v = Var.create 0

  let s = Var.create 0

  let t = Var.create 0

  let graph =
    bind (var v) @@ fun i ->
    bind (var s) @@ fun j ->
    bind (var t) @@ fun k -> return (i + j + k)

  let () = Var.set v 0

  let () = Var.set s 0

  let () = Var.set t 0

  (* let () = Format.printf "step 1@." *)

  (* let () =
   *   let oc = open_out "/tmp/graph1.dot" in
   *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

  let () = assert (get graph = 0)

  (* let () = Format.printf "got 0@." *)

  (* let () =
   *   let oc = open_out "/tmp/graph2.dot" in
   *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

  let () = Var.set t 1

  (* let () = Format.printf "step 2@." *)

  (* let () =
   *   let oc = open_out "/tmp/graph3.dot" in
   *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

  let () = assert (get graph = 1)

  (* let () = Format.printf "got 1@." *)

  (* let () =
   *   let oc = open_out "/tmp/graph4.dot" in
   *   Internal.(to_dot ~mode:Full (ex graph) oc) *)
end
