open Core_kernel

module Make (Incr : Incremental.S_gen) = struct
  module E = Incr.Expert

  (* Hashtbl starts out at a ridiculous size of 128. This is a more reasonable number
     of bins to have. *)
  let hashtbl_size = 10

  (** [setup_generator] sets up the staged conversion.

      [hashable] is used to build a table of necessary dependencies. We drop unnecessary
      dependencies to allow them to be collected.

      [compute_output key] returns the value of the output node corresponding to [key].
      [compute_output] is closed over the mutable state determining the current value of
      the selector's input.

      [make_input_node] makes sure that when the mutable state inside [compute_output] is
      updated, the corresponding nodes in the [necessary_dependencies] table are made
      stale.  *)
  let setup_generator
        (hashable : 'a Hashtbl_intf.Hashable.t)
        ~(compute_output : 'a -> 'b)
        ~(make_input_node : make_key_stale:('a -> unit) -> unit Incr.t)
    : ('a -> 'b Incr.t) Staged.t
    =
    let necessary_dependencies = Hashtbl.Using_hashable.create ~size:hashtbl_size ~hashable () in
    let (input_node : unit Incr.t) =
      let make_key_stale key =
        Hashtbl.find necessary_dependencies key
        |> Option.value ~default:[]
        |> List.iter ~f:E.Node.make_stale
      in
      make_input_node ~make_key_stale
    in
    (* Set the cutoff so that we never pass automatically from the update to the nodes. We
       want everything to go through [make_key_stale] *)
    Incr.set_cutoff input_node Incr.Cutoff.always;
    stage (fun key ->
      let input_dep = E.Dependency.create input_node in
      let rec output_node = lazy (
        E.Node.create
          (fun () -> compute_output key)
          ~on_observability_change:(fun ~is_now_observable ->
            if is_now_observable then
              Hashtbl.add_multi necessary_dependencies ~key ~data:(force output_node)
            else
              Hashtbl.change necessary_dependencies key ~f:(function
                | None -> None
                | Some l ->
                  match List.filter l ~f:(Fn.non (phys_equal (force output_node))) with
                  | [] -> None
                  | l' -> Some l'
              )))
      in
      let output_node = force output_node in
      E.Node.add_dependency output_node input_dep;
      E.Node.watch output_node
    )

  (** This creates a unit incremental that fires whenever the input incremental
      fires. When that occurs, it updates [selected] to match the current value of
      [input], and calls [make_key_stale] for both the old and new value of the
      incremental. *)
  let update_one ~input ~selected ~make_stale =
    Incr.map input ~f:(fun inp ->
      Option.iter inp ~f:make_stale;
      Option.iter !selected ~f:make_stale;
      selected := inp
    )

  let select_one'
        (type a)
        (module H : Hashable.Common with type t = a)
        (input : a option Incr.t)
    =
    let selected = ref None in
    let compute_output key =
      match !selected with
      | None -> false
      | Some key' -> H.compare key key' = 0
    in
    let make_input_node ~make_key_stale =
      update_one ~input ~selected ~make_stale:make_key_stale
    in
    setup_generator H.hashable ~compute_output ~make_input_node

  let select_one h input =
    select_one' h (Incr.map ~f:Option.some input)

  let select_one_value'
        (type a)
        (module H : Hashable.Common with type t = a)
        ~default
        input
    =
    let selected = ref None in
    let compute_output key =
      match !selected with
      | None -> default
      | Some (key', data) -> if H.compare key key' = 0 then data else default
    in
    let make_input_node ~make_key_stale =
      update_one ~selected ~input ~make_stale:(fun (key, _) -> make_key_stale key)
    in
    setup_generator H.hashable ~compute_output ~make_input_node

  let select_one_value h ~default input =
    select_one_value' h ~default (Incr.map ~f:Option.some input)

  let select_many_values
        (type a)
        (module H : Hashable.Common with type t = a)
        ~default
        input
    =
    let hashable = H.hashable in
    let selected = Hashtbl.Using_hashable.create ~size:hashtbl_size ~hashable () in
    let compute_output key =
      Hashtbl.find selected key |> Option.value ~default
    in
    let make_input_node ~make_key_stale =
      Incr.map input ~f:(fun inp ->
        Hashtbl.iter_keys selected ~f:make_key_stale;
        Hashtbl.clear selected;
        List.iter inp ~f:(fun (key, data) ->
          make_key_stale key;
          Hashtbl.set selected ~key ~data
        ))
    in
    setup_generator hashable ~compute_output ~make_input_node

  let select_many
        (type a)
        (module H : Hashable.Common with type t = a)
        input
    =
    let hashable = H.hashable in
    let selected = Hash_set.Using_hashable.create ~size:hashtbl_size ~hashable () in
    let compute_output key = Hash_set.mem selected key in
    let make_input_node ~make_key_stale =
      Incr.map input ~f:(fun inp ->
        let old_set = Hash_set.copy selected in
        Hash_set.clear selected;

        List.iter inp ~f:(fun key ->
          if not (Hash_set.mem old_set key) then
            make_key_stale key;
          Hash_set.add selected key
        );

        Hash_set.iter old_set ~f:(fun key ->
          if not (Hash_set.mem selected key) then
            make_key_stale key;
        ))
    in
    setup_generator hashable ~compute_output ~make_input_node
end

