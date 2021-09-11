open Ppxlib

(** {[
      [%defer later];
      now
    ]}

    will evaluate [later] after [now]. For example:

    {[
      let ic = open_in_bin "test.ml" in
      [%defer close_in ic];
      let length = in_channel_length ic in
      let bytes = really_input_string ic length in
      print_endline bytes
    ]}

    will close [ic] after reading and printing its content. *)

let make_defer ~later ~now =
  (* Evaluate [now] then [later], even if [now] raises an exception *)
  let loc = now.pexp_loc in
  [%expr
    match [%e now] with
    | __ppx_defer_actual_result ->
      [%e later];
      __ppx_defer_actual_result
    | exception __ppx_defer_actual_exception ->
      [%e later];
      raise __ppx_defer_actual_exception]

let make_defer_lwt ~later ~now =
  (* Evaluate [now] then [later], even if [now] raises an exception *)
  let loc = now.pexp_loc in
  [%expr Lwt.finalize (fun () -> [%e now]) (fun () -> [%e later])]

class mapper =
  object (_self)
    inherit Ast_traverse.map as super

    method! expression expr =
      match expr with
      | [%expr
          [%defer [%e? later]];
          [%e? now]] ->
        let (later, now) = (super#expression later, super#expression now) in
        let generated = make_defer ~later ~now in
        let pexp_loc =
          (* [loc_ghost] tells the compiler and other tools than this is
             generated code *)
          { generated.pexp_loc with Location.loc_ghost = true }
        in
        { generated with pexp_loc }
      | [%expr
          [%defer.lwt [%e? later]];
          [%e? now]] ->
        let (later, now) = (super#expression later, super#expression now) in
        let generated = make_defer_lwt ~later ~now in
        let pexp_loc =
          (* [loc_ghost] tells the compiler and other tools than this is
             generated code *)
          { generated.pexp_loc with Location.loc_ghost = true }
        in
        { generated with pexp_loc }
      | _ -> super#expression expr
  end

let () =
  let mapper = new mapper in
  Driver.register_transformation "ppx_defer" ~impl:mapper#structure
    ~intf:mapper#signature
