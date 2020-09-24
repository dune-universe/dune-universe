open Printf
let get_group_and_title (n : I3ipc.Reply.node) =
  match n.window_properties with
  | None -> ("", "")
  | Some prop ->
      (Option.value prop.class_ ~default:"", Option.value prop.title ~default:"")

let lnq_lex s1 s2 =
  if String.equal s1 s2 then false
  else
    let rec aux i1 i2 =
      if i1 = String.length s1 then true
      else if i2 = String.length s2 then false
      else
        let c1 = Char.code (Char.lowercase_ascii s1.[i1]) in
        let c2 = Char.code (Char.lowercase_ascii s2.[i2]) in
        if c1 = c2 then aux (i1 + 1) (i2 + 1) else c1 < c2
    in
    aux 0 0

let compare_lex s1 s2 =
  if String.equal s1 s2 then 0
  else
    let rec aux i1 i2 =
      if i1 = String.length s1 then 1
      else if i2 = String.length s2 then -1
      else
        let c1 = Char.code (Char.lowercase_ascii s1.[i1]) in
        let c2 = Char.code (Char.lowercase_ascii s2.[i2]) in
        if c1 = c2 then aux (i1 + 1) (i2 + 1) else Int.compare c1 c2
    in
    aux 0 0

let get_focused_window (tree : I3ipc.Reply.node) =
  let rec aux (node : I3ipc.Reply.node) =
    if node.focused then Some node
    else
      let nodeoptopt =
        List.find_opt
          (fun (n : I3ipc.Reply.node option) ->
            match n with Some (_ : I3ipc.Reply.node) -> true | None -> false)
          (List.map aux node.nodes)
      in
      match nodeoptopt with
      | None -> (
          let nodeoptopt =
            List.find_opt
              (fun (n : I3ipc.Reply.node option) ->
                match n with
                | Some (_ : I3ipc.Reply.node) -> true
                | None -> false)
              (List.map aux node.floating_nodes)
          in
          match nodeoptopt with
          | None -> None
          | Some None -> failwith "oooooops"
          | Some s -> s )
      | Some None -> failwith "ooops"
      | Some s -> s
  in
  match aux tree with Some node -> node | None -> failwith "no focused node"

let focus_win conn con_id =
  I3ipc.command conn (sprintf {|[con_id="%s"] focus|} con_id)

let move_window_left conn con_id =
  (let%lwt _ = focus_win conn con_id in
   Lwt.return_unit);%lwt
  let%lwt _ = I3ipc.command conn "move left" in
  Lwt.return_unit

let order_tabs conn =
  let%lwt tree = I3ipc.get_tree conn in
  let rec iterator (node : I3ipc.Reply.node) =
    if node.layout = I3ipc.Reply.Tabbed then
      let tab = Array.of_list node.nodes in
      let compare n1 n2 =
        let group1, name1 = get_group_and_title n1 in
        let group2, name2 = get_group_and_title n2 in
        if compare_lex group1 group2 = 0 then compare_lex name1 name2
        else compare_lex group1 group2
      in
      (* Array.iter (fun e -> let g, t = get_group_and_title e in printf "%s %s\n" g t) tab ;*)
      Order.ordered tab compare (fun (n : I3ipc.Reply.node) ->
          let g, t = get_group_and_title n in
          printf "moving %s %s\n" g t;
          move_window_left conn n.id)
    else Lwt_list.iter_s iterator node.nodes
  in
  Lwt_list.iter_s iterator tree.nodes

let do_ordering conn =
  let%lwt tree = I3ipc.get_tree conn in
  let curr_node = get_focused_window tree in
  order_tabs conn;%lwt
  let%lwt _ = focus_win conn curr_node.id in
            Lwt.return_unit

let main =
  let%lwt conn = I3ipc.connect () in
  let%lwt _ = I3ipc.subscribe conn [ I3ipc.Window ] in
  do_ordering conn ;%lwt
  while%lwt true do
    match%lwt I3ipc.next_event conn with
    | Window info -> (
        match info.change with
        | New | Title | FullscreenMode | Move ->
            do_ordering conn
        | _ -> Lwt.return_unit )
    | _ -> Lwt.return_unit
  done

let () = Lwt_main.run main
