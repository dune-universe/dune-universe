(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
*)

open Wcs_t

let omap f o =
  begin match o with
  | None -> None
  | Some x -> Some (f x)
  end

let new_id =
  let cpt = ref 0 in
  fun () -> incr cpt; "node_"^(string_of_int !cpt)

let list_workspaces_request
      (* ?version *)
      ?page_limit
      ?include_count
      ?sort
      ?cursor
      ()
  : list_workspaces_request =
  { (* list_ws_req_version = version; *)
    list_ws_req_page_limit = page_limit;
    list_ws_req_include_count = include_count;
    list_ws_req_sort = sort;
    list_ws_req_cursor = cursor; }

let get_workspace_request ?export workspace_id =
  { get_ws_req_workspace_id = workspace_id;
    get_ws_req_export = export; }

let example
      example
      ?created
      ?updated
      ()
  : intent_example =
  { ex_text = example;
    ex_created = created;
    ex_updated = updated; }

let intent
      intent
      ?description
      ?(examples=[])
      ?created
      ?updated
      ()
  : intent_def =
  { i_def_intent = intent;
    i_def_description = description;
    i_def_examples = List.map (fun s -> example s ()) examples;
    i_def_created = created;
    i_def_updated = updated; }

let value
      value
      ?metadata
      ?(synonyms=[])
      ?created
      ?updated
      ()
  : entity_value =
  { e_value = value;
    e_metadata = metadata;
    e_synonyms = synonyms;
    e_created = created;
    e_updated = updated; }

let entity
      entity
      ?description
      ?metadata
      ?source
      ?open_list
      ?(values=[])
      ?created
      ?updated
      ?fuzzy_match
      ()
  : entity_def =
  { e_def_entity = entity;
    e_def_description = description;
    e_def_metadata = metadata;
    e_def_source = source;
    e_def_open_list = open_list;
    e_def_values = List.map (fun (v, syn) -> value v ~synonyms:syn ()) values;
    e_def_created = created;
    e_def_updated = updated;
    e_def_fuzzy_match = fuzzy_match;  }

let next_step
      node
      ~selector
      ()
  : next_step =
  { next_behavior = "jump_to";
    next_selector = selector;
    next_dialog_node = node.node_dialog_node; }

let mk_next_step = next_step (* alias to avoid hiding *)

let next_step_id
      node_id
      ~selector
      ()
  : next_step =
  { next_behavior = "jump_to";
    next_selector = selector;
    next_dialog_node = node_id; }

let mk_next_step_id = next_step_id (* alias to avoid hiding *)


let output (* XX TODO : handle multiple outputs *)
      text
  : output_def =
  (`Assoc [ "text", `String text ])

let mk_output = output (* alias to avoid hiding *)

let dialog_node
      dialog_node
      ?description
      ?type_
      ?(conditions="true")
      ?parent
      ?previous_sibling
      ?text
      ?output
      ?context
      ?metadata
      ?next_step
      ?next_step_id
      ?created
      ?updated
      ?event_name
      ?variable
      ()
  : dialog_node =
  let parent_id =
    omap (fun node -> node.node_dialog_node) parent
  in
  let previous_sibling_id =
    omap (fun node -> node.node_dialog_node) previous_sibling
  in
  let output =
    begin match text, output with
    | None, None -> None
    | Some text, None -> Some (mk_output text)
    | None, Some output -> Some output
    | Some _, Some _ ->
        Log.error "Ws_builder"
          (Some None)
          "dialog_node: ~text and ~output cannot be present simlutanously"
    end
  in
  let next_step =
    begin match next_step, next_step_id with
    | None, None -> None
    | Some (node, selector), None ->
        Some (mk_next_step node ~selector ())
    | None, Some (node_id, selector) ->
        Some (mk_next_step_id node_id ~selector ())
    | Some _, Some _ ->
        Log.error "Ws_builder"
          (Some None)
          "dialog_node: ~next_step and ~next_step_id cannot be present simlutanously"
    end
  in
  { node_dialog_node = dialog_node;
    node_description = description;
    node_type_ = type_;
    node_conditions = Some conditions;
    node_parent = parent_id;
    node_previous_sibling = previous_sibling_id;
    node_output = output;
    node_context = context;
    node_metadata = metadata;
    node_next_step = next_step;
    node_created = created;
    node_updated = updated;
    node_child_input_kind = None;
    node_event_name = event_name;
    node_variable = variable; }

let response_condition ~parent =
  dialog_node (new_id ())
    ~type_: Node_response_condition
    ~parent:parent
    ?next_step: None (* Not yet implemented*)
    ?next_step_id: None (* Not yet implemented*)
    ?event_name: None
    ?variable: None

let fix_links nodes =
  let parent_child_tbl = Hashtbl.create 7 in
  let node_tbl = Hashtbl.create 7 in
  List.map
    (fun node ->
       let node =
         begin match node.node_parent, node.node_previous_sibling with
         | Some _, Some _ -> node
         | Some _, None
         | None, None ->
             begin try
               let previous_sibling =
                 Hashtbl.find parent_child_tbl node.node_parent
               in
               { node with
                 node_previous_sibling = Some previous_sibling.node_dialog_node }
             with Not_found ->
               node
             end
         | None, Some previous_sibling_name ->
             begin try
               let previous_sibling =
                 Hashtbl.find node_tbl previous_sibling_name
               in
               { node with
                 node_parent = previous_sibling.node_parent }
             with Not_found ->
               node
             end
         end
       in
       Hashtbl.add parent_child_tbl node.node_parent node;
       Hashtbl.add node_tbl node.node_dialog_node node;
       node)
    nodes

let workspace
      name
      ?description
      ?language
      ?metadata
      ?(counterexamples=[])
      ?(dialog_nodes=[])
      ?(entities=[])
      ?(intents=[])
      ?created
      ?updated
      ?modified
      ?created_by
      ?modified_by
      ?workspace_id
      ?status
      ()
  : workspace =
  let counterexamples =
    List.map (fun s -> example s ()) counterexamples
  in
  { ws_name = Some name;
    ws_description = description;
    ws_language = language;
    ws_metadata = metadata;
    ws_counterexamples = counterexamples;
    ws_dialog_nodes = fix_links dialog_nodes;
    ws_entities = entities;
    ws_intents = intents;
    ws_created = created;
    ws_updated = updated;
    ws_modified = modified;
    ws_created_by = created_by;
    ws_modified_by = modified_by;
    ws_workspace_id = workspace_id;
    ws_status = status; }


let logs_request
      (* ?version *)
      ?filter
      ?sort
      ?page_limit
      ?cursor
      ()
  : logs_request =
  { logs_filter = filter;
    logs_sort = sort;
    logs_page_limit = page_limit;
    logs_cursor = cursor; }



let sys_number : entity_def =
  entity "sys-number"
    ~source: "system.entities"
    ()


(** {6 Tree modifications} *)

let get_root tree
  : dialog_node option =
  begin try
    Some
      (List.find
         (fun x -> x.node_parent == None && x.node_previous_sibling == None)
         tree)
  with Not_found ->
    None
  end

let get_name  =
  omap (fun x -> x.node_dialog_node)

let add_node
      dialog_nodes
      dialog_node
      parent
      previous_sibling
  : dialog_node list =
  let node =
    { dialog_node with
      node_parent = get_name parent;
      node_previous_sibling = get_name previous_sibling; }
  in
  let dialog_nodes =
    List.map
      (fun n ->
         if n.node_previous_sibling = get_name previous_sibling
         then
           { n with node_previous_sibling = Some node.node_dialog_node; }
         else n)
      dialog_nodes
  in
  node :: dialog_nodes

let last_sibling dialog_node dialog_nodes =
  let is_last_sibling x =
    let x_is_previous_sibling =
      List.exists
        (fun y -> Some x.node_dialog_node = y.node_previous_sibling)
        dialog_nodes
    in
    x.node_parent == dialog_node.node_parent && not x_is_previous_sibling
  in
  begin try
    Some (List.find is_last_sibling dialog_nodes)
  with Not_found ->
    None
  end

let add_tree
      dialog_nodes
      tree
      parent
      previous_sibling
  : dialog_node list =
  let root =
    begin match get_root tree with
    | Some root -> root
    | None ->
        Log.error "Ws_builder"
          None
          "add_tree: tree has no root"
    end
  in
  let last = last_sibling root tree in
  let dialog_nodes =
    List.map
      (fun n ->
         if n.node_previous_sibling = get_name previous_sibling
         then { n with node_previous_sibling = get_name last; }
         else n)
      dialog_nodes
  in
  let tree =
    List.map
      (fun n ->
         begin match n.node_parent, n.node_previous_sibling with
         | None, None ->
             {n with node_parent = get_name parent;
                     node_previous_sibling = get_name previous_sibling;}
         | None, Some _ ->
             {n with node_parent = get_name parent; }
         | _ -> n
         end)
      tree
  in
  dialog_nodes @ tree
