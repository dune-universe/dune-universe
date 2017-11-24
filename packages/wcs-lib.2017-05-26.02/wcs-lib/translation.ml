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
open Dialog_t

let mk_conditions cond =
  begin match cond with
  | None -> None
  | Some cond -> Some (Spel.of_string cond)
  end

let mk_context context =
  begin match context with
  | None -> None
  | Some context -> Some (Json_spel.of_json context)
  end

let mk_output output =
  begin match output with
  | None -> None
  | Some output -> Some (Json_spel.of_json output)
  end

let dialog_node_of_node (n : node) : dialog_node list =
  let root =
    { node_dialog_node = n.n_dialog_node;
      node_type_ = None;
      node_description = n.n_description;
      node_conditions = mk_conditions n.n_conditions;
      node_parent = None;
      node_previous_sibling = None;
      node_output = None;
      node_context = None;
      node_metadata = n.n_metadata;
      node_next_step = n.n_next_step;
      node_child_input_kind = None;
      node_created = n.n_created;
      node_updated = n.n_updated;
      node_event_name = None;
      node_variable = None; }
  in
  let children =
    begin match n.n_reactions with
    | [] -> []
    | _ -> assert false (* XXX TODO XXX *)
    end
  in
  let children =
    begin match n.n_slots with
    | [] -> children
    | _ -> assert false (* XXX TODO XXX *)
    end
  in
  let root, children =
    begin match n.n_responses with
    | [] -> root, children
    | [ { r_conditions = None;
          r_output = output;
          r_context = context; } ] ->
        let root =
          { root with
            node_output = mk_output output;
            node_context = mk_context context; }
        in
        root, children
    | _ -> assert false (* XXX TODO XXX *)
    end
  in
  root :: children

let dialog_nodes_of_dialog (d: dialog) : dialog_node list =
  let rec compile d =
    begin match d with
    | D [] -> []
    | D l ->
        List.fold_right
          (fun (n, d) acc ->
             let children = compile d in
             let dn_tree = dialog_node_of_node n in
             let root = Wcs.get_root dn_tree in
             let dn_tree =
               Wcs.add_tree dn_tree children root None
             in
             Wcs.add_tree dn_tree acc None root)
          l []
    end
  in
  compile d
