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

open Wcs_lib
open Wcs_t
open Wcs

let omap f o =
  begin match o with
  | None -> None
  | Some x -> Some (f x)
  end

let set_node_parent parent (x:Wcs_t.dialog_node) =
  {x with node_parent = omap (fun (node:Wcs_t.dialog_node) -> node.node_dialog_node) parent}

let set_node_parent_if_none parent (x:Wcs_t.dialog_node) =
  match x.node_parent with
  | Some _ -> x
  | None -> {x with node_parent = omap (fun (node:Wcs_t.dialog_node) -> node.node_dialog_node) parent}

let update_node_name (f:string->string) (x:Wcs_t.dialog_node) =
  {x with node_dialog_node = f (x.node_dialog_node)}

let get_node_name (x:Wcs_t.dialog_node) =
  x.node_dialog_node


(** Entities *)

let entity_yes =
  entity "yes"
    ~values:[ "yes",
              [ "da";
                "definitely";
                "one more";
                "oui";
                "totally";
                "ya";
                "absolutely";
                "indeed";
                "it has";
                "yeah";
                "yep"; ]]
    ()

let entity_no =
  entity "no"
    ~values:[ "no",
              [ "done";
                "finished";
                "I'm done";
                "I'm finished";
                "nada";
                "niet";
                "no";
                "over";
                "nope"; ] ]
    ()

let entity_bmd =
  entity "bmd"
    ~values:[ "bmd",
              [ "bmd";
                "bom";
                "business model";
                "business model description";
                "business object model"; ] ]
    ()

let bom_sample_entities = [
  entity "entity"
    ~values:[ "authorization",
              [ "authorization response";
                "the authorization";
                "the authorization response" ];
              "transaction",
              [ "the transaction" ]; ]
    ();
  entity "field"
    ~values: [ "account", [];
               "amount", [];
               "country", [ "country code" ];
               "message", [];
               "transaction", [];]
    ();
  sys_number;
]

let entity_boolean =
  entity "boolean"
    ~values:["true",
             [];
             "false",
             []]
    ()

let intent_help =
  intent "mk_help"
    ~description:"The user needs help knowing what to do"
    ~examples:["help";
               "I am confused";
               "What can I do";
               "What are my choices";
               "options";
               "alternatives";
               "choices";
               "usage";
               "guide";
               "directions"
              ]
    ()

let anything_else =
  dialog_node "anything_else"
    ~conditions: "anything_else"
    ()

(** Sample BMD *)

let bmd_sample_entities =
  Bmd_to_wcs_entities.entities_of_bmd Bmd_samples.airline_schema

let mkEnglishList sep strList =
  match strList with
  | [] -> ""
  | [s] -> s
  | [s; t] -> s ^ " " ^ sep ^ " " ^ t
  | ls -> let rec mkLongEnglishList l =
            match l with
            | [] -> "" (* this case can't actually happen *)
            | [s; t] -> s ^ ", " ^ sep ^ " " ^ t
            | s::t -> s ^ ", " ^ mkLongEnglishList t
      in mkLongEnglishList ls

let bmd_fields bmd = Bmd_to_wcs_entities.bmd_getentity_values bmd "field"
let bmd_entities bmd = Bmd_to_wcs_entities.bmd_getentity_values bmd "entity"
let bmd_enumerations bmd = Bmd_to_wcs_entities.bmd_getentity_values bmd "enum"

let add_usage_extended add_node dialog_name kind parent goto usage =
  let usage_str = "The supported possibilities are: " ^ (mkEnglishList "and" usage ^ ".") in

  let help_node =
    set_node_parent parent (
      dialog_node (dialog_name ^ " Help")
        ~conditions: "#mk_help"
        ~text: ("No problem, I am happy to help.\nCan you please tell me "^kind^ " you want?\n"^usage_str)
        ~next_step: (goto, Goto_body)
        ()) in
  add_node help_node ;
  let unknown_node =
    set_node_parent parent (
      dialog_node (dialog_name ^  " Unknown")
        ~conditions: "anything_else"
        ~text: ("I am sorry, I am having trouble understanding "^ kind ^" you want.\n"^usage_str)
        ~next_step: (goto, Goto_body)
        ()) in
  add_node unknown_node

let dname (parent:Wcs_t.dialog_node option) s =
  match parent with
  | Some p -> get_node_name p ^ "." ^ s  ^ "."
  | None -> s

module type SubDialog = sig
  val add_node : Wcs_t.dialog_node -> unit
  val add_nodes : Wcs_t.dialog_node list -> unit
  val add_sub_dialogs : Wcs_t.dialog_node option ->
    (Wcs_t.dialog_node option -> Wcs_t.dialog_node list) list ->
    unit
  val get_nodes : unit -> Wcs_t.dialog_node list

  val add_usage : string -> string -> Wcs_t.dialog_node -> string list -> unit
  val add_usage_toplevel : string -> string -> Wcs_t.dialog_node -> string list -> unit


  val dialog_node :
    string ->
    ?description:string ->
    ?conditions:string ->
    ?parent:Wcs_t.dialog_node ->
    ?previous_sibling:Wcs_t.dialog_node ->
    ?text:string ->
    ?output:Wcs_t.json ->
    ?context:Wcs_t.json ->
    ?metadata:Wcs_t.json ->
    ?next_step:Wcs_t.dialog_node * selector ->
    ?next_step_id:string * selector ->
    ?created:string ->
    unit -> Wcs_t.dialog_node

  val add_dialog_node :
    string ->
    ?description:string ->
    ?conditions:string ->
    ?parent:Wcs_t.dialog_node ->
    ?previous_sibling:Wcs_t.dialog_node ->
    ?text:string ->
    ?output:Wcs_t.json ->
    ?context:Wcs_t.json ->
    ?metadata:Wcs_t.json ->
    ?next_step:Wcs_t.dialog_node * selector ->
    ?next_step_id:string * selector ->
    ?created:string ->
    unit -> Wcs_t.dialog_node
end

let make_sub_dialog_module top_parent = (module struct
                                          let dname = dname top_parent
                                          let dialog_node
                                                dn
                                                ?description
                                                ?conditions
                                                ?parent
                                                ?previous_sibling
                                                ?text
                                                ?output
                                                ?context
                                                ?metadata
                                                ?next_step
                                                ?next_step_id
                                                ?created
                                                ()
                                            = set_node_parent_if_none top_parent (dialog_node (dname dn)
                                                                                    ?description
                                                                                    ?conditions
                                                                                    ?parent
                                                                                    ?previous_sibling
                                                                                    ?text
                                                                                    ?output
                                                                                    ?context
                                                                                    ?metadata
                                                                                    ?next_step
                                                                                    ?next_step_id
                                                                                    ?created
                                                                                    ())
                                          let nodes = ref []
                                          let add_nodes ns =
                                            nodes := !nodes @ [ns]
                                          let add_node n =
                                            nodes := !nodes @ [[ n ]]
                                          let add_sub_dialogs parent children =
                                            List.iter (fun x -> add_nodes (x parent))
                                              children
                                          let get_nodes () =
                                            List.concat (!nodes)
                                          let add_dialog_node
                                                dn
                                                ?description
                                                ?conditions
                                                ?parent
                                                ?previous_sibling
                                                ?text
                                                ?output
                                                ?context
                                                ?metadata
                                                ?next_step
                                                ?next_step_id
                                                ?created
                                                ()
                                            = let n = dialog_node dn
                                                        ?description
                                                        ?conditions
                                                        ?parent
                                                        ?previous_sibling
                                                        ?text
                                                        ?output
                                                        ?context
                                                        ?metadata
                                                        ?next_step
                                                        ?next_step_id
                                                        ?created
                                                        () in
                                            add_node n ; n
                                          let add_usage dialog_name kind usage_parent usage =
                                            add_usage_extended add_node (dname dialog_name) kind (Some usage_parent) usage_parent usage
                                          let add_usage_toplevel dialog_name kind continuation usage =
                                            add_usage_extended add_node (dname dialog_name) kind top_parent continuation usage
                                        end : SubDialog)

let make_sub_dialog parent (f:(module SubDialog) -> 'a) =
  let sd = make_sub_dialog_module parent in
  f sd ;
  let (module SD) = sd in
  SD.get_nodes()

let make_child_dialog (f:(module SubDialog) -> 'a) parent =
  let sd = make_sub_dialog_module parent in
  f sd ;
  let (module SD) = sd in
  SD.get_nodes()

let make_top_dialog (f:(module SubDialog) -> 'a) =
  let sd = make_sub_dialog_module None in
  f sd ;
  let (module SD) = sd in
  SD.get_nodes()
