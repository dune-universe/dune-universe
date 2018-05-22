open Printf

(*
  Build this test with the following command:
  ocamlc -g -o page_result_control_test -package str -package ldap -thread -linkpkg page_result_control_test.ml
*)

let default_server = "ldap://x500.bund.de"
let default_base = "o=Bund,c=DE"
let default_who = ""
let default_cred = ""
let default_page_size = 200

let get_page_control controls =
  List.fold_left
    (fun cur_res control ->
      match cur_res with
      | None ->
        begin match control.Ldap_types.control_details with
        | `Paged_results_control _ -> Some control
        | _ -> None
        end
      | Some x -> Some x)
    None
    controls

let rec entry_list_builder_helper accum search_function page_size msgid conn =
  let cur_entry =
    try
      Ldap_funclient.get_search_entry_with_controls conn msgid
    with _ -> failwith "error"
  in
  begin match cur_entry with
  | `Success None -> accum
    (* This means we are done, if we are not using page
    control...but we are so we never reach here in this case *)
  | `Success (Some controls) ->
    (* do recursive call with cookie *)
    let pg_control = get_page_control controls in
    begin match pg_control with
    | None ->
      (*printf "Error: couldn't get page control\n";*)
      []
    | Some c ->
      begin match c.Ldap_types.control_details with
      | `Paged_results_control value ->
        let mycookie=value.Ldap_types.cookie in
        if mycookie = "" then
          accum (* This means we are done. *)
        else
          let new_msgid = search_function (`Subctrl (page_size,mycookie)) in
          entry_list_builder_helper accum search_function page_size new_msgid conn
      | `Unknown_value _ ->
        (*printf "Error: unknown ldap control value: %s\n" s;*)
        []
      end
    end
  | `Entry e ->
    entry_list_builder_helper (e::accum) search_function page_size msgid conn
  | `Referral _ ->
    entry_list_builder_helper accum search_function page_size msgid conn
    (*ignore referrals and continue *)
  end

let entry_list_builder search_function page_size msgid conn =
  entry_list_builder_helper [] search_function page_size msgid conn

let search_function conn page_control =
  Ldap_funclient.search
    ~base:default_base
    ~scope:`SUBTREE
    ~attrs:["dc"]
    ~page_control
    conn
    "(objectclass=*)"

let () =
  let conn = Ldap_funclient.init [default_server] in
  Ldap_funclient.bind_s
    conn
    ~who:default_who
    ~cred:default_cred
    ~auth_method:`SIMPLE;
  let msgid = search_function conn (`Initctrl default_page_size) in
  let elist =
    entry_list_builder (search_function conn) default_page_size msgid conn
  in
  printf "got %d entries\n" (List.length elist)

