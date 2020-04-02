open Acommon
open Key_value

module Property = KeyValueF.Make (String) (String)

module Properties = struct
  include Property.Map

  let get = find_opt

  let get_or_default key ~default ps = match get key ps with | Some v -> v | None -> default

  let rec of_list = function
    | [] -> empty
    | (k,v)::l -> add k v @@ of_list l

  let to_list = bindings

  let of_string ?(prop_sep=";") ?(kv_sep="=") s =
    if (String.length s = 0) then empty
    else
      let add_prop props p =
        match Astring.cut ~sep:kv_sep p with
        | Some (k,v) -> add k v props
        | None -> add p "" props
      in
      Astring.cuts ~sep:prop_sep s |> List.fold_left add_prop empty


  let to_string ?(prop_sep=";") ?(kv_sep="=") (p:Property.Value.t t) =
    to_list p |> List.map (fun (k,v)-> k^kv_sep^v) |> String.concat prop_sep
  
  let contains_key prop ps = match get prop ps with | Some _ -> true | None -> false 

  let contains_property k v ps = 
    match get k ps with
    | Some v' -> v = v'
    | None -> false  

  let contains_conflicting_property k v ps = 
    match get k ps with
    | Some v' -> v <> v'
    | None -> false

  let is_subset ps ps' = not @@ exists (fun k v -> not @@ contains_property k v ps') ps

  let not_conflicting ps ps' = not @@ exists (fun k v -> contains_conflicting_property k v ps') ps

  let decode_property_value decoder prop ps = 
    let open Option.Infix in 
    get prop ps >>= fun v -> 
    try 
      Some (decoder v)
    with 
    | _ -> None

  let encode_property_value encoder v = 
    try 
      Some (encoder v)
    with 
    | _ -> None

end

type properties = Property.Value.t Properties.t
