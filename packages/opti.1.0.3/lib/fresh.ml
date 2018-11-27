open Syntax_tree

type fresh_name_generator = {
    mutable fresh_name_generator_counter: int
  }

let make_fresh_name_generator ()
    =
  { fresh_name_generator_counter = 0 }

let fresh_name_generator_generate_name (g : fresh_name_generator) ~(base_name: string)
    =
  let name = Printf.sprintf "%s%i" base_name g.fresh_name_generator_counter in
  g.fresh_name_generator_counter <- g.fresh_name_generator_counter + 1;
  name

let rename_subscripts_in_variable (g : fresh_name_generator) (v : variable)
    =
  let table = Hashtbl.create 10 in
  let rec rename subscript_name =
    try Hashtbl.find table subscript_name
    with Not_found ->
      let new_name = fresh_name_generator_generate_name g ~base_name:subscript_name in
      if Hashtbl.mem table new_name then
        rename subscript_name (* try again *)
      else begin
        Hashtbl.add table subscript_name new_name;
        new_name
      end
  in      
  map_subscripts_in_variable rename v
