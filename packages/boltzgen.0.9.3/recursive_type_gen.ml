open Type
open Sum_type

let evaluate st =
  let named = named_of_sum st in
  add_type_to_lib named

let named_of_string st =
  let td = Parse_from_compiler.parse_typedef st in
  named_of_sum td

let abstract_impl = Name ("nat", [])

(* print the to_string *)
let rec gen_string_of_compo meml = function
  | Abstract _ -> gen_string_of_compo meml abstract_impl
  | Name (x, _) as bt ->
      let nt = find_type x in
      nt.string_of_named meml gen_string_of_compo bt
  | Prod l ->
      let def, body = print_prod_aux (gen_string_of_compo meml) 1 l in
      Printf.sprintf "(fun (%s) -> (\"(\"^%s^\")\"))" def body
  | _ -> failwith "not implemented yet"

let boltzman_memoize = Hashtbl.create 10

let add_memoize bt z v = Hashtbl.add boltzman_memoize (bt, z) v

(*Hashtbl.iter
  (fun (bt, z) _ ->
    Printf.printf "Hashtblentry: '%s':%s \n" (string_of_compo bt)
      (string_of_float z))
  boltzman_memoize*)

let rec boltzman_from_compo bt x =
  if !verbose > 5 then
    Format.printf "boltzman_from_compo %a@." (pp_compo ~use_boltz_id:false) bt;
  match Hashtbl.find_opt boltzman_memoize (bt, x) with
  | Some b -> b
  | None ->
      let b =
        match bt with
        | Abstract _ -> (x, 1.0)
        | Name (n, _) ->
            let nt = find_type n in
            nt.boltzman_fun boltzman_from_compo bt x
        | Prod l ->
            let rec aux i = function
              | [] -> (1.0, 0.0)
              | [ b ] -> boltzman_from_compo b x
              | tb :: q ->
                  let v, dv = aux (i + 1) q in
                  let v2, dv2 = boltzman_from_compo tb x in
                  (v *. v2, (dv *. v2) +. (dv2 *. v))
            in
            aux 1 l
        | _ -> (x, 1.0)
      in
      add_memoize bt x b;
      b

let rec equations_from_compo bt accrec =
  match bt with
  | Name (n, _) ->
      let nt = find_type n in
      if not nt.is_simple then nt.get_equ equations_from_compo accrec bt
      else accrec
  | Prod l ->
      let b =
        List.fold_left (fun acc b -> equations_from_compo b acc) accrec l
      in
      b
  | _ -> accrec

(* generate the value as a value *)
let rec gen_from_compo rs m bt z =
  match bt with
  | Abstract _ ->
      let v, s = gen_from_compo rs m abstract_impl z in
      (hide (reveal v abstract_impl) bt, s)
      (* Cast int -> abstract *)
  | Name (x, _) ->
      let nt = find_type x in
      nt.gen_fun rs m gen_from_compo boltzman_from_compo bt z
  | Prod l ->
      let n = List.length l in
      let arrayt = Array.make n 0 in
      let size = ref 1 in
      List.iteri
        (fun i ct ->
          let v, s = gen_from_compo rs m ct z in
          size := !size + s;
          let value = reveal v ct in
          arrayt.(i) <- value)
        l;
      (hide arrayt bt, !size)
  | _ -> failwith "not implemented yet"

(* print the generated value as string *)
let rec print_from_compo bt f cv =
  match bt with
  | Abstract _ ->
      (* Abstract are instantiate as int *)
      Format.pp_print_int f (reveal cv bt)
  | Name (x, _) ->
      let nt = find_type x in
      nt.print print_from_compo bt f cv
  | Prod l ->
      let vtable = reveal cv bt in
      let ttable = Array.of_list l in
      Format.pp_print_string f "(";
      Array.iteri
        (fun i v ->
          let t = ttable.(i) in
          if i > 0 then Format.pp_print_string f ",";
          print_from_compo t f (hide v t))
        vtable;
      Format.pp_print_string f ")"
  | _ -> failwith "not implemented yet"

let pp_type_of_out f = function
  | Abstract _ -> Format.pp_print_string f "int"
  | x -> pp_compo ~use_boltz_id:true f x

(* if bt is a function generate it otherwise do nothing *)
let gen_random_fun_def_compo hash f bt =
  match bt with
  | Fun (l, ofun) -> (
      match Hashtbl.find_opt hash (l, ofun) with
      | Some _ -> ()
      | None ->
          let id = char_of_int (97 + Hashtbl.length hash) in
          Hashtbl.add hash (l, ofun) id;
          let fn, tpl, _ =
            (*name each arg and make a tuple i.e. uncurry  *)
            List.fold_left
              (fun (a, a2, i) _ ->
                ( a ^ Printf.sprintf " %c" (char_of_int i),
                  a2
                  ^ Printf.sprintf "%s%c"
                      (if a2 <> "" then "," else "")
                      (char_of_int i),
                  i + 1 ))
              ("", "", 97) l
          in
          Format.fprintf f "\tlet fun_%c n%s = rand_fun \"%a\" n (%s)@." id fn
            pp_type_of_out ofun tpl )
  | _ -> ()

let gen_random_fun_def out hash f =
  Format.pp_print_list (gen_random_fun_def_compo hash) out f.intypes

let gen_compo_type hash rs m z = function
  | Fun (l, ofun) ->
      let n = Random.int m in
      let fid = Hashtbl.find hash (l, ofun) in
      (Format.sprintf "(fun_%c %i)" fid n, 1)
  | ct ->
      let cv, size = gen_from_compo rs m ct z in
      print_from_compo ct Format.str_formatter cv;
      (Format.flush_str_formatter (), size)

let already_gen = Hashtbl.create 10

let rec call_random ?tsrange ?(max_iter = 100) hash rs m z f =
  let m2 = List.map (fun t -> gen_compo_type hash rs m z t) f.intypes in
  let m3, size =
    List.fold_left
      (fun (acc, s1) (s, s2) -> (acc ^ " " ^ s, s1 + s2))
      ("", 0) m2
  in
  let str = Printf.sprintf "%s%s" f.name m3 in
  if max_iter <= 0 then str
  else if Hashtbl.mem already_gen str then
    call_random ?tsrange ~max_iter:(max_iter - 1) hash rs m z f
  else (
    Hashtbl.add already_gen str ();
    match tsrange with
    | None -> str
    | Some (low, up) when low <= size && size <= up -> str
    | _ -> call_random ?tsrange ~max_iter:(max_iter - 1) hash rs m z f )
