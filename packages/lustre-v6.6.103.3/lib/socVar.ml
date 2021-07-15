(* Time-stamp: <modified the 12/03/2020 (at 15:02) by Erwan Jahier> *)

open Data

let rec (expand_profile : bool -> bool -> Soc.var list -> Soc.var list) =
  fun enum_flag c_access vl ->
    let res = List.flatten (List.map (expand_var enum_flag c_access) vl) in
    (* fix point. now useless ? *)
    if List.length res = List.length vl then res else
      expand_profile enum_flag c_access res
and expand_var enum_flag c_access var = match var with
  | (_vn,(Bool| Int | Real)) -> [var]
  | (vn,Enum(_n,_l)) -> if enum_flag then [vn,Int] else [var]
  | (vn,Array(vt,i)) ->
    let res = ref [] in
    for k=i-1 downto 0 do
      let access = 
        if c_access then "["^(string_of_int k)^"]" else "_"^(string_of_int k)
      in
      res := (vn^access,vt) :: !res;
    done;
    (expand_profile enum_flag c_access !res)
  | (vn,Struct(_name,fl)) -> 
    expand_profile enum_flag c_access 
      (List.map (fun (fn,t) -> vn^(if c_access then "." else "_")^fn,t ) fl)
  | (_vn,Extern _id) -> [var]
  | (vn,Alias(_n, t)) -> expand_var enum_flag c_access (vn,t)
  | (_vn,Alpha _) -> assert false (* should not occur *) 
  | (_vn, String) -> assert false (* should not occur *) 

let (_int_to_enum : Data.v -> Soc.ident list -> Data.v) =
  fun v el ->
    match v with
      | I i -> (try E (List.nth el i,i) with _ ->
        failwith ("Enum out of the range [0,"^(string_of_int (List.length el - 1))^"]"))
      | _ -> assert false (* should not occur *)

let (expand_subst: Data.subst -> Data.subst list) =
  fun s ->
    let rec aux acc (n,v) =
      match v with
        | U | I _ | F _ | B _ | Str _  -> (n,v)::acc
        | E(_e,i) -> (n,I i)::acc
        | S fl -> 
          let f (fn,fv) = n^"_"^fn, fv in
          let fl = List.map f fl in
          List.fold_left aux acc fl
        | A a -> 
          let res = ref acc in
          for i=0 to (Array.length a)-1 do
            let n_i = n^"_"^(string_of_int i) in
            res := aux !res (n_i, a.(i));
          done;
          !res
    in
    aux [] s

(* exported *)

let (int_to_enum : Data.v -> Soc.ident list -> Data.v) =
  fun v el ->
    match v with
      | I i -> (try E (List.nth el i,i) with _ ->
        failwith ("Enum out of the range [0,"^(string_of_int (List.length el - 1))^"]"))
      | _ -> assert false (* should not occur *)


(* A local shortcut to ease the profile def *)
type sl = Data.subst list

(* Reconstruct the flattenned data *)
let (unexpand_profile : sl -> Soc.var list -> sl) =
  fun sl vl -> 
    let rec (aux : sl -> sl -> Soc.var list -> sl * sl)=
      fun sl_done sl_todo vl -> 
        (* Returns the (accumulated) result and the unused subst
           (which should be empty at the top-level call) *)
        match sl_todo, vl with
          | [],_::_ -> sl_done,[] 
          | _,[] -> sl_done, sl_todo
          | s::sl, (_, (Bool| Int | Real | Extern _ | String ))::vl ->
            aux (s::sl_done) sl vl
          | (id,v)::sl, (_,Enum(_n,el))::vl -> 
            let s =  (id, int_to_enum v el) in
            aux (s::sl_done) sl vl 
          | _, (vn, Array(vt,i))::vl -> (
            let sl_todo_ref = ref sl_todo in
            let sl_done_ref = ref [] in
            let a_fake_value = I 42 in
            let res = Array.make i a_fake_value in
            for k=0 to i-1 do
              let (vk_l:Soc.var list) = [("fake_name",vt)] in
              let (sl_done_v, sl_todo_v) = aux !sl_done_ref !sl_todo_ref vk_l in
              sl_todo_ref:=sl_todo_v;
              sl_done_ref:=sl_done_v;
              Array.set res k (snd (List.hd !sl_done_ref));
            done; 
            let sl_done = (vn, A res)::sl_done in
            aux sl_done !sl_todo_ref vl
          )
          | _, (vn,Struct(_sn,fl))::vl -> 
            let sl_todo, fl = List.fold_left aux_field (sl_todo,[]) fl in
            let sl_done = (vn, S fl)::sl_done in
            aux sl_done sl_todo vl

          | _, (_vn,Alpha _  )::_ -> assert false (* should not occur *) 
          | _, (_, Alias _)::_ -> assert false (* should not occur *) 

    and (aux_field : sl * (ident * Data.v) list -> ident * Data.t -> sl * (ident * Data.v) list ) =
      fun (sl_todo, fl) (fn, t) ->
        let new_sl_done, sl_todo = aux [] sl_todo [fn,t] in
        let (_,v) = List.hd new_sl_done in
        sl_todo, (fn,v)::fl

    in
    let res, remaining = aux [] sl vl in
    assert (remaining=[]);
    res
