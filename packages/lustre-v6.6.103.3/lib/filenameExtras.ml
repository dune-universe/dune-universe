(* Time-stamp: <modified the 26/02/2015 (at 09:53) by Erwan Jahier> *)


let (to_list : string -> string list) =
  fun f -> 
    let rec aux acc f =
      let dir = Filename.dirname f
      and base = Filename.basename f in
        if dir = f then dir::acc else
          aux (base::acc) dir
    in
      aux [] f
        

(* exported *)
let (simplify : string -> string) =
  fun f -> 
    let rec simplify_aux l =
      match l with
        | []
        | _::[] -> l
        | x1::x2::tail -> 
            if x1 = Filename.current_dir_name then
              simplify_aux (x2::tail)
            else if x1 = Filename.parent_dir_name then 
              (* x1=.. -> nothing to do *)
              x1 :: (simplify_aux (x2::tail))
            else if x2 = Filename.parent_dir_name then
              (* "x1/.." -> we simplify into "" *)
              simplify_aux tail
            else
              (* x2 maybe also be ".." after simplifications -> fixpointing *)
              match simplify_aux (x2::tail) with
                | [] -> [] (* dead code *)
                | head::tail -> 
                    if head = Filename.parent_dir_name then 
                      tail (* bingo! we simplify *)
                    else
                      x1::head::tail
    in
    let l = to_list f in
    let l = 
      match l with
        | [] -> []
        | head::tail -> 
            (* simplify_aux removes all the "." in the path, so we preserve the first
               one here if necessary *)
            if head = Filename.current_dir_name then  
              head::(simplify_aux tail) 
            else
              simplify_aux l
    in
      (* build the filename back *)
      List.fold_left (fun x acc -> Filename.concat x acc) (List.hd l) (List.tl l)


(* A few unit tests *)
let _ = if Sys.os_type <> "Win32" then ( (* fails on win32 because of path *)
  assert(simplify "/home/name/dir/file" = "/home/name/dir/file");
  assert(simplify "/home/name/dir/../file" = "/home/name/file");
  assert(simplify "/home/name/dir/../../file" = "/home/file");
  assert(simplify "/home/./name/././././dir/.././../file" = "/home/file");
  assert(simplify "" = "./."); (* hum, that one is not simpler... *)
  assert(simplify "./a/b/../../../x" = "./../x")
)
