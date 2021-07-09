(* Time-stamp: <modified the 27/08/2019 (at 16:00) by Erwan Jahier> *)

open Mypervasives

type t = {
  tab : bool StringMap.t;
  preambule : string;
  file_name : string
}

let (init_t : (string * bool) list -> bool StringMap.t) = 
  fun sl -> 
    List.fold_left (fun acc (vn,value) -> StringMap.add vn value acc) StringMap.empty sl

let (get_file_name : t -> string) =
  fun cov -> 
  cov.file_name
    
let (init : string list -> string -> bool -> t) =
  fun oracle_names cov_file reinit_cov -> 
    if not (Sys.file_exists cov_file)  then 
      { 
        tab = init_t (List.map (fun x -> x,false) oracle_names);
        preambule = "";
        file_name = cov_file
      }
    else
      let str = readfile cov_file in
      let rec loop_rif acc i = 
        try
          let i = Str.search_forward (Str.regexp "RIF: ") str i in
          let j = Str.search_forward (Str.regexp "\n") str i in
            loop_rif (acc ^ (String.sub str i (j-i+1))) (j+1)
        with 
            Not_found -> acc
      in
      let rec loop_var cov i =
        try
          let i = 5+Str.search_forward (Str.regexp "VAR: ") str i in
          let j = Str.search_forward (Str.regexp " ") str i in
          let var_name = String.sub str i (j-i) in
          let status = (String.sub str (j+1) 1) in
            loop_var ((var_name, if reinit_cov then false else status= "t")::cov) (j+1)            
        with 
            Not_found -> cov
          | Invalid_argument _ -> failwith ("File " ^ cov_file ^ " is not correctly formatted.
It should looks like:
VAR: var_name1 t
VAR: var_name2 t
VAR: var_name3 f
")
      in
        { 
          tab = init_t (loop_var [] 0);
          preambule = if reinit_cov then "" else (loop_rif "" 0);
          file_name = cov_file
        }



let (compute_stat :  t -> int * int * float ) =
  fun cov -> 
    let i,n = 
      StringMap.fold
        (fun _vn value (i,n) -> if value then (i+1,n+1) else (i,n+1))
        cov.tab
        (0,0)
    in
    let cov_rate = 100. *. (float_of_int i) /. (float_of_int n) in
      n, i, cov_rate



let (update_cov : Data.subst list -> t ->  t) =
  fun substs cov -> 
    let cov =
      List.fold_left 
        (fun cov (vn, value) ->
(*            let vn = node ^ "_" ^ vn in *)
             if value <> Data.B true || not (StringMap.mem vn cov.tab) then 
               cov
             else
               { cov with tab = StringMap.add vn true cov.tab }
        )
        cov
        substs
    in
      cov

let is_bool vv = match vv with Data.B _ -> true | _ -> false

let (dump_oracle_io : Data.subst list -> Data.subst list -> t -> string) =
  fun inputs substs cov -> 
    let bools, nums = List.partition (fun (_vn,vv) -> is_bool vv) substs in
    let true_bools, false_bools = List.partition (fun (_vn,vv) -> Data.B true = vv) bools in
    let true_other, true_first = 
      List.partition
        (fun (vn,_vv) -> try StringMap.find vn cov.tab with _  -> true) true_bools
    in
    let pn (vn,_vv) = Printf.sprintf "%s" vn in
    let pv (vn,vv) = Printf.sprintf "%s=%s" vn (Data.val_to_string string_of_float vv) in
    let true_first_str = String.concat "\n\t" (List.map pn true_first) in
    let true_other_str = String.concat "\n\t" (List.map pn true_other) in
    let false_bools_str = String.concat "\n\t" (List.map pn false_bools) in
    let nums_str = String.concat "\n\t" (List.map pv nums) in
    let inputs_str = String.concat "\n\t" (List.map pv inputs) in
        (if true_first_str = "" then "" else
           Printf.sprintf "Boolean oracle outputs true for the first time: \n\t%s\n"  true_first_str) ^
        (if true_other_str = "" then "" else
           Printf.sprintf "Other Oracle outputs that are true: \n\t%s\n"  true_other_str) ^
        (if false_bools_str = "" then "" else
           Printf.sprintf "Oracle outputs that are false: \n\t%s\n"  false_bools_str) ^
        (if nums_str = "" then "" else
           Printf.sprintf "Oracle numeric outputs: \n\t%s\n"  nums_str) ^
        (if inputs_str = "" then "" else
           Printf.sprintf "Oracle inputs: \n\t%s\n"  inputs_str) 


let (dump : string -> string -> t -> unit) =
  fun ecfile riffile cov -> 
    let oc = open_out cov.file_name in
    let time = Unix.localtime (Unix.time ()) in
    let date = (
      (string_of_int time.Unix.tm_mday) ^ "/" ^
	     (string_of_int (time.Unix.tm_mon+1)) ^  "/" ^
	     (string_of_int (1900+time.Unix.tm_year))
    )
    and time_str = (
      (string_of_int time.Unix.tm_hour) ^  ":" ^
        (if time.Unix.tm_min < 10 then "0" else "") ^
        (string_of_int time.Unix.tm_min) ^   ":" ^
        (if time.Unix.tm_sec < 10 then "0" else "") ^
        (string_of_int time.Unix.tm_sec) 
    )
    and hostname = Unix.gethostname ()
    in
    let (_to_cov, _covered, cov_rate)= compute_stat cov in
      Printf.fprintf oc "ORACLE: %s\n" ecfile;
      output_string oc cov.preambule;
      Printf.fprintf oc "RIF: %s # generated at %s the %s on %s ; the coverage rate is %.1f%s\n" 
        riffile time_str date hostname cov_rate "%";
      StringMap.iter
        (fun vn st -> Printf.fprintf oc "VAR: %s %s\n" vn (if st then "t" else "f"))
        cov.tab;
      flush oc;
      close_out oc;


