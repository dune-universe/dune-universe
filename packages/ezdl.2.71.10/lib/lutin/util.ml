(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: util.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Miscellaneous general purposes functions. *)

(* This file probably ougth to be splitted at some point ... *)



(**
  [rm x l] returns [l] without [x].
*)
let rec (rm: 'a -> 'a list -> 'a list) =
  fun x l ->
    match l with
	[] -> []
      | y::t -> (if x = y then (rm x t) else y::(rm x t))



(**
  [rm_first x l] returns [l] without the left-most occurence of [x].
*)
let (rm_first: 'a -> 'a list -> 'a list) =
  fun x l ->
    match l with
	[] -> []
      | y::t -> (if x = y then t else y::(rm x t))

(**
  [list_is_included l1 l2] tests if elements of [l1] are included into [l2].
*)
let rec (list_is_included: 'a list -> 'a list -> bool) =
  fun l1 l2 ->
    match l1 with
	[] -> true
      | (x1::t1) ->
	  ( if (List.mem x1 l2) then
	      list_is_included t1 (rm x1 l2)
	    else
	      false )


(** [count_empty_list llist] counts the number of empty list that
occurs in the list of list [llist] *)
let (count_empty_list : 'a list list -> int) =
  fun llist ->
    let (ell, _) = List.partition (fun l -> l = []) llist in
      List.length ell

let _ = assert ((count_empty_list []) = 0)
let _ = assert ((count_empty_list [[1];[1];[1]]) = 0)
let _ = assert ((count_empty_list [[1];[];[1];[];[2;3]]) = 2)


(** compare 2 elements according to the order defined by a list *)
let (compare_list : 'a list -> 'a -> 'a -> int) =
  fun list a b ->
    let rec compare_list_rec list a b =
      match list with
	  x::tail ->
	    if x = a then -1
	    else if x = b then 1
	    else compare_list_rec tail a b
	| [] -> compare a b
    in
      if a = b then 0 else compare_list_rec list a b

let _ = assert ((compare_list [3; 5; 4] 4 5) > 0)
let _ = assert ((compare_list [3; 5; 4] 4 1) < 0)
let _ = assert ((compare_list [3; 5; 4] 1 11) < 0)


(* [string_to_string_list str] returns the list of substrings of
[str] that are separated by blanks. *)
let (string_to_string_list : string -> string list) =
  fun str ->
    Str.split (Str.regexp "[ \t]+") str

let _ = assert ((string_to_string_list "a aa aaa aa") = ["a";"aa";"aaa";"aa"])



(** Appends two lists and sorts the result *)
let append_and_sort l1 l2 =
  List.sort (compare) (List.rev_append l1 l2)

let append_and_sort_rev l1 l2 =
  List.sort (fun x y -> compare y x) (List.rev_append l1 l2)


(**
  [list_are_equals l1 l2] tests if [l1] and [l2] contains the same elements
  (in any order).
*)
let (list_are_equals: 'a list -> 'a list -> bool) =
  fun l1 l2 ->
    (list_is_included l1 l2 && list_is_included l2 l1)

(**
  [list_intersec l1 l2] returns all the elements that are both in [l1] and [l2].
*)
let (list_intersec: 'a list -> 'a list -> 'a list) =
  fun l1 l2 ->
    List.filter (fun x -> List.mem x l1) l2


(**
  [diff_list_as_set l1 l2] returns all the elements that are in [l1]
  and not in [l2].
*)
let (diff_list_as_set : 'a list -> 'a list -> 'a list) =
  fun l1 l2 ->
    List.filter (fun x -> not (List.mem x l1)) l2

(** Checks is a list is sorted w.r.t. compare *)
let sorted list =
  ((List.sort (compare) list) = list)

(**
  [power a b] returns a to the power of b
*)
let (power_2: int -> int) =
  fun n ->
    let _ = assert (0 <= n) in
      if n = 0 then 1 else
        if (n < (Nativeint.size - 2)) then 2 lsl (n-1)
	else
	  failwith ("*** Too many solutions. ")
(* 		    "try to use --use-big-int or --use-float option instead.\n") *)


let _ = assert (power_2 0 = 1)
let _ = assert (power_2 1 = 2)
let _ = assert (power_2 29 = int_of_float (2. ** 29.))

(*************************************************************************)
let (readfile_rm_crtl_m: string -> string) = 
  fun file -> 
    let remove_control_m str =
      Str.global_replace (Str.regexp "\013") "" str
    in
      remove_control_m (Mypervasives.readfile file)

(*************************************************************************)

let rec (list_iter3: ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list
	   -> 'c list -> unit) =
  fun f l1 l2 l3 ->
    match (l1, l2, l3) with
	([], [], []) -> ()
      | (e1::t1, e2::t2, e3::t3) ->
	  (
	    (f e1 e2 e3);
            (list_iter3 (f) t1 t2 t3);
	  )
      | _ -> failwith ("*** Error: list_iter3 should be called with lists "
		       ^ "of the same size.\n")


let rec list_fold_left3 f accu l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> accu
  | (a1::l1, a2::l2, a3::l3) -> list_fold_left3 f (f accu a1 a2 a3) l1 l2 l3
  | (_, _, _) -> invalid_arg "Util.list_fold_left3"

let rec list_fold_left4 f accu l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
    ([], [], [], []) -> accu
  | (a1::l1, a2::l2, a3::l3, a4::l4) ->
      list_fold_left4 f (f accu a1 a2 a3 a4) l1 l2 l3 l4
  | (_, _, _, _) -> invalid_arg "Util.list_fold_left4"

let rec list_fold_left5 f accu l1 l2 l3 l4 l5 =
  match (l1, l2, l3, l4, l5) with
    ([], [], [], [], []) -> accu
  | (a1::l1, a2::l2, a3::l3, a4::l4, a5::l5) ->
      list_fold_left5 f (f accu a1 a2 a3 a4 a5) l1 l2 l3 l4 l5
  | (_, _, _, _, _) -> invalid_arg "Util.list_fold_left5"



let rec (list_map3: ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list
	   -> 'c list -> 'd list) =
  fun f l1 l2 l3 ->
    match (l1, l2, l3) with
	([], [], []) -> []
      | (e1::t1, e2::t2, e3::t3) ->
	  (f e1 e2 e3)::(list_map3 f t1 t2 t3)
      | _ -> failwith ("*** Error: list_map3 should be called with lists "
		       ^ "of the same size.\n")

let rec (list_map4: ('a -> 'b -> 'c -> 'd -> 'e) -> 'a list -> 'b list
	   -> 'c list -> 'd list -> 'e list) =
  fun f l1 l2 l3 l4 ->
    match (l1, l2, l3, l4) with
	([], [], [], []) -> []
      | (e1::t1, e2::t2, e3::t3, e4::t4) ->
	  (f e1 e2 e3 e4)::(list_map4 f t1 t2 t3 t4)
      | _ -> failwith ("*** Error: list_map4 should be called with lists "
		       ^ "of the same size.\n")
	
let rec (list_map6: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a list -> 'b list
	   -> 'c list -> 'd list -> 'e list -> 'f list -> 'g list) =
  fun f l1 l2 l3 l4 l5 l6->
    match (l1, l2, l3, l4, l5, l6) with
	([], [], [], [], [], []) -> []
      | (e1::t1, e2::t2, e3::t3, e4::t4, e5::t5, e6::t6) ->
	  (f e1 e2 e3 e4 e5 e6)::(list_map6 f t1 t2 t3 t4 t5 t6)
      | _ -> failwith ("*** Error: list_map6 should be called with lists "
		       ^ "of the same size.\n")

let rec (list_split3: ('a * 'b * 'c) list -> 'a list * 'b list * 'c list) = 
  function
    | [] -> ([], [], [])
    | (x,y,z)::l ->
        let (rx, ry, rz) = list_split3 l in (x::rx, y::ry, z::rz)

let rec (list_split4: ('a * 'b * 'c * 'd) list -> 'a list * 'b list * 'c list * 'd list) = 
  function
    | [] -> ([], [], [], [])
    | (x,y,z,t)::l ->
        let (rx, ry, rz, rt) = list_split4 l in (x::rx, y::ry, z::rz, t::rt)

let rec (list_split6: ('a * 'b * 'c * 'd * 'e * 'f) list -> 
          'a list * 'b list * 'c list * 'd list * 'e list * 'f list) = 
  function
    | [] -> ([], [], [], [], [], [])
    | (x,y,z,t,u,v)::l ->
        let (rx, ry, rz, rt, ru, rv) = list_split6 l in (x::rx, y::ry, z::rz, t::rt, u::ru,v::rv)

let rec (list_split7: ('a * 'b * 'c * 'd * 'e * 'f * 'g) list -> 
          'a list * 'b list * 'c list * 'd list * 'e list * 'f list * 'g list) = 
  function
    | [] -> ([], [], [], [], [], [], [])
    | (x,y,z,t,u,v,w)::l ->
        let (rx, ry, rz, rt, ru, rv, rw) = 
          list_split7 l in (x::rx, y::ry, z::rz, t::rt, u::ru, v::rv, w::rw)

	

(** checks that a list does not contain any duplicate *)
let rec (no_dup : 'a list -> 'a option) =
  fun list ->
    match list with
	[] -> None
      | elt::tail ->
	  if List.mem elt tail then Some elt
	  else no_dup tail

(** Removes duplicates from a list (conserving its order) *)
let (rm_dup : 'a list -> 'a list) =
  fun list ->
    let rec aux acc list =
    match list with
	   | [] -> List.rev acc
      | elt::tail ->
	     if List.mem elt acc then aux acc tail
	     else aux (elt::acc) tail
    in
    aux [] list


(** Sorts a list of pair which lhs is a string lexicographically
  w.r.t. to this string.
*)
let (sort_list_string_pair:  (string * 'a) list -> (string * 'a) list) =
  fun var_list ->
    List.sort (fun (vn1, _t1) (vn2, _t2) -> compare vn1 vn2) var_list


(** Merges two lists without introducing duplicates, respecting the
  original elements ordering.
*)
let rec (merge: 'a list -> 'a list -> 'a list) =
  fun list1 list2 ->
    match list1 with
	[] -> list2
      | x1::t1 ->
	  if (List.mem x1 list2)
	  then merge t1 list2
	  else merge t1 (List.append list2 [x1])

let _ = assert (list_are_equals (merge [2;3;4] [4;1;5;6]) [1;2;3;4;5;6])
let _ = assert (list_are_equals (merge [] [4;1;5;6]) [1;4;5;6])

(**
  [unfold f i n] calls [n] times [f i] and returns the [n] results into
  a list, e.g., [[(f i); (f i); ...; (f i)]]. Of course, this makes
  more sense if f is not side effects free.
*)
let rec (unfold: ('a -> 'b) -> 'a -> int -> 'b list) =
  fun f i n ->
    assert (n >= 0);
    unfold_do f i n

and unfold_do f i =
  function
      0 -> []
    | n -> (f i)::(unfold f i (n-1))

let _ = assert ((unfold (fun x -> x+1) 0 5) = [1; 1; 1; 1; 1])
	

(**
 [call_n_times f i n] applies [f] [n] times , e.g.,  [f(f(f(f...(f i))))].
*)
let rec (call_n_times: ('a -> 'a) -> 'a -> int -> 'a ) =
  fun f i ->
    function
	0 -> i
      | n -> (f (call_n_times f i (n-1)))

let _ = assert ((call_n_times (fun x -> x+1) 0 5) = 5)


(** Returns the size of an Hashtbl. *)
let (hashtbl_size: ('a, 'b) Hashtbl.t -> int) =
  fun tbl ->
    Hashtbl.fold (fun _ _ cpt -> cpt+1) tbl 0



(************************************************************************)


(** [is_substring ss s] test whether [ss] a sub-string of [s] *)

(* Why isn't that £@!ù&^$¤! of ocaml String lib does not contain that
   one ??? I know it is not very fast, but still...*)
let (is_substring: string -> string -> bool) =
  fun ss s ->
    let reg = Str.regexp_string ss in
      try
	let _ = Str.search_forward reg s 0 in
	  true
      with Not_found -> false


(** [gauss_draw m d] generates a gaussian pseudo-random number of
mean [m] and deviation [d]. The generation algorithm is based on the
so-called << polar form of the Box-Muller transformation >> that I
found it at the url http://www.taygeta.com/random/gaussian.html
*)
let rec (gauss_draw : float -> float -> float) =
  fun m d ->
    let x1 = ((Random.float 2.) -. 1.)
    and x2 = ((Random.float 2.) -. 1.) in
    let w = x1 *. x1 +. x2 *. x2 in
      if w >= 1.
      then gauss_draw m d
      else
	let w2 = sqrt( (-2.0 *. (log w) ) /. w ) in
	  (x1 *.w2 *. d) +. m

(** Ditto, but returns 2 numbers. Indeed, the Box-Muller algorithm
  computes 2 of them anyway... *)
let rec (gauss_draw2 : float -> float -> float * float) =
  fun m d ->
    let x1 = ((Random.float 2.) -. 1.)
    and x2 = ((Random.float 2.) -. 1.) in
    let w = x1 *. x1 +. x2 *. x2 in
      if w >= 1.
      then gauss_draw2 m d
      else
	let w2 = sqrt( (-2.0 *. (log w) ) /. w ) in
	let y1 = (x1 *.w2 *. d) +. m
	and y2 = (x2 *.w2 *. d) +. m in
	  (y1, y2)

	
(****************************************************************************)
(** Map of strings *)
module StringMap = struct
  include Map.Make(struct type t = string let compare = compare end)
end

(* archaic profile of tbl size

commented out because StringMap.cardinal is defiend since 3.12 only !

let mfind k m = 
  let c = StringMap.cardinal m in 
  if c > 1000 then (print_int c; print_string " ");
  StringMap.find k m
*)
let mfind = StringMap.find

let hfind = Hashtbl.find 

(****************************************************************************)
(* ZZZ ATTENTION : ne pas changer l'un sans changer l'autre !!!!*)
(* en d'autres termes, utiliser change_precision pour changer la precision !!! *)
let precision = ref 2 (* ZZZ *)
let eps = ref 0.01 (* ZZZ *)
(* ZZZ *)
let (update_eps : unit -> unit) =
  fun _ ->
    eps := 1.0 /. (10.0**(float_of_int (!precision)))

(* let (update_precision : unit -> unit) = *)
(*   fun _ ->  *)
(*     precision := int_of_float (log10 (1.0 /. !eps)) *)

let change_precision p =
  precision := p;
  update_eps ()

(****************************************************************************)
(**
   I define my own version of print_float to turn around a bug (or is
   it a bug in ocaml?) of sim2chro where it does not understand floats
   without digit (e.g., 4. instead of 4.0)
*)

(* format_float is not exported in stdlib.mli 
   and its name change in ocaml version 3.08...*)

external format_float: string -> float -> string = "caml_format_float" 
(* external format_float: string -> float -> string = "format_float"   *)
  

let my_string_of_float f = 
  let precision_str = string_of_int !precision in 
    format_float ("%." ^ precision_str ^ "f") f 

let my_string_of_float_precision f p = 
  let precision_str = string_of_int p in 
    format_float ("%." ^ precision_str ^ "f") f 



(* external string_length : string -> int = "%string_length" *)
(* let my_string_of_float f =  *)
(*   let s = format_float "%.12g" f in  *)
(*   let l = string_length s in  *)
(*   let rec loop i =  *)
(*     if i >= l then s ^ ".0" else  *)
(*       (*                ^^^ I have added a "0" there... *)  *)
(*     if s.[i] = '.' || s.[i] = 'e' then s  *)
(*     else loop (i+1)  *)
(*   in  *)
(*   loop 0  *)


let my_print_float f _p = output_string stdout (my_string_of_float f)

	  
(** returns the extension of a filename *)
let (get_extension : string -> string) =
  fun file ->
    let base = Filename.basename file in
      try
	let i = String.index base '.' in
	  String.sub base (i) ((String.length base) - i)
      with Not_found -> ""

let _ = assert ((get_extension "$HOME/toto.ml") = ".ml")


let chop_ext_no_excp file =
  try Filename.chop_extension file with _ -> file


let (remove_extension : string -> string) =
  fun str ->
    let file_ext = Filename.basename str
    and dir  = Filename.dirname str in
    let file = try Filename.chop_extension file_ext with _ -> file_ext in
      if dir = "." then file else (Filename.concat dir file)

let _ = assert ((remove_extension "../toto/tutu.lus") = "../toto/tutu")
let _ = assert ((remove_extension "/home/toto/tutu.lus") = "/home/toto/tutu")
let _ = assert ((remove_extension "home/toto/tutu.lus") = "home/toto/tutu")
let _ = assert ((remove_extension "home/toto/tutu") = "home/toto/tutu")

let (cartesian_product : 'a list -> 'a list -> ('a -> 'a -> 'b) -> 'b list) =
  fun l1 l2 op ->
    if l1 = [] then [] else if l2 = [] then [] else
      (*     if l1 = [] then l2 else if l2 = [] then l1 else *)
      (List.flatten
	      (List.map
	         (fun x ->
	            (List.map
		            (fun y -> op x y)
		            l1
	            )
	         )
	         l2
	      )
      )


let _ = assert(
  let l1 = [1; 1; 1; 2] and l2 = [3; 5; 7] in
    (cartesian_product l1 l2 (fun x y  -> x*y)) =
       [3; 3; 3; 6; 5; 5; 5; 10; 7; 7; 7; 14]
)

let rec (union_find : ('a list -> 'a list -> bool) -> 'a list list ->
	   'a list list) =
  fun p ll ->
    (* [union_find p ll] group in the same sub-list elements for which p is true *)
    (* Check out the real "union-find" algorithm for doing that more efficiently *)
    match ll with
      | [] -> []
      | [_] -> ll
      | cl::tail ->
	  let ll1, ll2 = List.partition (p cl) tail in
	    if
	      ll1 = []
	    then
	      cl::(union_find p ll2)
	    else
	      union_find p (List.rev_append cl (List.flatten ll1) :: ll2)


let _ = assert (							
  (union_find
     (fun l1 l2 -> List.exists (fun x -> List.mem x l1) l2)
     [[1];[2];[13];[4];[4;5];[6;1];[4;7;6]]
  )
  = [[6; 7; 4; 1; 6; 1; 4; 4; 5]; [2]; [13]]
)


(* 
   Used in LucProg to guess the line and col number from the char number. 
   Takes into account the line number pragma generated by cpp.
*)
let rec (from_char_pos_to_line_and_col : string -> int -> bool -> string * string) =
  fun str c count_cpp ->
    let s = Stream.of_string str in
    skip_cr s c 1 0 count_cpp


and skip_cr s c line col count_cpp =
  if
    Stream.count s >= c
  then
    (string_of_int line, string_of_int col)
  else 
    let current = (Stream.next s) in
    if 
	   (* do not take into account lines added by cpp *)
	   current = '#' && count_cpp
    then
	   (
	     let line_new = get_new_line_number s line in
	     skip_cr s c line_new 0 count_cpp
	   )
    else if
	   current = '\n'
    then
	   skip_cr s c (line+1) 0 count_cpp
    else
	   skip_cr s c line (col+1) count_cpp

and get_new_line_number s line =
  let chars = Stream.npeek 30 s in
  match (search_int chars) with
  | Some i -> (i-1)
  | None  -> line


and search_int chars =
  match chars with
  | 'l'::'i'::'n'::'e'::' '::tail -> search_int_acc "" tail
  | _ -> None

and search_int_acc acc chars =
  match chars with
  | [] -> None
  | ' '::_ -> Some (int_of_string acc)
  | x::tail -> 
	 let i = (int_of_char x) - 48 in
	 if 
	   i < 0 || i > 9 
	 then
	   None
	 else
	   search_int_acc (acc ^ (string_of_int i)) tail

(* use to perform system calls *)  
type my_create_process_result =
    OK
  | KO
  | PID of int (* if called with ~wait:false *)

let (my_create_process : ?std_in:(Unix.file_descr) -> ?std_out:(Unix.file_descr) ->
      ?std_err:(Unix.file_descr) ->
      ?wait:(bool) -> string -> string list -> my_create_process_result) =
  fun ?(std_in = Unix.stdin) ?(std_out = Unix.stdout) ?(std_err = Unix.stderr)
    ?(wait = true) prog args -> 
      try
        let pid = 
	       List.iter (fun x -> output_string stderr (x ^ " ")) (prog::args);
	       output_string stderr "\n";
	       flush stderr;
	       Unix.create_process
	         prog
	         (Array.of_list (prog::args))
	         (std_in)
	         (std_out)
	         (std_err)
        in
	       if not wait then PID pid else 
	         let (_,status) = (Unix.waitpid [Unix.WUNTRACED] pid) in
	           ( match status with 
		              Unix.WEXITED i -> 
		                if i = 0 || i = 1 then
		                  (
			                 output_string stderr ("     ... " ^ prog ^ " exited normally.\n");
			                 flush stderr;
			                 OK
		                  )
		                else
		                  (
			                 output_string stderr (
                            "*** Error: " ^ prog ^ " exited abnormally (return code=" ^ 
                              (string_of_int i)^").\n");
			                 flush stderr;
			                 KO
		                  )
                  | Unix.WSIGNALED i-> 
		                output_string stderr ("*** Error: " ^ prog ^ " process was killed by signal " ^ 
                                              (string_of_int i)^"\n");
		                flush stderr;
		                KO
                  | Unix.WSTOPPED i -> 
		                output_string stderr ("*** Error: " ^ prog ^ " process was stopped by signal " ^ 
                                              (string_of_int i)^"\n");
		                flush stderr;
		                KO
	           )
      with 
        | Unix.Unix_error(error, name, arg) -> 
	         let msg = ( "*** '" ^
			                 (Unix.error_message error) ^
			                 "'in the system call: '" ^ name ^ " " ^ arg ^ "'\n")
	         in
	           output_string stdout msg;
	           flush stdout;
	           output_string stderr msg;
	           flush stderr;
	           KO
        | e -> 
	         output_string stdout (Printexc.to_string e);
	         flush stdout;
	         output_string stderr (Printexc.to_string e);
	         flush stderr;
	         KO

(************************************************************************)
(** [gv ps_file] calls the post-script visualizer [gv] on [ps_file]. *)
let (gv: string -> unit) =
  fun ps_file ->
    let ps_viewer =
      try Unix.getenv "PS_VIEWER"
      with _ ->
	     print_string "*** Can not find PS_VIEWER env variable; using gv\n";
	     flush stdout;
	     "gv"
    in
      ignore (my_create_process ps_viewer [ps_file])

(************************************************************************)
(** [pdf file] calls the pdf visualizer [xpdf] on [file]. *)
let (pdf: string -> unit) =
  fun file ->
    let viewer =
      try Unix.getenv "PDF_VIEWER"
      with _ ->
	     print_string "*** Can not find PDF_VIEWER env variable; using xpdf\n";
	     flush stdout;
	     "xpdf"
    in
      ignore (my_create_process viewer [file])



(************************************************************************)
(** [dot dot_file ps_file] calls dot on [dot_file] and produce its
 result in [pd_file] *)
let (dot : string -> string -> int) =
  fun dot_file pdf_file ->
    try
      let dot_exe = try Unix.getenv "DOT" with _ -> "dot" in
	     ignore (my_create_process ~wait:true dot_exe 
		            ["-Tpdf"; dot_file; "-o"; pdf_file]);
	     0
    with e -> 
      print_string (Printexc.to_string e);
      flush stdout;
      1


let (get_fresh_dir : string -> string) =
  fun host ->
    let dir = Filename.temp_file "lurette" "" in
      Sys.remove dir;
      Unix.mkdir dir 0o755; 
      Unix.mkdir (Filename.concat dir host) 0o755;
      dir

let exe () = 
  match Sys.os_type with
    | "Win32"  -> ".exe"
    | _ -> ""



(****************************************************************************)
(* Portable File utility *)
(* I could also use ocaml-fileutils (???) 
   seems to be overkill just to have cp and rm on unix and win32...
*)

(* type hostkind = Unix | Win32  *)
(*  *)
(* let hostkind =  *)
(*   match Unix.getenv "HOST_TYPE" with *)
(*     | "i386-linux-gcc3" -> Unix *)
(*     | "i386-linux" -> Unix *)
(*     | "sparc-sun" -> Unix *)
(*     | "cygwin" -> Unix *)
(*     | "win32" -> Win32 *)
(*  *)
(* let (cp : string -> string -> unit) = *)
(*   fun file dest ->  *)
(*     let cmd = *)
(*       if hostkind = Win32 then *)
(* 	( *)
(* 	   *)
(* 	) *)
(*       else *)
(* 	( *)
(* 	  "cp " ^ file ^ " " ^ dest *)
(* 	) *)
(*     in *)
(*       if Sys.command cmd  <> 0 then *)
(* 	( *)
(* 	  print_string (cmd ^ "\n file copying command failed.\n"); *)
(* 	  flush stdout *)

	
(****************************************************************************)

let (tabulate_result : ('a, 'b) Hashtbl.t ref -> int ref -> int -> ('a -> 'b)
       -> 'a -> 'b) =
  fun tbl tbl_size max_size f arg ->
    try
      Hashtbl.find !tbl arg
    with Not_found ->
      let res = f arg in
	if
	  !tbl_size > max_size
	then
	  (* to avoid table size explosion *)
	  (
	    tbl_size := 0;
	    Hashtbl.clear !tbl
	  );
	Hashtbl.add !tbl arg res;
	tbl_size := !tbl_size + 1;
	res

let print_fl std fl =
  List.iter
    (fun xi -> output_string std ((string_of_float xi) ^ " "))
    fl

let print_fll std fll =
  List.iter
    (fun fl ->
       output_string std "\n\n";
       print_fl std fl
      )
    fll

(********************************************************************)

let rec transpose m =
  assert (m <> []);
  if List.mem [] m then [] else
    try 
      List.map List.hd m :: transpose (List.map List.tl m)
    with _ -> assert false

(********************************************************************)

let safe_remove_file file =
  try Sys.remove file
  with Sys_error _ -> ()


(********************************************************************)


let entete comment_b comment_e =
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
    (comment_b ^ " Automatically generated by "^ 
       Sys.executable_name^" version "^Version.str^" (\"" ^Version.sha^"\")"^comment_e ^"\n" ^ 
       comment_b ^ " on " ^ hostname ^ 
       " the " ^ date ^ " at " ^ time_str ^ comment_e ^"\n" ^
       comment_b^(String.concat " " (Array.to_list Sys.argv))^ comment_e ^"\n\n")


let generate_up_and_down_macro dir =
    let file = (Filename.concat dir "up_and_down_macro") in
    let oc = open_out file in
      output_string oc "// % -*- mode: C; c-mode -*-

// use cpp to make Var goes up and down (and so on) between Min and Max, 
// with a derivative bounded by Delta

#define up_and_down(Var, Min, Max, Delta) ( \\
    (if \\
        (pre Var < Min) or ((pre Var < Max) and (pre pre Var <= pre Var)) \\
     then \\
          (Var > pre Var) \\
     else \\
          (Var < pre Var) \\
     ) \\
     and (abs(Var - pre Var) < Delta) \\
 )

";
      flush oc;
      close_out oc

  
let (rm_dir : out_channel -> string -> unit) =
  fun oc dir ->
    let cmd = 
      if Sys.os_type = "Win32" then
	     "rmdir /S /Q " ^ dir
      else
	     "rm -rf " ^ dir
    in
      output_string oc cmd;
      output_string oc "\n";
      flush oc;
      if Sys.command cmd  <> 0 then
	     (
	       output_string oc (cmd ^ " command failed.\n");
	       flush oc
	     ) 
      else 
	     ()

(* get the io name and type from a ec or a lustre file *)
let (get_io_from_lustre : string -> string option -> 
     (string * string) list * (string * string) list) =
  fun file node_opt -> 
    try
      let str = readfile_rm_crtl_m file in
      let i = 
        match node_opt with
          None -> 
          (* If no node is specified, we take the first one *)
          Str.search_forward (Str.regexp ("^node[ \n\t]+")) str 0 
        | Some node -> Str.search_forward (Str.regexp ("^node[ \n\t]+"^node)) str 0
      in
      let i1 = 1+Str.search_forward (Str.regexp "(") str (i+(String.length (Str.matched_string str))) in
      let j1 = Str.search_forward (Str.regexp ")") str i1 in
      let i2 = 1+Str.search_forward (Str.regexp "(") str j1 in
      let j2 = Str.search_forward (Str.regexp ")") str i2 in
      let _remove_comment str =
        let comment1 = Str.regexp "--[.]*\n" 
        and comment2 = Str.regexp ".(\\*[^\\*]*\\*)" 
        in
        let str =  Str.global_replace comment1 "" str in
        let str =  Str.global_replace comment2 "" str in
        str
      in
      let input_str = String.sub str i1 (j1-i1) in
      (*       let input_str = remove_comment input_str in *)
      let output_str = String.sub str i2 (j2-i2) in
      (*       let output_str = remove_comment output_str in *)
      let get_io_from_str s =
        let decls = Str.split (Str.regexp ";") s in
        let rm_blank s = 
          let buff = ref "" in
          for i = 0 to String.length s - 1 do
            match s.[i] with
            | ' ' | '\t' | '\n' -> ()
            | c -> buff:=!buff^(String.make 1 c)
          done;
          !buff        
        in
        let decl_to_pair s = 
          match Str.split (Str.regexp ":") s with
          | [n;t] -> rm_blank n, rm_blank t
          | _ -> failwith ("Cannot split '"^s^"'")
        in
        let decls = List.filter (is_substring ":") decls in 
        let io = List.map decl_to_pair decls in
        let expand_decl (n,t) =
          let lv = Str.split (Str.regexp ",") n in
          List.map (fun v -> v,t) lv
        in
        let _ = assert (
          expand_decl ("T,T1,T2","real") = ["T","real"; "T1","real"; "T2","real"])
        in
        let io = List.flatten (List.map expand_decl io) in
        io
      in
      get_io_from_str input_str, get_io_from_str output_str

    with Not_found -> 
      print_string ("Error when searching for the I/O "^
                    (match node_opt with None -> "" | Some node -> " of node "^node) ^
                    " in file '"^file^
                    "'\n  Is '"^file ^"' a syntactically correct file?\n");
      flush stdout;
      exit 2

(***************************************************************************)
let overflow_msg str = 
    Printf.eprintf "Fail to convert into an int the string '%s'.\n" str;
    Printf.eprintf "Hint: use bounds when declaring output or local ";
    Printf.eprintf "vars (e.g., 'x : real [-10000.0; 10000.0]')\n";
    flush stderr
    
let int_of_string str =
  try int_of_string str 
  with _  -> 
    let msg = Printf.sprintf "Fail to convert into an int the string '%s'.\n" str in
      overflow_msg msg;
      exit 2
        
let int_of_num n =
  try Num.int_of_num n 
  with _  -> 
    let str = Num.string_of_num n in
    let msg = Printf.sprintf "Fail to convert into an int the num '%s'.\n" str in
      overflow_msg msg;
      exit 2
   

(* Cloned from the OCaml stdlib Arg module: I want it on stdout! (scrogneugneu) *)
let usage_out speclist errmsg =
  Printf.printf "%s" (Arg.usage_string speclist errmsg)

(* The behavior of ocaml wrt non representable int is arguably wrong.
   It would make more sense to reason modulo 2^(max_int-1) instead of
   raising a fatal error! Here I try to mimick the expected behavior
   as much as possible.
*)

let big_max_int = Big_int.big_int_of_nativeint Nativeint.max_int
(* let big_min_int = Big_int.big_int_of_nativeint Nativeint.min_int *)
let my_int_of_string str = 
  try int_of_string str
  with _ ->
    let big_i = Big_int.big_int_of_string str in
    try 
      let big_i = Big_int.mod_big_int big_i big_max_int in
      Nativeint.to_int (Big_int.nativeint_of_big_int big_i)
    with _ -> 
      assert false (* Should not occur !!!! 

The following ran for hours...

let x = ref Big_int.zero_big_int;;
while true do 
  let i1 = (my_int_of_string (Big_int.string_of_big_int (!x))) in
  let i2 = (my_int_of_string (Big_int.string_of_big_int (Big_int.minus_big_int !x))) in
  if i1 = 0 then (print_string "coucou\n"; flush stdout);
  x := Big_int.succ_big_int !x;
  x := Big_int.mult_big_int (Big_int.big_int_of_int ((Random.int 1001) - 500 )) !x ;
done
        
*)
