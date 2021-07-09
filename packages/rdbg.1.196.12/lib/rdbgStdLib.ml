(* Time-stamp: <modified the 28/06/2021 (at 10:36) by Erwan Jahier> *)
open RdbgEvent



(******************************************************************)
(* print_event stuff *)
let show_trace = ref true
let show_data = ref true
let show_src = ref false

let pf = Printf.printf

let src_info_atoms2str (si:src_info_atom) = 
  let rec aux pref si =
    Printf.sprintf "file:%s::%i (%i-%i) '%s'\n" si.file
      (fst si.line) (fst si.char) (snd si.char) si.str 
    ^ (match si.stack with
      | None -> ""
      | Some si ->  aux (pref ^ "   ") si)
  in
    aux "" si
let print_src_info_atoms (si:src_info_atom) = 
  let rec aux pref si = pf "file:%s::%i (%i-%i) '%s'\n" si.file
      (fst si.line) (fst si.char) (snd si.char) si.str ;
    match si.stack with
      | None -> ()
      | Some si ->  aux (pref ^ "   ") si
  in
    aux "" si

let (sinfo_to_string : RdbgEvent.t -> string) = 
  fun e -> 
    match e.sinfo with
      | None  -> ""
      | Some si -> String.concat "&" (List.map (fun sia->sia.str) (si()).atoms)

let (_sinfo_to_string2 : RdbgEvent.t -> string) = 
  fun e ->
    match e.sinfo with
      | None  -> ""
      | Some si -> String.concat "&" (List.map src_info_atoms2str (si()).atoms)
 
(* call stack *)
let (print_src : RdbgEvent.t -> unit) = 
  fun e -> 
    match e.sinfo with
      | None  -> ()
      | Some si -> List.iter print_src_info_atoms (si()).atoms


let (print_call_stack : RdbgEvent.t -> unit) = print_src
                             
let (kind2str: RdbgEvent.kind -> string) =
  function
    | Ltop -> "top " 
    | Call -> "call"
    | Exit -> "exit"
    | MicroStep str -> str

(* Which one should I use? *)
let my_string_of_float = string_of_float
(* let my_string_of_float = Mypervasives.my_string_of_float *)

let string_of_event (e:RdbgEvent.t) =
  let remove_vars l =
    List.filter
      (fun (vn,_) -> vn.[0] <> '_' &&
        (String.length vn < 4 || String.sub vn 0 4 <> "pre_")) l
  in
  let vals = if not !show_data then "" else 
      match e.kind with
      | Call ->
        let l = List.filter (fun (id,_) ->      List.mem_assoc id e.inputs) e.data in
        let l = snd (List.split l) in
        let l = List.map (Data.val_to_string my_string_of_float) l in
        let s = String.concat "," l in
        Printf.sprintf "(%s)" s
      | Exit ->
        let l = List.filter (fun (id,_) -> not (List.mem_assoc id e.inputs)) e.data in
        let l = remove_vars l in
        let outs,locs = List.partition (fun (id,_) -> List.mem_assoc id e.outputs) l in
        let outs = snd (List.split outs) in
        let outs = List.map (Data.val_to_string my_string_of_float) outs in
        let outs = String.concat "," outs in
        let locs = List.map
          (fun (vn,v) -> vn^"="^(Data.val_to_string my_string_of_float v)) locs
        in
        let locs = String.concat "," locs in
        if locs = "" then 
          Printf.sprintf "(%s)" outs
        else
          Printf.sprintf "(%s) [%s]" outs locs
      | _ -> 
        let l = remove_vars e.data in
        let l = List.map
          (fun (vn,v) -> vn^"="^(Data.val_to_string my_string_of_float v)) l 
        in
        let s = String.concat "," l in
        Printf.sprintf "[%s]" s
  in
  let blanks = String.make e.depth ' ' in
  let si_str = if !show_src then (" \""^(sinfo_to_string e)^"\"") else "" in
  Printf.sprintf "%3i %3i %s%s %-15s%s%s\n" e.nb e.step blanks
    (kind2str e.kind) e.name vals si_str

let print_event (e:RdbgEvent.t) =
  print_string (string_of_event e);
  flush stdout

(******************************************************************)
(* hooks (functions that are called after each next) *)

let (hooks: (string, (RdbgEvent.t -> unit)) Hashtbl.t) = Hashtbl.create 10

let add_hook = Hashtbl.replace hooks 
let del_hook = Hashtbl.remove hooks
let get_hook = Hashtbl.find hooks 
let del_all_hooks () = Hashtbl.clear hooks
let list_hooks () = Hashtbl.fold (fun s _ acc -> s::acc) hooks []
let _ = add_hook "print_event" (fun e -> if !show_trace then print_event e)

(******************************************************************)
(* A rather naive time travel mechanism. cf mli  *)

let ckpt_rate = ref 100

let check_periodic e = e.nb mod !ckpt_rate = 0 || e.nb = 1
type ckptl = RdbgEvent.t list

let (check_ref : (RdbgEvent.t -> bool) ref) = ref check_periodic
let ckpt_list: ckptl ref = ref []
let debug = false

(* exported *)
let (goto_last_ckpt : int -> RdbgEvent.t) =
  fun i -> 
  (*   Printf.eprintf "===> find_ckpt %i\n" i; *)
  let rec f i el =
    match el with
    | e::nel -> if e.nb < i || nel = [] then (
        ckpt_list:= el;
        e.restore_state e.nb;
        if debug then pf "  -> Goto event %d\n%!" e.nb;
        e
      )
      else f i nel
    | []  ->
      failwith
        "Activate the time travel mode to go backwards (time_travel:=true)"
  in
  f i !ckpt_list

let ckpt_to_str () =
  String.concat "," (List.map (fun e->string_of_int e.nb) !ckpt_list)

let check_point e =
  if !check_ref e then (
    match !ckpt_list with
    | [] ->
       e.save_state e.nb;
       ckpt_list := e::!ckpt_list
    | e1::_ ->
       if e1.nb = e.nb then () else (
         if debug then
           Printf.eprintf "===> Add ckpt at event %i (%s)\n%!" e.nb (ckpt_to_str ());
         e.save_state e.nb;
         ckpt_list := e::!ckpt_list
       )
  )


let (time_travel : bool -> unit) = 
  fun on -> 
    if on then add_hook "time_travel" check_point
          else del_hook "time_travel"

      
(******************************************************************)
(* Moving forward *)

let (next :RdbgEvent.t -> RdbgEvent.t) = 
  fun e -> 
    let ne = e.next () in
     Hashtbl.iter (fun _ f -> f ne) hooks; 
     ne
       
let (next_np : RdbgEvent.t -> RdbgEvent.t) = 
  fun e -> 
  let ne = e.next () in
  Hashtbl.iter (fun str f -> if str<>"print_event" then f ne) hooks; 
  ne


let rec (nexti : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e cpt -> 
    if cpt > 0 then nexti (next e) (cpt-1) else e

let rec (nexti_np : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e cpt -> 
  if cpt > 0 then nexti_np (next_np e) (cpt-1) else e

let rec (next_cond_gen :
           RdbgEvent.t -> (RdbgEvent.t -> bool) -> (RdbgEvent.t -> RdbgEvent.t) ->
           RdbgEvent.t) =
  fun e p next ->
  let ne = next e in
  if p ne then ne else next_cond_gen ne p next

let (next_cond : RdbgEvent.t -> (RdbgEvent.t -> bool) -> RdbgEvent.t) =
  fun e p ->
  next_cond_gen e p next_np


let next_match e str =
  next_cond e (fun e -> Str.string_match (Str.regexp_string str) e.name 0)

let rec (goto_forward : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e i -> 
    let e = if e.nb < i then goto_forward (next_np e) i else (
        e
      )
  in
  e
 
let loopforever e =
  let rec aux e = aux (e.next ()) in
  try aux e
  with RdbgRun.OracleError str ->
    Printf.printf "\027[35m %s \027[00m\n"  str;
    flush stdout
;;

let (step : RdbgEvent.t -> RdbgEvent.t) =
  fun e ->
    let cond ne =
      if ne.step>1+e.step then (
        pf "Did not find the event corresponding to %i in the next step\n" e.nb;
        raise Not_found
      )
      else 
      e.kind=ne.kind && ne.depth=e.depth && e.step+1=ne.step && e.name=ne.name && 
                         (e.lang="lutin" || match e.sinfo, ne.sinfo with
                          Some si, Some nsi -> si()=nsi()| _,_ ->  true)
                          
    in
    try next_cond e cond with Not_found -> e
        
let rec (stepi : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e cpt ->
    if cpt > 0 then stepi (step e) (cpt-1) else e


(* TODO: should try to go backwards if necessary
*)
let rec (goto_s : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e i -> 
    if e.step < i then goto_s (step e) i else e

(******************************************************************)
(* Breakpoints stuff *)

let breakpoints = ref []
let (delete : unit -> unit) =
  fun () -> 
    breakpoints := []

let (break : string -> unit) =
  fun str ->
  breakpoints := str::!breakpoints


let (break_matches : RdbgEvent.t -> string -> bool) =
  fun e b -> 
    (* b ougth to be of the form:
       "node"
       "node@line"
       "file"
       "file@line"
    *)
    let si_match_file str si =
      (si.file = str || Filename.basename si.file = str) 
    in
    let si_match_line ln {line=(d,f);str=_;file=_;char=_;stack=_}  =
      let i = try int_of_string ln with _ -> -1 in
        (d <= i && i <= f)
    in
      match e.sinfo, Str.split (Str.regexp "@") b with
        |  _, [] -> false  (* no more breakpoints *)
        | None, _ -> false (* no source info at the current event *)
        | Some src, str::tail ->
          let src = src() in
            if Sys.file_exists str then
              match tail with
                | [ln] -> 
                    List.exists 
                      (fun si -> si_match_file str si && si_match_line ln si) src.atoms
                | [] ->  List.exists (si_match_file str) src.atoms
                | _  -> false
            else (* str file not exist. Maybey it's a node *)
              e.name = str && 
        (match tail with
           | [] -> true
           | [ln] -> List.exists (si_match_line ln) src.atoms
           | _ -> false         
        )

let (goto_brk : RdbgEvent.t -> string -> RdbgEvent.t) =
  fun e b -> 
    next_cond e (fun e -> break_matches e b)
    

let (continue : RdbgEvent.t -> RdbgEvent.t) =
  fun e -> 
    let stop e = List.exists (break_matches e) !breakpoints in
      next_cond e stop

(******************************************************************)
(* Accessing data *)

let (v:string -> RdbgEvent.t -> Data.v) =
  fun n e  -> 
    if not (List.mem_assoc n e.data) then
      failwith ("No variable named " ^n^" exists")
    else
     List.assoc n e.data

let (vi:string -> RdbgEvent.t -> int) =
  fun n e  -> 
    if not (List.mem_assoc n e.data) then
      failwith ("No variable named " ^n^" exists")
    else
    match List.assoc n e.data with
      | Data.I i -> i
      | _ -> failwith (n^" is not an int")
let (vf:string -> RdbgEvent.t -> float) =
  fun n e -> 
    if not (List.mem_assoc n e.data) then
      failwith ("No variable named " ^n^" exists")
    else
    match List.assoc n e.data with
      | Data.F f -> f
      | _ -> failwith (n^" is not a float")


let (vb:string -> RdbgEvent.t -> bool) =
  fun n e -> 
    if not (List.mem_assoc n e.data) then
      failwith ("No variable named " ^n^" exists")
    else
      match List.assoc n e.data with
        | Data.B b -> b
        | _ -> failwith (n^" is not a bool")


let (run : ?call_hooks:bool -> unit -> RdbgEvent.t) =
 fun ?(call_hooks=true) () ->
 let e = RdbgMain.run () in
  ckpt_list := [];
  check_point e;
  if call_hooks then Hashtbl.iter (fun _ f -> f e) hooks; 
  e
  
(* exported *)
let (rev_cond_gen :
       RdbgEvent.t -> (RdbgEvent.t -> bool) -> (RdbgEvent.t -> RdbgEvent.t) ->
       (int -> unit) -> RdbgEvent.t) =
  fun e p next restore ->
  let goto_ckpt i = let e = goto_last_ckpt i in restore e.nb; e in
  if debug then pf "\n ==> rev_cond_gen e.nb=%i\n%!" e.nb;
  let rec f e max_i = (* rev_cond for an event < max_i *)
    let ckpt = goto_ckpt e.nb in
    if ckpt.nb = e.nb (* e is already the first ckpt *) then e else
      match sback ckpt (e.nb-1) with
      | None -> f ckpt (ckpt.nb -1) 
      | Some ne -> if ne.nb < max_i then sforward ne max_i else ne
  and sback e i =
    assert (e.nb<=i);
    if debug then pf "search backward if there exists e in [%i; %i] s.t. p e\n%!" e.nb i;
    let ne = if p e then e else next_cond_gen e (fun e -> p e || e.nb > i) next in
    if p ne then Some ne else None
  and sforward e max_i =
    if debug then
      pf "event %i is good; is there a good one in ]%i; %i[\n%!" e.nb e.nb max_i;
    if e.nb >= max_i then e else 
      let ne = next_cond_gen e (fun e -> p e || e.nb >= max_i) next in
      if ne.nb >= max_i then (
        let pe = goto_ckpt e.nb in
        if debug then
          pf "we have moved too far: replay from %i to %i \n%s\n%!" pe.nb e.nb
            "i.e., replay from the last checkpoint to get the PRGS right";
        if e.nb = pe.nb then e (* may occur at the first event *) else
          next_cond_gen pe (fun le -> le.nb >= e.nb) next
      )
      else
        sforward ne max_i
  in
  if e.nb = 1 then failwith "Cannot move backwards from the first event.\n"
  else
    f e e.nb

let (rev_cond : RdbgEvent.t -> (RdbgEvent.t -> bool) -> RdbgEvent.t) =
  fun e p ->
  let e = rev_cond_gen e p next_np (fun _ -> ()) in
  e

let previous_match e str =
  rev_cond e (fun e -> Str.string_match (Str.regexp_string str) e.name 0)


(* move backwards until a breakpoint is reached *)
let (rev : RdbgEvent.t -> RdbgEvent.t) =
  fun e -> 
    let stop e = List.exists (break_matches e) !breakpoints in
    rev_cond e stop

let _stop_at i e = e.nb = i;;

let (prev : RdbgEvent.t -> RdbgEvent.t) =
  fun e ->
    let cond ne =
      if ne.nb = 1 then (
        pf "Warning: did not find a satisfiable event before event %i\n" e.nb;
        true
      )
      else 
      e.kind=ne.kind && ne.depth=e.depth && e.step-1=ne.step && e.name=ne.name && 
                         (e.lang="lutin" || match e.sinfo, ne.sinfo with
                          Some si, Some nsi -> si()=nsi()| _,_ ->  true)
                          
    in
    try rev_cond e cond with Not_found -> e
        
let rec (previ : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e cpt ->
    if cpt > 0 then previ (prev e) (cpt-1) else e


let (goto : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e i -> 
    if i > e.nb then goto_forward e i else if e.nb = i then e else
      let e = goto_last_ckpt i in
      goto_forward e i

let (_backi : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e i ->
  let stop ce =  (ce.nb = e.nb - i) in
  let pe = get_hook "print_event" in
  let _ = del_hook  "print_event" in
  try
    let e = rev_cond e stop in
    add_hook "print_event" pe;
    pe e;
    e
  with e ->
    add_hook "print_event" pe;
    raise e

let (backi : RdbgEvent.t -> int -> RdbgEvent.t) =
  fun e i ->
    if i > e.nb then goto e 1 else goto e (e.nb-i)


(* let (backi : RdbgEvent.t -> int -> RdbgEvent.t) = *)
(*   fun e i -> *)
(*   let stop ce =  (ce.nb = e.nb - i) in *)
(*   let pe = get_hook "print_event" in *)
(*   let e = rev_cond e stop in *)
(*   pe e; *)
(*   e *)
    
let (back : RdbgEvent.t -> RdbgEvent.t) =
  fun e ->
  backi e 1


      

           
