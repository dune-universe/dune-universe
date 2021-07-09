(* Time-stamp: <modified the 16/03/2020 (at 11:32) by Erwan Jahier> *)

type label = string
type uniq_id = string
type url = string
type node_call = label * uniq_id * url
type wire = label * Data.v
type io = In | Out
type rank = int (* used to display clocks *)
type port = io * rank
type link = node_call * port * wire
type call_graph =  link list * link list (* *)
type clock = string * Data.v (* name , value *)

open RdbgEvent
 exception NoSourceInfo
 let (get_src : RdbgEvent.t -> src_info) = 
  fun e -> 
    match e.sinfo with
      | None  -> raise NoSourceInfo
      | Some si -> si ()

let log_file = "callgraph.log"
let log = open_out log_file
let verbose = ref false

let pdf_viewer = ref "xpdf -remote "
                     
(*********************************************************************************)
(* to merge pre.set and pre.get in the same node *)
type src_info_select = (string * (int * int) * (int * int) * src_info_atom option) list
(* pre.set and pre.get shares this information *)
                                                                      
type store = {
  mutable cpt : int;
  pre_tbl: (src_info_select, node_call) Hashtbl.t;
}
let store_init () =
  { cpt = 0;
    pre_tbl=Hashtbl.create 0
  }

let (get_nodecall: store -> RdbgEvent.t -> node_call) =
  fun s e ->
  let name = e.name in
  let si = get_src e in
  let label = try (List.hd si.atoms).str with _ -> name in
  match name with
  | "Lustre::pre.set" ->
     let key = List.map (fun a -> a.file, a.line, a.char, a.stack) si.atoms in
     let label, uniq, url = 
       (try Hashtbl.find s.pre_tbl key
        with Not_found -> (* should only occur when pre.set is the first event*)
          ("pre","pre","pre.pdf"))
     in
     label, uniq, url
  | _ ->
     if name = "Lustre::pre.get" then (
       let key = List.map (fun a -> a.file, a.line, a.char, a.stack) si.atoms in
       try
         let label, uniq, url = Hashtbl.find s.pre_tbl key in
         label, uniq, url
       with Not_found -> (
         let id = s.cpt in
         let uniq = Printf.sprintf "%s_%d" "pre" id in
         let url =  Printf.sprintf "%s.pdf" uniq in
         Hashtbl.add s.pre_tbl key ("pre", uniq, url);
         s.cpt <- s.cpt+1;
         label, uniq, url
       )
     ) else (
       let id = s.cpt in
       let uniq = Printf.sprintf "%s_%d" name id in
       let url =  Printf.sprintf "%s.pdf" uniq in
       s.cpt <- s.cpt+1;
       label, uniq, url
     )  

let (get_val : var -> Data.subst list -> Data.v) =
  fun (v,_) s ->
    try List.assoc v s
    with Not_found -> failwith ("can't find the value of " ^ v)

let val_to_string v =
  match Data.val_to_string string_of_float v with
  | "Lustre::true" -> "t"
  | "Lustre::false" -> "f"
  | s -> s

           
let (get_links : store -> RdbgEvent.t -> node_call -> clock list -> link list) =
  fun _s e node_call clks ->
  assert(e.kind = Exit);
  let si = get_src e in
  let make_link io i (arg,par) =
    let wire = fst arg, get_val par e.data in
    let link = node_call, (io, i), wire in
    link
  in
  let in_links = List.mapi (make_link In) si.in_subst in (* *)
  let out_links = List.mapi (make_link Out) si.out_subst in (* *)
  let clk_links = List.map (fun c -> node_call, (In,-1), c) clks in
  in_links@out_links@clk_links


let (get_outter_links : RdbgEvent.t -> link list) =
  fun e ->
  (* links of the main call, used to deal with predefined operators *)
  assert(e.kind = Exit);
  let si = get_src e in
  let make_link io i (arg,par) =
    let wire = fst arg, get_val par e.data in
    let link = (fst par,fst par,""), (io, i), wire in
    link
  in
  let in_links  = List.mapi (make_link In) si.in_subst in (* *)
  let out_links = List.mapi (make_link Out) si.out_subst in (* *)
  (in_links)@(out_links)
             
let _io2string = function In -> "i" | Out -> "o"

let (gen_dot: RdbgEvent.t -> node_call -> bool -> bool -> call_graph -> unit) =
  fun e (_lbl,uid,_url) full view (ll,oll) ->
  let dot = uid^".dot" in
  let ps = uid ^".ps" in
  let pdf = uid ^".pdf" in
  let oc = open_out dot in
  let dl ((_,node_call,_),(io,rank),(n, v)) =
    let f,t = node_call, n in
    let f,t,_shape =
      match io with
      | In -> t, f, ""
      | Out -> f, t, ""
    in
    let value = (val_to_string  v) in
    let color = if value="nil" then ";color=tomato1; fontcolor=tomato1" else "" in
    let clk = if rank = -1 then "; headport=n; arrowhead=dot" else "" in
    Printf.fprintf oc "\"%s\" -> \"%s\"  [label =\"%s\" %s %s]\n" f t value color clk
  in
  let dl_outter ((_,node_call,_),(io,_rank),(n, v)) =
    let f,t = node_call, n in
    let f,t,_shape =
      match io with
      | In -> t, f, ""
      | Out -> f, t, ""
    in
    Printf.fprintf oc "\"%s\" -> \"%s\"  [label =\"%s\"] ;\n" f t (val_to_string v)
  in
  let locals, newvars =
    List.fold_left
      (fun (loc,nv) (_,_,(l,_)) ->
       if l.[0]='_' then
         if (List.mem l nv) then (loc,nv) else (loc,l::nv)
       else
         if (List.mem l loc) then (loc,nv) else (l::loc,nv)
      )
      ([],[]) ll
  in
  let nodes = 
    List.fold_left
      (fun acc (nc,_,(_,_)) -> if List.mem nc acc then acc else nc::acc)
      [] ll
  in
  let interface = fst (List.split (e.inputs @ e.outputs)) in
  let ll1,ll2= List.partition (fun (_,_,(label,_)) -> List.mem label interface) ll in
  let si = get_src e in
  let tooltip = Printf.sprintf "step=%d; depth=%d" e.step e.depth in
  output_string oc "digraph G {
	rankdir=LR;
	node [shape = rect];
 {\n";
  List.iter (fun loc -> Printf.fprintf oc "\"%s\" [shape=point]\n" loc) newvars;
  List.iter (fun loc -> Printf.fprintf oc "\"%s\" [shape=ellipse]\n" loc) locals;
  List.iter (fun (label,id,url) ->
             Printf.fprintf oc
                            "\"%s\" [label=\"%s\" URL=\"%s\" tooltip=\"%s\"]\n"
                            id label url tooltip)
            nodes;
  if ll <> [] then (
    List.iter
      (fun (v,_) -> Printf.fprintf oc "\"%s\" [style=filled fillcolor=lightblue]\n" v)
      e.inputs;
    List.iter
      (fun (v,_) -> Printf.fprintf oc "\"%s\" [style=filled fillcolor=red]\n" v)
      e.outputs;
  );
  Printf.fprintf oc  "}\n subgraph cluster1 { \nlabel=\"%s\"; \n" (List.hd si.atoms).str;
  if ll <> [] then (
    List.iter dl ll2;
    output_string oc  "}\n";
    List.iter dl ll1;
  ) else (
    
  List.iter
    (fun (v,_) -> Printf.fprintf oc "\"%s\" [style=filled fillcolor=lightblue]\n" v)
    e.inputs;
  List.iter
    (fun (v,_) -> Printf.fprintf oc "\"%s\" [style=filled fillcolor=red]\n" v)
    e.outputs;
  output_string oc  "}\n";
  List.iter dl_outter oll;
  );
  output_string oc  "}\n";
  flush oc;
  close_out oc;
  let cmd =
    if full then
      if view then 
        Printf.sprintf "dot %s -Tps2 > %s && ps2pdf %s&& %s %s %s &\n"
                       dot ps ps !pdf_viewer pdf pdf
      else
        Printf.sprintf "dot %s -Tps2 > %s && ps2pdf %s &\n" dot ps ps
    else  if view then
      Printf.sprintf "dot %s -Tpdf > %s&& %s %s %s & \n"
                     dot pdf  !pdf_viewer pdf pdf
      else
      Printf.sprintf "dot %s -Tpdf > %s \n" dot pdf 
  in
  output_string log cmd; flush log;
  ignore(Sys.command cmd)

let next e =
  if !verbose then RdbgStdLib.next e else RdbgStdLib.next_np e

(* the core of the work *)
let rec (call_graph: store ->  RdbgEvent.t -> bool -> bool -> node_call option ->
         RdbgEvent.t * call_graph * node_call) =
  fun s e full first inst_opt ->
  if not (e.kind = Call) then failwith "Not a call event";
  (*   if not (e.lang="lustre") then failwith "Not a Lustre node"; *)
  let d = e.depth in
  let stop e2 = e2.kind=Exit && e2.depth=d in
  let rec (f: RdbgEvent.t -> node_call -> link list -> clock list ->
           RdbgEvent.t * link list * node_call) =
    fun e inst cg clks ->
    if stop e then (e, cg, inst) else
      if e.kind=Call && e.depth = d+1 && e.name = "when" then
        let si = get_src e in
        let carg,cpar = List.hd si.in_subst in
        let clk = (fst carg), get_val cpar e.data   in
        let e = next e in
        let inst = get_nodecall s e in
        f e inst cg (clk::clks)
      else
      if e.kind=Exit && e.depth = d+1 && e.name = "when" then
        let e = next e in
        let inst = get_nodecall s e in
        f e inst cg (List.tl clks)
      else
      if e.kind=Exit && e.depth = d+1 then
        let links = get_links s e inst clks in
        let cg = cg@links in
        let e = next e in
        let inst = get_nodecall s e in
        f e inst cg clks
      else if full && e.kind=Call && e.depth > d then (
        let e,_,inst = call_graph s e full false (Some inst) in
        f e inst cg clks
      )
      else
        let e = next e in
        let inst = get_nodecall s e in
        f e inst cg clks
  in
  let inst = match inst_opt with Some i -> i | None -> get_nodecall s e in
  let e, ll, inst =  f e inst [] [] in
  let cg = (ll,get_outter_links e) in
  gen_dot e inst full first cg;
  e,cg, inst 


let last_uid = ref ""
(*exported *)                     
let gen_call_graph e =
  let s = store_init () in
  let e,_,(_,uid,_)= (call_graph s e false true None) in
  last_uid := uid;
  Printf.printf "system calls can be seen in %s\n" log_file;
  e
             
(*exported *)                     
let gen_call_graph_full e =
  let s = store_init () in
  let e,_,(_,uid,_)= (call_graph s e true true None) in
  last_uid := uid;
  Printf.printf "system calls can be seen in %s\n" log_file;
  e
  
(*exported *)                     
let display_call_graph () =
  let pdf = !last_uid ^".pdf" in
  let cmd = Printf.sprintf "%s %s &\n" !pdf_viewer pdf in
  if !verbose then (output_string log cmd; flush log);
  ignore(Sys.command cmd)
            
(*
 #use "callgraph.ml";;

 let c () = gen_call_graph !e;;  
 let cf () = gen_call_graph_full  !e;;  
 let d () = display_call_graph !e;;  

 *)
