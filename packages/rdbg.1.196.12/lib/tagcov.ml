(* Time-stamp: <modified the 16/03/2020 (at 11:33) by Erwan Jahier> *)

type label = string
type uniq_id = string
type node_call = label * uniq_id 
type wire = label 
type rank = int (* used to display clocks *)
type port = In of int | Out of int | Clk
type link = node_call * port * wire
type call_graph =  link list 
type clock = string * Data.v (* name , value *)

open RdbgEvent
type call_tbl = (node_call,
                 link list * var list * var list * var list * src_info) Hashtbl.t
let call_tbl:call_tbl = Hashtbl.create 0
               
let verbose = ref false
let log_file = "callgraph.log"
let log = open_out log_file

(*********************************************************************************)
type caller_tbl = (node_call, node_call) Hashtbl.t
let caller_tbl:caller_tbl = Hashtbl.create 0

let top ="top","top" (* the father of all fathers *)
let (caller : node_call -> node_call)  =
  fun nc ->
  try Hashtbl.find caller_tbl nc
  with Not_found -> top

let (caller_update : node_call -> node_call -> unit)  =
  fun caller called ->
  Hashtbl.replace caller_tbl called caller
                 
(*********************************************************************************)
type tag = int
                                           
module Tags = Set.Make(struct type t = tag let compare = compare end)
type tags = Tags.t
type tags_ref = Tag of Tags.t | Ref of node_call * wire

(* the give (unique) names to tags *)
type tag_tbl  = (node_call * wire, tag) Hashtbl.t                           

(* associer les valeurs et le tags a un wire+le nodecall appelant ? *)
(* type tags_tbl = (link, rtags) Hashtbl.t *)
type tags_tbl = (node_call * wire, Data.v * tags_ref) Hashtbl.t
                                   
let tag_cpt = ref 0
let tag_tbl:tag_tbl = Hashtbl.create 0
let tags_tbl:tags_tbl = Hashtbl.create 0

(* The only spot where tags are created are:
- top-level inputs
- local vars
- constants
 *)     
let (make_tag: node_call -> wire -> unit) =
  fun nc wire ->
  try ignore (Hashtbl.find tag_tbl (nc, wire))
  with Not_found ->
    let tag = !tag_cpt in
    incr tag_cpt;
    if !verbose then Printf.printf "Add (%s,%s) -> %d\n" (snd nc) wire tag;
    Hashtbl.add tag_tbl (nc,wire) (tag);
    Hashtbl.add tags_tbl (nc,wire) (Data.U,  Tag(Tags.singleton tag));
    ()

(* also returns the entry in the tags_ref tbl that contains the real tags *)
let rec (get_tags : node_call -> wire -> node_call * wire * Data.v * tags) =
  fun nc wire ->
    (match  Hashtbl.find tags_tbl (nc,wire) with
     | v, Tag tags -> nc, wire, v, tags
     | _, Ref (nc2,w2)  ->
        assert ((nc2,w2) <> (nc,wire));
        get_tags nc2 w2)

(* create a tag if necessary (this first time a tag site is encountered) *)
let (get_tags_nf : node_call -> wire -> node_call * wire * Data.v * tags) =
  fun nc wire ->
  try get_tags nc wire 
  with _ ->
    make_tag nc wire;
    try get_tags nc wire
    with Not_found -> assert false
      
(* used to make node params reference towards node args *)
let (make_tag_ref: node_call -> node_call -> wire -> wire -> unit) =
  fun nc ncf wire_arg wire_par ->
  try ignore (Hashtbl.find tag_tbl (nc, wire_par))
             (* the ref already exist; we are cool *)
  with Not_found ->
    let ncr,wr,_,_ = get_tags_nf ncf wire_arg in   
    let tag_ref = Ref (ncr, wr) in
    Hashtbl.add tag_tbl (nc,wire_par) (-2);
    Hashtbl.add tags_tbl (nc,wire_par) (Data.U,tag_ref);
    ()

let (get_tag : node_call -> wire -> tag) =
  fun nc wire -> (* nc is the caller *)
  try (Hashtbl.find tag_tbl (nc,wire))
  with Not_found -> 
    (* Printf.printf "Cannot find the tag for (%s,%s)\n" (snd nc) wire; *)
    -1

(*********************************************************************************)
(* pack list of elt into list of list of size n (at most) *)
let (pack: 'a list -> int -> 'a list list) =
  fun l n -> 
  let rec aux acc cpt l =
    match l,acc with
    | _,[] -> assert false
    | [],_ -> acc
    | elt::tail, acc0::acc_tail ->
       if cpt=0 then aux ([elt]::acc) n tail
       else          aux ((elt::acc0)::acc_tail) (cpt-1) tail
  in
  aux [[]] n l

let (int_list_to_str : int list -> string list) =
  fun il ->
  assert (il<>[]);
  let inter2str l h =
    if l=h then string_of_int l else Printf.sprintf "%d-%d" l h
  in
  let rec aux (l,h) acc il =
    match il with
    | [] -> (inter2str l h)::acc
    | x::t ->
       if x = h+1 then aux (l,x) acc t
       else            aux (x,x) ((inter2str l h)::acc) t
  in
  let f = List.hd il in
  aux (f,f) [] (List.tl il)

let _ =
  assert (int_list_to_str [1;2;3;4;5;7;8;9;11] = (List.rev ["1-5";"7-9";"11"]))
      
let tags2str tags =
  let tagl = Tags.fold (fun tag acc -> tag::acc) tags [] in
  let tagl = List.rev tagl in
  let tagstrl = int_list_to_str tagl in
  let tagstrll = pack tagstrl 10 in
  let l = List.map (fun tagl -> String.concat "," tagl) tagstrll in
  "{"^(String.concat ",\n" l)^"}"

let tags_ref2str = function
  | Tag tags -> tags2str tags
  | Ref (_nc,_w) -> ""

let val_to_string v =
  match Data.val_to_string string_of_float v with
  | "Lustre::true" -> "t"
  | "Lustre::false" -> "f"
  | s -> s

let d () =
  Hashtbl.iter
    (fun ((lbl,uid),w) (v,tags) ->
     Printf.printf "(%s,%s),%s: %s (%s)\n"
                   lbl uid w (tags_ref2str tags) (val_to_string v)) tags_tbl
    
let (get_link_val : link -> Data.v) =
  fun (nc,_,wire) ->
  let _,_,v,_ = get_tags_nf (caller nc) wire in
  v

(* raises Not_found the first time it is called in get_link. *)
let (tags_of_link : link -> tags) =
  fun (nc,_,wire) ->
  let _,_,_,tags = get_tags_nf (caller nc) wire in
  tags

(* replace add (vith Tags.union) tags to the tags pointed by (nc, wire) *)
let (update_tags : node_call -> wire -> tags -> unit) =
  fun nc wire tags -> 
  try
    let nc,wire,v,ptags = get_tags_nf nc wire in
    let tags = Tags.union ptags tags in
    if !verbose then Printf.printf "(%s,%s) -> Some tags [update_tags]\n" (snd nc) wire;
    Hashtbl.replace tags_tbl (nc,wire) (v, Tag tags)
  with Not_found ->
    make_tag nc wire;
    let v,_ = Hashtbl.find tags_tbl (nc,wire) in
    if !verbose then Printf.printf "(%s,%s) -> Some tags [update_tags 1]\n" (snd nc) wire;
    Hashtbl.replace tags_tbl (nc,wire) (v,Tag tags)

let (update_val : node_call -> wire -> Data.v -> unit) =
  fun ncf (* meant to be the caller *) wire v ->
    try
      let nc,w,_v,tags = get_tags_nf ncf wire in
      if !verbose then Printf.printf "(%s,%s) -> Some tags [update_val]\n" (snd nc) wire;
      Hashtbl.replace tags_tbl (nc,w) (v, Tag tags)
    with _ ->
      make_tag ncf wire;
      let nc,w,_v,tags = get_tags_nf ncf wire in
      if !verbose then
        Printf.printf "(%s,%s) -> Some tags [update_val 1]\n" (snd nc) wire;
      Hashtbl.replace tags_tbl (nc,w) (v, Tag tags)

(*********************************************************************************)
open RdbgEvent
 exception NoSourceInfo
 let (get_src : RdbgEvent.t -> src_info) = 
  fun e -> 
    match e.sinfo with
      | None  -> raise NoSourceInfo
      | Some si -> si ()

(*********************************************************************************)
(* to merge pre.set and pre.get in the same node *)
 type src_info_select = (string * (int * int) * (int * int) * src_info_atom option) list
(* pre.set and pre.get shares this information *)
                                                                      
let cpt = ref 0
let pre_tbl = Hashtbl.create 0

(* We don't want to create new node instances from one step to
  another, hence we tabulate it using src_info as a key, which is
  wrong for nodes that are called via meta-operators !!! Indeed, each
  node call has the exact same src_info (same line numbers, same
  stack, etc.). Arf.  *)
let node_tbl = Hashtbl.create 0
  
let (get_nodecall: RdbgEvent.t -> node_call) =
  fun e ->
  let name = e.name in
  let si = get_src e in
  let label = try (List.hd si.atoms).str with _ -> name in
  match name with
  | "Lustre::pre.set" ->
     let key = List.map (fun a -> a.file, a.line, a.char, a.stack) si.atoms in
     let label, uniq = 
       (try Hashtbl.find pre_tbl key
        with Not_found -> (* should only occur when pre.set is the first event*)
          ("pre","pre"))
     in
     label, uniq
  | "Lustre::pre.get" -> (
    let key = List.map (fun a -> a.file, a.line, a.char, a.stack) si.atoms in
    try Hashtbl.find pre_tbl key 
    with Not_found -> 
      let label, uniq = "pre", Printf.sprintf "%s_%d" "pre" !cpt in
      Hashtbl.add pre_tbl key (label, uniq);
      incr cpt;
      label, uniq
  )
  | _ ->  (
    let key = name, List.map (fun a -> a.file, a.line, a.char, a.stack) si.atoms in
    try Hashtbl.find node_tbl key 
    with Not_found -> 
      let uniq =  Printf.sprintf "%s_%d" name !cpt in
      incr cpt;
      Hashtbl.add node_tbl key (label, uniq);    
      label, uniq
  )

(*********************************************************************************)
let (get_val : var -> Data.subst list -> Data.v) =
  fun (v,_) s ->
    try List.assoc v s
    with Not_found -> failwith ("can't find the value of " ^ v)

let link2str ((_,id),_port,label) =
    Printf.sprintf "%s -> %s" id label
         
(*********************************************************************************)
let (get_links : RdbgEvent.t -> node_call -> node_call -> clock list -> link list) =
  fun e ncf ncc clks ->
  (* returns the link corresponding to ncc I/O, and the clock links *)
  assert(e.kind = Exit);
  let si = get_src e in
  let make_link is_input i (arg,par) =
    let wire_arg = fst arg in
    let _wire_par = fst par in
    let v = get_val par e.data in
    let link = ncc, (if is_input then In i else Out i), wire_arg in
    update_val ncf wire_arg v;
    link
  in
  let in_links  = List.mapi (make_link true)  si.in_subst  in (* *)
  let out_links = List.mapi (make_link false) si.out_subst in (* *)
  let clk_links = List.map (fun (c,_cval) -> ncc, Clk, c) clks in
  List.iter2 (fun (_nc,_,w) (_,cval) -> update_val ncf w cval) clk_links clks; 
  in_links@out_links@clk_links

(*********************************************************************************)
let pdf_viewer = ref "xpdf -remote "

let get_url str = Printf.sprintf "%s.pdf" str

(*********************************************************************************)
let (gen_dot: var list -> var list -> var list -> RdbgEvent.src_info -> node_call ->
     bool -> bool ->  call_graph -> unit) =
  fun inputs outputs _locs si nc full view ll ->
  let _lbl,uid = nc in
  let interface = fst (List.split (inputs @ outputs)) in
  let dot = uid^".dot" in
  let ps = uid ^".ps" in
  let pdf = uid ^".pdf" in
  let oc = open_out dot in
  let dl link =
    let (node_call,io,wire) = link in
    let f,t = snd node_call, wire in
    let f,t,shape =
      match io with
      | In _  -> t, f, ""
      | Out _ -> f, t, ""
      | Clk  -> t, f, "headport=n; arrowhead=dot"
    in
    let v = get_link_val link in
    Printf.fprintf oc "\"%s\" -> \"%s\"  [%s label=\"%s\"]\n" f t shape (val_to_string v)
  in
  let locals, newvars =
    List.fold_left
      (fun (loc,nv) (_,_,l) ->
       if l.[0]='_' then
         if (List.mem l nv) then (loc,nv) else (loc,l::nv)
       else
         if (List.mem l loc) then (loc,nv) else (l::loc,nv)
      )
      ([],[]) ll
  in
  let nodes = 
    List.fold_left
      (fun acc (nc,_,_) -> if List.mem nc acc then acc else nc::acc)
      [] ll
  in
  let ll1,ll2= List.partition (fun (_,_,label) -> List.mem label interface) ll in
  let tooltip = "" in
  output_string oc "digraph G {
	                 rankdir=LR;
	                 node [shape = rect];
                    {\n";
  let var_list = ref [] in (* used to collect used vars (useful to know which 
    array or struct access should be shown) *)
  let pr_var opt var =
    if not (List.mem var !var_list) then var_list := var:: !var_list;
    let _,_,v, tags = get_tags_nf nc var in
    let tag = get_tag nc var in
    let tagstr = if tag<0 then "" else string_of_int tag in
    let color = if v=Data.U then ";color=tomato1; fontcolor=tomato1" else "" in
    Printf.fprintf oc "\"%s\" [label=\"%s\n%s %s\" %s %s]\n"
                   var var tagstr (tags2str tags) opt color
  in
  List.iter (pr_var "shape=diamond") newvars;
  List.iter (pr_var "shape=ellipse") locals;
  List.iter (fun (label,id) ->
             let links,_,_,_,_= Hashtbl.find call_tbl (label,id) in
             if links = [] then
               Printf.fprintf oc "\"%s\" [label=\"%s\" tooltip=\"%s\"]\n"
                              id label tooltip
             else
               Printf.fprintf oc "\"%s\" [label=\"%s\" URL=\"%s\" tooltip=\"%s\"]\n"
                              id label (get_url id) tooltip
            )
            nodes;
  if ll <> [] then (
    List.iter (pr_var "style=filled fillcolor=lightblue shape=ellipse")
              (fst (List.split inputs));
    List.iter (pr_var "style=filled fillcolor=red shape=ellipse")
              (fst (List.split outputs));
  );
  Printf.fprintf oc  "}\n subgraph cluster1 { \nlabel=\"%s\"; \n" (List.hd si.atoms).str;
  if ll <> [] then (
    List.iter dl ll2; 
    output_string oc  "}\n";
    List.iter dl ll1;
  ) else (
    (*     List.iter *)
    (*       (fun (v,_) -> Printf.fprintf oc "\"%s\" [style=filled fillcolor=lightblue]\n" v) *)
    (*       e.inputs; *)
    (*     List.iter *)
    (*       (fun (v,_) -> Printf.fprintf oc "\"%s\" [style=filled fillcolor=red]\n" v) *)
    (*       e.outputs; *)
    (*     output_string oc  "}\n"; *)
  (*     List.iter dl_outter oll; *)
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

(*********************************************************************************)
open Data
let clk_stack: clock list ref = ref []
let nc_stack : node_call list ref = ref [] 
let lk_stack : link list list ref = ref [[]] 

(* previous event number to prevent time travel during callgraph computation *)
let pre_enb = ref 0

(* since pre is split into get and set, we need to store its inputs at set events
to be able to propagate the tags at get events
 *)
let pre_input_tbl = Hashtbl.create 0
                         
(* we know at call events if an arrow is at its first step;
therefore we store this info at call(arrow) step and use it at
exit(arrow) step *)
let last_arrow_first = ref true
                           
(* [add_tags l t] add the tags t *)
let (add_tags: node_call -> link list -> tags -> unit) =
  fun nc ol t ->
  let otags = List.map tags_of_link ol in
  let otags = List.map (Tags.union t) otags in
  List.iter2 (fun (_,_,w) ntag -> update_tags nc w ntag) ol otags

let boolred_do min max ncf il ol =
    (* there are 3 cases:
    (1)  | I={i | xi  }| < min 
        in this case we return the intersection of tag_i forall i *not* in I
    (2) | I={i | xi  }| > max 
        in this case we return the intersection of tag_i forall i in I
    (3) | I={i | xi  }|  in [min,max]
       in this case we return the union of tag_i forall i in I
     *)
      let vals = List.map get_link_val il in
      let tags = List.map tags_of_link il in
      let n = List.fold_left (fun acc v -> if v=B true then acc+1 else acc) 0 vals in
      let t = 
        if n < min then (* case 1 *)
          List.fold_left2
            (fun acc t v -> if v = B false then Tags.inter acc t else acc)
            Tags.empty tags vals
        else if n>max then (* case 2 *)
          List.fold_left2
            (fun acc t v -> if v = B true then Tags.inter acc t else acc)
            Tags.empty tags vals
        else (* n=1, case 3 *)
          List.fold_left2
            (fun acc t v -> if v = B true then Tags.union acc t else acc)
            Tags.empty tags vals
      in
      add_tags ncf ol t
    
let propagate_tags_predef ncc ncf e il _cl ol =
  match e.name with
  | "Lustre::if"  -> (
    let c, i1, i2 = match il with [c;i1;i2] -> c,i1,i2 | _ -> assert false in
    let tc, t1, t2 = tags_of_link c, tags_of_link i1, tags_of_link i2 in
    match get_link_val c with
    | B true  -> add_tags ncf ol (Tags.union tc t1)
    | B false -> add_tags ncf ol (Tags.union tc t2)
    | U -> () 
    | _ -> assert false
  )
  | "Lustre::and" -> (
    let i1, i2 = match il with [i1;i2] -> i1,i2 | _ -> assert false in
    let t1, t2 = tags_of_link i1, tags_of_link i2 in
    match get_link_val i1, get_link_val i2 with
    | B true,  B true  -> add_tags ncf ol (Tags.union t1 t2)
    | B true,  B false -> add_tags ncf ol t2
    | B false, B true  -> add_tags ncf ol t1
    | B false, B false -> add_tags ncf ol (Tags.inter t1 t2)
    | U,_ | _,U -> () 
    | _ -> assert false
  )
  | "Lustre::or"  -> (
    let i1, i2 = match il with [i1;i2] -> i1,i2 | _ -> assert false in
    let t1, t2 = tags_of_link i1, tags_of_link i2 in
    match get_link_val i1, get_link_val i2 with
    | B true,  B true  -> add_tags ncf ol (Tags.inter t1 t2)
    | B true,  B false -> add_tags ncf ol t1
    | B false, B true  -> add_tags ncf ol t2
    | B false, B false -> add_tags ncf ol (Tags.union t1 t2)
    | U,_ | _,U -> () 
    | _ -> assert false
  )
  | "Lustre::impl"->  (
    let i1, i2 = match il with [i1;i2] -> i1,i2 | _ -> assert false in
    let t1, t2 = tags_of_link i1, tags_of_link i2 in
    match get_link_val i1, get_link_val i2 with
    | B true,  B true  -> add_tags ncf ol t1
    | B true,  B false -> add_tags ncf ol (Tags.union t1 t2)
    | B false, B true  -> add_tags ncf ol (Tags.inter t1 t2)
    | B false, B false -> add_tags ncf ol t2
    | U,_ | _,U -> () 
    | _ -> assert false
  )
  | "Lustre::xor" ->  (* boolred_do 1 1 ncf il ol *)
    (* there are 3 cases:
    (1) forall i, not(xi)
        in this case we take the intersection of all tag_i
    (2) | I={i | xi  }| >= 2
        in this case we take the intersection of all { tag_i | i in I}
    (3) exist a unique i st xi (i.e.,forall j<>i, not xj)
       in this case we return tag_i
     *)
     let vals = List.map get_link_val il in
     let tags = List.map tags_of_link il in
     let n = List.fold_left (fun acc v -> if v=B true then acc+1 else acc) 0 vals in
     let t = 
       if n = 0 then (* case 1 *)
         List.fold_left (fun acc t -> Tags.inter acc t) Tags.empty tags 
       else if n>1 then (* case 2 *)
         List.fold_left2
           (fun acc t v -> if v = B true then Tags.inter acc t else acc)
           Tags.empty tags vals
       else (* n=1, case 3 *)
         List.fold_left2
           (fun acc t v -> if v = B true then t else acc)
           Tags.empty tags vals
     in
     add_tags ncf ol t

  | "Lustre::nor" -> boolred_do 0 0 ncf il ol 
  | "Lustre::current" -> assert false (* sno *)
  | "Lustre::diese" -> (
    boolred_do 0 1 ncf il ol 
  )
  | "Assign" -> (
    let i = match il with [i] -> i | _ -> assert false in
    let t = tags_of_link i in
    add_tags ncf ol t 
  ) 
  | "Lustre::arrow"  -> (
    let i1, i2 = match il with [i1;i2] -> i1,i2 | _ -> assert false in
    let t = if !last_arrow_first then tags_of_link i1 else tags_of_link i2 in
    add_tags ncf ol t 
  )
  | "Lustre::pre.set"  -> ( (* XXX useless??? *)
    let i = match il with [i1] -> i1 | _ -> assert false in
    Printf.printf "Store the input of %s\n" (snd ncc) ;
    Hashtbl.replace pre_input_tbl ncc i
  )
  | "Lustre::pre.get"  -> ( (* XXX useless??? *)
    try
      let i = Hashtbl.find pre_input_tbl ncc in
      let t = tags_of_link i in
      Printf.printf "Got the input of %s\n" (snd ncc) ;
      add_tags ncf ol t
    with Not_found  -> ()
  )
  | n ->
     (* by default, we propagate all tags *)
     if (il = []) then Printf.printf "Warning: %s has no input!\n" n else
       let itags = List.map tags_of_link il in
       let itag = List.fold_left Tags.union (List.hd itags) (List.tl itags) in
       add_tags ncf ol itag
 
let (propagate_out_tags:
       node_call -> node_call -> RdbgEvent.t -> link list -> link list -> clock list -> unit)=
  fun nc ncf e sub_links links _clks -> 
  let il, ol =
    List.partition (fun (_,io,_) -> match io with Out _ -> false | _ -> true) links
  in
  let cl, il =
    List.partition (fun (_,io,_) -> match io with Clk -> true | _ -> false) il
  in
  let clock_tags = List.map tags_of_link cl in
  let clock_tags = List.fold_left (fun acc t -> Tags.union acc t) Tags.empty clock_tags in
  add_tags ncf ol clock_tags;
  (*      Printf.printf "il=%s\ncl=%s\nol=%s\nsub_links=%s\n"  *)
  (*                    (String.concat ", \n\t" (List.map link2str il))  *)
  (*                    (String.concat ", \n\t" (List.map link2str cl))  *)
  (*                    (String.concat ", \n\t" (List.map link2str ol))  *)
  (*                    (String.concat ", \n\t" (List.map link2str sub_links))  *)
  (*      ;  *)
  
  if sub_links = [] then propagate_tags_predef nc ncf e il cl ol else (
  )
                                                                     
(* the core of the work *)
let (update_tagcov: RdbgEvent.t -> unit) =
  fun e ->
  if not (e.nb <> !pre_enb || e.nb <> !pre_enb +1 ) then
    failwith "cannot skip event or move backwards when computing tag coverage";
  if not (e.lang="lustre") then print_string "Not a Lustre node\n" else (
    pre_enb := e.nb;
    match e.kind, e.name, !lk_stack with
    | Call,"when", _ -> (
      let si = get_src e in
      let carg,cpar = List.hd si.in_subst in
      let clk = (fst carg), get_val cpar e.data in
      clk_stack := clk :: !clk_stack
    )
    | Exit, "when", _ -> clk_stack := List.tl !clk_stack
    | Call, _ , lstk  -> (
      let nc = get_nodecall e in
      let caller = if !nc_stack = [] then top else List.hd !nc_stack in
      let si = get_src e in
      caller_update caller nc;
      nc_stack := nc :: !nc_stack;
      lk_stack := []::lstk;
      if e.name = "Lustre::arrow" then (
        let first = try List.assoc "_memory" e.data with Not_found -> assert false in
        last_arrow_first := (first = B true)
      ) else (
        (* lustre::arrow has a local var (_memory), but it is
          (currently) not traced and is necessary covered anywayrd *)
        let vars = if e.depth=2 then (e.inputs @ e.outputs @ e.locals) else (e.locals) in
        (*  let vars =  (e.inputs @ e.outputs @ e.locals)  in *)
        let wires = fst (List.split vars) in
        List.iter (fun w -> make_tag nc w) wires
      );
      List.iter (fun ((arg,_),(par,_)) -> make_tag_ref nc caller arg par) si.in_subst;
      List.iter (fun ((arg,_),(par,_)) -> make_tag_ref nc caller arg par) si.out_subst;
    )
    | Exit, _n, sub_links::links::lstk -> (
      let nc,nc_father = match !nc_stack with
        | nc1::nc2::_ -> nc1,nc2
        | [nc] -> nc,top
        | [] -> assert false
      in
      let nlinks = get_links e nc_father nc !clk_stack in
      let si = get_src e in
      propagate_out_tags nc nc_father e sub_links nlinks !clk_stack;
      Hashtbl.replace call_tbl nc (sub_links, e.inputs, e.outputs, e.locals, si); 
      lk_stack :=  (links @ nlinks)::lstk;
      nc_stack := try List.tl !nc_stack with _ -> assert false;
    )
    | Ltop, _ , _     -> lk_stack := [[]]; clk_stack := []; nc_stack := []
    | MicroStep _, _, _ -> assert false
    | Exit, _, _::[] -> assert false
    | Exit, _, [] -> assert false
  )
                                                                          
let (next : RdbgEvent.t -> RdbgEvent.t) =
  fun e ->
  let e = RdbgStdLib.next e in
  update_tagcov e;
  e
    
let rec (nexti : RdbgEvent.t  -> int -> RdbgEvent.t) =
  fun e i ->
  if i=0 then e else nexti (next e) (i-1)

let dump_call_tbl () =
  Hashtbl.iter
    (fun (label,uniq) (links,_,_,_,_) ->
     if links <> [] then
       let strl = List.map
                    (fun ((_,id),_port,label) ->
                     Printf.sprintf "%s %s" id label)
                    links
       in
       let str = String.concat "\n\t" strl in
       Printf.printf "'%s-%s' \n\t%s\n" label uniq str
    )
    call_tbl;;

let gen_all_dot event =
  Hashtbl.iter
    (fun (label,uniq) (links,inputs, outputs, locals, si) ->
     if links <> [] then
       let view = (get_nodecall event = (label,uniq)) in
       gen_dot inputs outputs locals si (label,uniq) true view  links 
    )
    call_tbl

 let gen_one_dot event =
  let (label,uniq) = get_nodecall event in
  let links,inputs,locals, outputs,si = Hashtbl.find call_tbl (label,uniq) in
  gen_dot inputs outputs locals si (label,uniq) false true  links 
  
let init () =
  Hashtbl.clear call_tbl;
  Hashtbl.clear tags_tbl;
  Hashtbl.clear tag_tbl;
  Hashtbl.clear pre_tbl;
  cpt := 0;
  tag_cpt := 0;
  pre_enb :=0;
  clk_stack := [];
  nc_stack  := [] ;
  lk_stack  := [[]] 

  (*
add_hook "tagcov" Tagcov.update_tagcov;;

 e:= RdbgStdLib.next !e;;

  update_tagcov !e;;

let tc () =
 update_tagcov !e;
 e := next !e; while (!e.kind <> Ltop) do  e := next !e done;;

 e := next !e;;

 let c () = gen_call_graph !e;;  
 let cf () = gen_call_graph_full  !e;;  
 let d () = display_call_graph !e;;  

 *)
