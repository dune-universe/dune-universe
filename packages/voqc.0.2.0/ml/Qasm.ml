open OpenQASM
open OpenQASM.AST
open Printf
open StandardGateSet
open UnitaryListRepresentation

module StringMap = Map.Make(String)

(* Code for converting between OpenQASM and SQIR circuits uing the standard gate set. *)

(** List of recognized gates **)

let qelib1 = [

  (* standard gates *)
  ("u3",  TGate(3,1));
  ("u2",  TGate(2,1));
  ("u1",  TGate(1,1));
  ("cx",  TGate(0,2));
  ("id",  TGate(0,1));
  ("x",   TGate(0,1));
  ("y",   TGate(0,1));
  ("z",   TGate(0,1));
  ("h",   TGate(0,1));
  ("s",   TGate(0,1));
  ("sdg", TGate(0,1));
  ("t",   TGate(0,1));
  ("tdg", TGate(0,1));
  ("rx",  TGate(1,1));
  ("ry",  TGate(1,1));
  ("rz",  TGate(1,1));
  ("cz",  TGate(0,2));
  ("swap",  TGate(0,2));
  (* ("cy",  TGate(0,2));
  ("ch",  TGate(0,2)); *)
  ("ccx", TGate(0,3));
  ("ccz", TGate(0,3));
  (* ("crz", TGate(1,2));
  ("cu1", TGate(1,2));
  ("cu3", TGate(3,2)); *)
  
  (* custom - rotation by a rational multiple of PI *)
  ("rzq", TGate(2,1))
]

let check_stmt symTab stmt =
  match stmt with
  | Include "qelib1.inc" -> List.fold_left
                              (fun map (gate, typ) -> StringMap.add gate typ map)
                              symTab qelib1
  | Include _ -> raise (Failure "ERROR: Unsupported include")
  | Decl (QReg (id, size)) -> StringMap.add id (TQReg size) symTab
  | Decl (CReg (id, size)) -> StringMap.add id (TCReg size) symTab
  | GateDecl ((id, params, qargs), _) ->
    StringMap.add id (TGate (List.length params, List.length qargs)) symTab
  | OpaqueDecl (id, params, qargs) ->
    StringMap.add id (TGate (List.length params, List.length qargs)) symTab
  | _ -> symTab

let check program = List.fold_left check_stmt StringMap.empty program

(** Convert OpenQASM AST to SQIR program **)

(* For qubit mapping *)
module QbitIdx =
struct
  type t = string * int
  let compare (q0, i0) (q1, i1) =
    match Stdlib.compare q0 q1 with
    | 0 -> Stdlib.compare i0 i1
    | c -> c
end

module QbitMap = Map.Make(QbitIdx)

(* Controlled gate application *)
let apply_c_gate gate ctrl tgt qmap sym_tab =
  let (cid, cidx) = ctrl in
  let (tid, tidx) = tgt in
  match cidx, tidx with
  | Some ci, Some ti ->
    [gate (QbitMap.find (cid, ci) qmap) (QbitMap.find (tid, ti) qmap)]
  | None, Some ti ->
    (match StringMap.find cid sym_tab with
     | TQReg csize ->
       let tgt_idx = (QbitMap.find (tid, ti) qmap) in
       List.init csize (fun i -> gate (QbitMap.find (cid, i) qmap) tgt_idx)
     | _ -> raise (Failure "ERROR: Not a qubit register"))
  | Some ci, None ->
    (match StringMap.find tid sym_tab with
     | TQReg tsize ->
       let ctrl_idx = (QbitMap.find (cid, ci) qmap) in
       List.init tsize (fun i -> gate ctrl_idx (QbitMap.find (tid, i) qmap))
     | _ -> raise (Failure "ERROR: Not a qubit register"))
  | None, None -> (* parallel application *)
    (match StringMap.find cid sym_tab, StringMap.find tid sym_tab with
     | TQReg csize, TQReg tsize ->
       if csize != tsize
       then raise (Failure "ERROR: Register sizes do not match")
       else List.init csize (fun i ->
           gate (QbitMap.find (cid, i) qmap) (QbitMap.find (tid, i) qmap))
     | _ -> raise (Failure "ERROR: Not a qubit register"))

(* Doubly-controlled gate application (partial) *)
let apply_double_c_gate gate ctrl1 ctrl2 tgt qmap sym_tab =
  let _ = ignore sym_tab in
  let (cid1, cidx1) = ctrl1 in
  let (cid2, cidx2) = ctrl2 in
  let (tid, tidx) = tgt in
  match cidx1, cidx2, tidx with
  | Some ci1, Some ci2, Some ti ->
    [gate (QbitMap.find (cid1, ci1) qmap) (QbitMap.find (cid2, ci2) qmap) (QbitMap.find (tid, ti) qmap)]
  (* ignore other cases... *)
  | _ -> raise (Failure "ERROR: Not a qubit register")

let apply_gate gate (id, idx) qmap sym_tab =
  match idx with
  | Some i  -> [gate (QbitMap.find (id, i) qmap)]
  | None    ->
    match StringMap.find id sym_tab with
    | TQReg size -> List.init size (fun i -> gate (QbitMap.find (id, i) qmap))
    | _ -> raise (Failure "ERROR: Not a qubit register")

let rec interp_float_exp (e : exp) : float =
  match e with
  | Real r -> r
  | Pi -> Float.pi
  | Nninteger n -> Float.of_int n
  | UnaryOp (UMinus, e1) -> -. (interp_float_exp e1)
  | BinaryOp (Times, e1, e2) -> (interp_float_exp e1) *. (interp_float_exp e2)
  | BinaryOp (Div, e1, e2) -> (interp_float_exp e1) /. (interp_float_exp e2)
  | _ -> raise (Failure "NYI: Invalid float expression")

let rec interp_int_exp (e : exp) : int =
  match e with
  | Nninteger n -> n
  | UnaryOp (UMinus, e1) -> - (interp_int_exp e1)
  | _ -> raise (Failure "NYI: Invalid int expression")

let translate_statement s qmap sym_tab qasm_to_sqir_gate =
  match s with
  | Qop qop ->
    (match qop with
     | Uop uop -> qasm_to_sqir_gate uop qmap sym_tab
     | Meas _ -> print_endline ("NYI: Unsupported op: Measure"); []
     | Reset _ -> print_endline ("NYI: Reset"); [])
  | If _ -> print_endline ("NYI: If"); []
  | Barrier _ -> print_endline ("NYI: Unsupported op: Barrier"); []
  | _ -> []

let parse_decl (s : AST.statement) : (string * int) list =
  match s with
  | Decl d ->
    (match d with
     | QReg (name, size) ->
       List.map (fun i -> (name, i)) (List.init size (fun i -> i))
     | _ -> [])
  | _ -> []

let rec parse_qreg_decls' p acc =
  match p with
  | []      -> acc
  | s :: p' ->
    let first = parse_decl s in
    parse_qreg_decls' p' (acc @ first)
let parse_qreg_decls p = parse_qreg_decls' p []

let rec translate_program' p qbit_map sym_tab acc qasm_to_sqir_gate =
  match p with
  | []      ->  acc
  | s :: p' ->  
      let l = translate_statement s qbit_map sym_tab qasm_to_sqir_gate in
      translate_program' p' qbit_map sym_tab (acc @ l) qasm_to_sqir_gate
let translate_program p qbit_map sym_tab qasm_to_sqir_gate = 
  translate_program' p qbit_map sym_tab [] qasm_to_sqir_gate

let get_gate_list f qasm_to_sqir_gate =
  let ast = OpenQASM.get_ast f in (* dumb parsing *)
  let sym_tab = check ast in (* semantic analysis *)
  let qbit_list = parse_qreg_decls ast in
  let (qbit_map, n) = List.fold_left
      (fun (map, idx) entry -> (QbitMap.add entry idx map, idx+1))
      (QbitMap.empty, 0) qbit_list in
  (translate_program ast qbit_map sym_tab qasm_to_sqir_gate, n)

let ig q = App1 (StandardGateSet.U_I, q)
let x q = App1 (StandardGateSet.U_X, q)
let y q = App1 (StandardGateSet.U_Y, q)
let z q = App1 (StandardGateSet.U_Z, q)
let h q = App1 (StandardGateSet.U_H, q)
let s q = App1 (StandardGateSet.U_S, q)
let t q = App1 (StandardGateSet.U_T, q)
let sdg q = App1 (StandardGateSet.U_Sdg, q)
let tdg q = App1 (StandardGateSet.U_Tdg, q)
let rx r q = App1 (StandardGateSet.U_Rx(r), q)
let ry r q = App1 (StandardGateSet.U_Ry(r), q)
let rz r q = App1 (StandardGateSet.U_Rz(r), q)
let rzq r q = App1 (StandardGateSet.U_Rzq(r), q)
let u1 r1 q = App1 (StandardGateSet.U_U1(r1), q)
let u2 r1 r2 q = App1 (StandardGateSet.U_U2(r1,r2), q)
let u3 r1 r2 r3 q = App1 (StandardGateSet.U_U3(r1,r2,r3), q)
let cx q1 q2 = App2 (StandardGateSet.U_CX, q1, q2)
let cz q1 q2 = App2 (StandardGateSet.U_CZ, q1, q2)
let swap q1 q2 = App2 (StandardGateSet.U_SWAP, q1, q2)
let ccx q1 q2 q3 = App3 (StandardGateSet.U_CCX, q1, q2, q3)
let ccz q1 q2 q3= App3 (StandardGateSet.U_CCZ, q1, q2, q3)

let qasm_to_sqir_gate uop qmap sym_tab =
match uop with
| CX (ctrl, tgt) -> apply_c_gate cx ctrl tgt qmap sym_tab
| U _ -> raise (Failure "NYI: Generic Unitary")
| Gate (id, params, qargs) ->
   (match StringMap.find_opt id sym_tab with
    | Some TGate _ -> 
       (match id with
       | "id"  -> apply_gate ig (List.hd qargs) qmap sym_tab
       | "x"  -> apply_gate x (List.hd qargs) qmap sym_tab
       | "y"  -> apply_gate y (List.hd qargs) qmap sym_tab
       | "z"  -> apply_gate z (List.hd qargs) qmap sym_tab
       | "h"  -> apply_gate h (List.hd qargs) qmap sym_tab
       | "s"  -> apply_gate s (List.hd qargs) qmap sym_tab
       | "t"  -> apply_gate t (List.hd qargs) qmap sym_tab
       | "sdg"  -> apply_gate sdg (List.hd qargs) qmap sym_tab
       | "tdg"  -> apply_gate tdg (List.hd qargs) qmap sym_tab
       | "rx" -> let r = interp_float_exp (List.nth params 0) in
                 apply_gate (rx r) (List.hd qargs) qmap sym_tab
       | "ry" -> let r = interp_float_exp (List.nth params 0) in
                 apply_gate (ry r) (List.hd qargs) qmap sym_tab
       | "rz" -> let r = interp_float_exp (List.nth params 0) in
                 apply_gate (rz r) (List.hd qargs) qmap sym_tab
       | "rzq" -> let param = Q.of_ints (interp_int_exp (List.nth params 0)) (interp_int_exp (List.nth params 1)) in
                  apply_gate (rzq param) (List.hd qargs) qmap sym_tab            
       | "u1" -> let r1 = interp_float_exp (List.nth params 0) in
                 apply_gate (u1 r1) (List.hd qargs) qmap sym_tab
       | "u2" -> let r1 = interp_float_exp (List.nth params 0) in
                 let r2 = interp_float_exp (List.nth params 1) in
                 apply_gate (u2 r1 r2) (List.hd qargs) qmap sym_tab
       | "u3" -> let r1 = interp_float_exp (List.nth params 0) in
                 let r2 = interp_float_exp (List.nth params 1) in
                 let r3 = interp_float_exp (List.nth params 2) in
                 apply_gate (u3 r1 r2 r3) (List.hd qargs) qmap sym_tab
       | "cx" -> apply_c_gate cx (List.hd qargs) (List.nth qargs 1) qmap sym_tab
       | "cz" -> apply_c_gate cz (List.hd qargs) (List.nth qargs 1) qmap sym_tab
       | "swap" -> apply_c_gate swap (List.hd qargs) (List.nth qargs 1) qmap sym_tab
       | "ccx" -> apply_double_c_gate ccx (List.hd qargs) (List.nth qargs 1) (List.nth qargs 2) qmap sym_tab                 
       | "ccz" -> apply_double_c_gate ccz (List.hd qargs) (List.nth qargs 1) (List.nth qargs 2) qmap sym_tab     
       | _ -> raise (Failure ("NYI: Unsupported gate: " ^ id)))
    | Some _ -> raise (Failure ("ERROR: Not a gate: " ^ id))
    | None -> raise (Failure ("ERROR: Undefined gate: " ^ id)))

let read_qasm f = get_gate_list f qasm_to_sqir_gate

(** Write SQIR program out as OpenQASM **)

(* Currently too simple! Should convert back to OpenQASM AST and write from there! *)

let write_qasm_file fname p dim sqir_to_qasm_gate =
  let oc = open_out fname in
  (fprintf oc "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n\n";
   fprintf oc "qreg q[%d];\n" dim;
   fprintf oc "\n";
   ignore(List.map (sqir_to_qasm_gate oc) p);
   close_out oc)
   
let sqir_to_qasm_gate oc g =
   match g with
   | App1 (StandardGateSet.U_I, n) -> fprintf oc "id q[%d];\n" n
   | App1 (StandardGateSet.U_X, n) -> fprintf oc "x q[%d];\n" n
   | App1 (StandardGateSet.U_Y, n) -> fprintf oc "y q[%d];\n" n
   | App1 (StandardGateSet.U_Z, n) -> fprintf oc "z q[%d];\n" n
   | App1 (StandardGateSet.U_H, n) -> fprintf oc "h q[%d];\n" n
   | App1 (StandardGateSet.U_S, n) -> fprintf oc "s q[%d];\n" n
   | App1 (StandardGateSet.U_T, n) -> fprintf oc "t q[%d];\n" n
   | App1 (StandardGateSet.U_Sdg, n) -> fprintf oc "sdg q[%d];\n" n
   | App1 (StandardGateSet.U_Tdg, n) -> fprintf oc "tdg q[%d];\n" n
   | App1 (StandardGateSet.U_Rx(r), n) -> fprintf oc "rx(%f) q[%d];\n" r n
   | App1 (StandardGateSet.U_Ry(r), n) -> fprintf oc "ry(%f) q[%d];\n" r n
   | App1 (StandardGateSet.U_Rz(r), n) -> fprintf oc "rz(%f) q[%d];\n" r n
   | App1 (StandardGateSet.U_Rzq(q), n) -> 
       fprintf oc "rzq(%a,%a) q[%d];\n" Z.output (Q.num q) Z.output (Q.den q) n
   | App1 (StandardGateSet.U_U1(r1), n) -> fprintf oc "u1(%f) q[%d];\n" r1 n
   | App1 (StandardGateSet.U_U2(r1,r2), n) -> fprintf oc "u2(%f,%f) q[%d];\n" r1 r2 n
   | App1 (StandardGateSet.U_U3(r1,r2,r3), n) -> fprintf oc "u3(%f,%f,%f) q[%d];\n" r1 r2 r3 n
   | App2 (StandardGateSet.U_CX, m, n) -> fprintf oc "cx q[%d], q[%d];\n" m n
   | App2 (StandardGateSet.U_CZ, m, n) -> fprintf oc "cz q[%d], q[%d];\n" m n
   | App2 (StandardGateSet.U_SWAP, m, n) -> fprintf oc "swap q[%d], q[%d];\n" m n
   | App3 (StandardGateSet.U_CCX, m, n, p) -> fprintf oc "ccx q[%d], q[%d], q[%d];\n" m n p
   | App3 (StandardGateSet.U_CCZ, m, n, p) -> fprintf oc "ccz q[%d], q[%d], q[%d];\n" m n p
   (* badly typed case (e.g. App2 of H) -- should be unreachable *)
   | _ -> raise (Failure ("ERROR: Failed to write qasm file; unexpected gate in sqir_to_qasm_gate"))

let write_qasm p dim fname = write_qasm_file fname p dim sqir_to_qasm_gate 
