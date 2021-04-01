open Printf
open Voqc.Qasm
open Voqc.Main

(* Requested transformations will be applied in the following order:
   Nam optimization -> IBM optimization -> Mapping
   At most one mapping routine will be applied, the precedence is lnn > lnnring > grid > tenerife

   Usage:

    -i <f> : input file
    -o <f> : output file
    -optimize-nam : run the Nam optimizations
    -optimize-nam-light : run the light version of optimize-nam
    -optimize-nam-lcr <n> : run LCR optimization with Nam optimizations; give gate counts for n iterations of the input program (incompatible with all other options except -i and -o)
    -optimize-ibm : run the IBM optimizations
    -lnn <n> : map to n-qubit LNN architecture with a given input layout (specified after --)
    -lnnring <n> : map to n-qubit LNNRing architecture with a given input layout (specified after --)
    -grid <m> <n> : map to m x n-qubit 2d-grid architecture with a given input layout (specified after --)
    -tenerife : map to IBM's 5-qubit Tenerife architecture with a given input layout (specified after --)
    -- : initial layout for mapping
*)

(* Print gate counts *)
let print_gc (gc : gate_counts) =
  match gc with
  | BuildCounts (i, x, y, z, h, s, t, sdg, tdg, rx, ry, rz, rzq, u1, u2, u3, cx, cz, swap, ccx, ccz) ->
      (printf "I : %d, X : %d, Y : %d, Z : %d, H : %d, %!" i x y z h; 
       printf "S : %d, T : %d, Sdg : %d, Tdg : %d, Rx : %d, %!" s t sdg tdg rx; 
       printf "Ry : %d, Rz : %d, Rzq : %d, U1 : %d, U2 : %d, U3 : %d, %!" ry rz rzq u1 u2 u3; 
       printf "CX : %d, CZ : %d, SWAP : %d, CCX : %d, CCZ : %d%!\n" cx cz swap ccx ccz) 

(* Print mapping layout *)
let print_layout la n = List.iter (printf "%d ") (layout_to_list la n); printf "\n"

(* Run mapping and print statistics *)
let run_mapping n dim c la cg = (* n = # qubits in prog, dim = # qubits on machine *)
    if n > dim then failwith "ERROR: Program uses too many qubits for the given architecture" else
    let la = list_to_layout la in
    if not (check_layout la dim) then failwith "ERROR: Invalid input layout" else
    let _ = (printf "Input layout: "; print_layout la dim) in
    let (c',la') = simple_map c la cg in
    let gc = count_gates c' in
    let _ = (printf "Input layout: "; print_layout la' dim) in
    let _ = printf "Gate counts after mapping (%d total):\n" (total_gate_count c') in
    let _ = print_gc gc in
    c'

(* Argument parsing *)
let inf = ref ""
let outf = ref ""
let optimnam = ref false
let lcr = ref 0
let light = ref false
let lnn = ref 0
let lnnring = ref 0
let gridx = ref 0
let gridy = ref 0
let tenerife = ref false
let optimibm = ref false
let layout : int list ref = ref []
let add_to_layout (s:string) = layout := (!layout @ [int_of_string s])
let usage = "usage: " ^ Sys.argv.(0) ^ " -i string -o string [-optimize-nam] [-optimize-nam-light] [-optimize-nam-lcr int] [-optimize-ibm] [-lnn int] [-lnnring int] [-grid int int] [-tenerife] [-optimize-ibm] [-- int list]"
let speclist = [
    ("-i", Arg.Set_string inf, ": input file");
    ("-o", Arg.Set_string outf, ": output file");
    ("-optimize-nam", Arg.Set optimnam, ": run the Nam optimizations");
    ("-optimize-nam-light", Arg.Set light, ": run the light version of optimize-nam");
    ("-optimize-nam-lcr", Arg.Set_int lcr,  ": run LCR optimization with Nam optimizations; give gate counts for n iterations of the input program (incompatible with all other options except -i and -o)");
    ("-optimize-ibm", Arg.Set optimibm, ": run the IBM optimizations");
    ("-lnn", Arg.Set_int lnn, ": map to n-qubit LNN architecture with a given input layout (specified after --)");
    ("-lnnring", Arg.Set_int lnnring, ": map to n-qubit LNNRing architecture with a given input layout (specified after --)");
    ("-grid", Arg.Tuple ([Arg.Set_int gridx ; Arg.Set_int gridy]), ": map to m x n-qubit 2d-grid architecture with a given input layout (specified after --)");
    ("-tenerife", Arg.Set tenerife, ": map to IBM's 5-qubit Tenerife architecture with a given input layout (specified after --)");
    ("--", Arg.Rest add_to_layout, ": initial layout for mapping");
  ]
let () =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
if !inf = "" then printf "ERROR: Input filename (-i) required.\n" else 
if !outf = "" then printf "ERROR: Output filename (-o) required.\n" else 
let _ = printf "Input file: %s\nOutput file: %s\n" !inf !outf in
if !lcr <> 0 && !lcr < 3 then printf "ERROR: LCR option requires an argument >= 2\n" else
let (c, n) = read_qasm !inf in
let _ = printf "Input program uses %d gates and %d qubits\n" (total_gate_count c) n in
if !lcr <> 0 
then (
    let _ = printf "LCR option selected with %d iterations\n" !lcr in
    match optimize_nam_lcr c with
    | None -> printf "ERROR: LCR optimization failed\n"
    | Some ((l,c0),r) -> 
        let inc = scale_count (count_gates c) !lcr in
        let outc = count_gates_lcr ((l,c0),r) !lcr in
        let _ = printf "Gates required for n iterations of the original circuit:\n" in
        let _ = print_gc inc in
        let _ = printf "Gates required for n iterations of the optimized circuit:\n" in
        print_gc outc
) else (
    let _ = if !optimnam && not !light then printf "Nam optimization enabled\n" else () in
    let _ = if !light then printf "Nam optimization (light) enabled\n" else () in
    let _ = if !optimibm then printf "IBM optimization enabled\n" else () in
    let inc = count_gates c in
    let _ = printf "Input gate counts (%d total):\n" (total_gate_count c) in
    let _ = print_gc inc in
    let c1 = if !optimnam && not !light then optimize_nam c else c in
    let c2 = if !light then optimize_nam_light c1 else c1 in
    let c3 = if !optimibm then optimize_ibm c2 else c2 in
    let outc = count_gates c3 in
    let _ = printf "Gate counts after optimization (%d total):\n" (total_gate_count c3) in
    let _ = print_gc outc in
    let c4 = if !lnn > 0 then (
                 let cg = make_lnn !lnn in
                 let _ = printf "LNN mapping enabled\n" in
                 run_mapping n !lnn c3 !layout cg 
             ) else if !lnnring > 0 then (
                 let cg = make_lnn_ring !lnnring in
                 let _ = printf "LNN ring mapping enabled\n" in
                 run_mapping n !lnnring c3 !layout cg 
             ) else if !gridx > 0 && !gridy > 0 then (
                 let cg = make_grid !gridx !gridy in
                 let _ = printf "LNN mapping enabled\n" in
                 run_mapping n (!gridx * !gridy) c3 !layout cg 
             ) else if !tenerife then (
                 let cg = make_tenerife () in
                 let _ = printf "LNN mapping enabled\n" in
                 run_mapping n 5 c3 !layout cg 
             ) else c3 in
    let n' = if !lnn > 0 then !lnn else 
             if !lnnring > 0 then !lnnring else
             if !gridx > 0 && !gridy > 0 then (!gridx * !gridy) else
             if !tenerife then 5 else n in
    write_qasm c4 n' !outf 
)

