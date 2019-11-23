open Printf

module Log = Dolog.Log.Make(struct let section = "Mole" end)

module Fp = Fingerprint
module Ht = BatHashtbl
module L = MyList
module StringSet = BatSet.String

module Utls = MyUtils

let no_index = -1

type t =
  { index: int; (* rank in input file *)
    name: string; (* molecule name *)
    ic50: float; (* IC50 *)
    fp: Fp.t } (* fingerprint *)

let get_index m = m.index
let get_name m = m.name
let get_ic50 m = m.ic50
let get_fp m = m.fp

let is_active (m: t): bool =
  BatString.starts_with m.name "active"

let get_bits (m: t): Bitv.t =
  Fp.get_bits m.fp

let count_set_bits (m: t): int =
  Fp.count_set_bits m.fp

let create index name ic50 fp =
  { index; name; ic50; fp }

let of_maccs_string (index: int) (s: string): t =
  Scanf.sscanf s "%s@,%f,%s" (fun name ic50 fp_string ->
      create index name ic50 (Fp.of_maccs_string fp_string)
    )

let of_ecfp4_string (index: int) (s: string): t =
  Scanf.sscanf s "%s@,%f,%s" (fun name ic50 fp_string ->
      create index name ic50 (Fp.of_ecfp4_string fp_string)
    )

let of_pubch_string (index: int) (s: string): t =
  Scanf.sscanf s "%s@,%f,%s" (fun name ic50 fp_string ->
      create index name ic50 (Fp.of_pubch_string fp_string)
    )

let of_mop2d_string (index: int) (s: string): t =
  Scanf.sscanf s "%s@,%f,%s" (fun name ic50 fp_string ->
      create index name ic50 (Fp.of_mop2d_string fp_string)
    )

let to_string m: string =
  sprintf "%s,%f,%s" m.name m.ic50 (Fp.to_string m.fp)

let name_in_set m set =
  StringSet.mem m.name set

let most_potent_first (l: t list): t list =
  (* the most active molecule has the smallest ic50 value; so we just
     sort in increasing order *)
  L.sort (fun m1 m2 ->
      BatFloat.compare (get_ic50 m1) (get_ic50 m2)
    ) l

(* compressed marshalling *)
let to_chan (out: Gzip.out_channel) (m: t): unit =
  let to_compress = Marshal.to_string m [Marshal.No_sharing] in
  Utls.gzip_output_string out to_compress

let from_chan (input: Gzip.in_channel): t =
  let header = Bytes.create Marshal.header_size in
  Gzip.really_input input header 0 Marshal.header_size;
  let buffer = Bytes.create (Marshal.total_size header 0) in
  Gzip.really_input input buffer Marshal.header_size
    (Marshal.data_size header 0);
  Bytes.unsafe_blit header 0 buffer 0 Marshal.header_size;
  Marshal.from_bytes buffer 0

(* supported file formats *)
type file_format = SDF
                 | MOL2
                 | SMI
                 | CSV (* a csv file I crafted myself (with MACCS Fps) *)
                 | ECFP4 (* a csv file I crafted myself (with ECFP4 Fps) *)
                 | PUBCH (* a csv file I crafted myself (with PUBCH Fps) *)
                 | MOP2D (* a csv file I crafted myself (with MOP2D Fps) *)
                 | BIN (* marshalling output format *)

let file_format_of_filename (fn: string): file_format =
  if BatString.ends_with fn ".bin" then BIN
  else if BatString.ends_with fn ".sdf" then SDF
  else if BatString.ends_with fn ".mol2" then MOL2
  else if BatString.ends_with fn ".smi" then SMI
  else if BatString.ends_with fn ".csv" ||
          BatString.ends_with fn ".maccs" then CSV
  else if BatString.ends_with fn ".ecfp4" then ECFP4
  else if BatString.ends_with fn ".pubc" then PUBCH
  else if BatString.ends_with fn ".mop2d" then MOP2D
  else failwith ("unsupported file format: " ^ fn)

(* find the ob_maccs exe *)
let do_we_have_ob_maccs (): string option =
  Utls.find_command "lbvs_consent_ob_maccs" "OB_MACCS_EXE"

(* find ecfp4.py script *)
let do_we_have_ecfp4_py (): string option =
  Utls.find_command "lbvs_consent_ecfp4.py" "ECFP4_PY_SCRIPT"

let read_and_encode_one (fp: Flags.fingerprint) (encode_cmd: string)
  : in_channel -> t =
  match fp with
  | Flags.MOP2D -> failwith "read_and_encode_one: MOP2D not supported"
  | Flags.MACCS ->
    (fun input ->
       let mol = Sdf.read_one input in
       let tmp_fn = Filename.temp_file "" (* no_prefix *) ".sdf" in
       Utls.with_out_file tmp_fn (fun out -> fprintf out "%s" mol);
       let maccs_str = Utls.get_command_output (encode_cmd ^ tmp_fn) in
       Sys.remove tmp_fn;
       of_maccs_string no_index maccs_str)
  | Flags.ECFP4 ->
    (fun input ->
       let mol = Sdf.read_one input in
       let tmp_fn = Filename.temp_file "" (* no_prefix *) ".sdf" in
       Utls.with_out_file tmp_fn (fun out -> fprintf out "%s" mol);
       let ecfp4_str = Utls.get_command_output (encode_cmd ^ tmp_fn) in
       Sys.remove tmp_fn;
       of_ecfp4_string no_index ecfp4_str)

(* given a filename, retrieves the function
   that can read one molecule at a time from it *)
let mol_reader_of_filename (fn: string) =
  match file_format_of_filename fn with
  | BIN -> failwith "mol_reader_of_filename: .bin not supported"
  | CSV ->
    (fun input -> of_maccs_string no_index (input_line input))
  | ECFP4 ->
    (fun input -> of_ecfp4_string no_index (input_line input))
  | PUBCH ->
    (fun input -> of_pubch_string no_index (input_line input))
  | MOP2D ->
    if !Flags.curr_fingerprint <> Flags.MOP2D then
      Log.debug "mol_reader_of_filename: Flags.curr_fingerprint <> MOP2D";
    (fun input -> of_mop2d_string no_index (input_line input))
  (*
  | MOL2 -> failwith "mol_reader_of_filename: .mol2 not supported \
                         (obabel Luke!)"
  | SMI -> failwith "mol_reader_of_filename: .smi not supported \
                        (obabel Luke!)"
  *)
  | MOL2 | SMI | SDF ->
    match !Flags.curr_fingerprint with
    | Flags.MOP2D -> failwith "mol_reader_of_filename: MOP2D not supported"
    | Flags.MACCS ->
      let ob_maccs = BatOption.get (do_we_have_ob_maccs ()) in
      read_and_encode_one !Flags.curr_fingerprint (ob_maccs ^ " ")
    | Flags.ECFP4 ->
      let ecfp4_py = BatOption.get (do_we_have_ecfp4_py ()) in
      read_and_encode_one !Flags.curr_fingerprint (ecfp4_py ^ " ")

(* given a filename, retrieves the function
   that can read one molecule at a time from it *)
let mol_reader_of_filename_fp (fp: Flags.fingerprint) =
  match fp with
  | Flags.MOP2D -> failwith "mol_reader_of_filename_fp: MOP2D unsupported"
  | Flags.MACCS ->
    let ob_maccs = BatOption.get (do_we_have_ob_maccs ()) in
    read_and_encode_one !Flags.curr_fingerprint (ob_maccs ^ " ")
  | Flags.ECFP4 ->
    let ecfp4_py = BatOption.get (do_we_have_ecfp4_py ()) in
    read_and_encode_one !Flags.curr_fingerprint (ecfp4_py ^ " ")

let from_some_file (fmt: file_format) (fn: string): t list =
  Log.info "reading molecules from %s ..." fn;
  let index = ref 0 in
  let res = ref [] in
  begin match fmt with
    | CSV | ECFP4 | PUBCH | MOP2D | SDF ->
      let mol_reader = mol_reader_of_filename fn in
      let out_fn, mol_tap =
        let bin_out_fn = fn ^ ".bin" in
        if Utls.lock_file_for_writing bin_out_fn then
          (Log.info "creating %s" bin_out_fn;
           (bin_out_fn, fun output mol -> to_chan output mol))
        else
          (Log.info "not overwriting %s" bin_out_fn;
           ("/dev/null", fun _output _mol -> ())) in
      let clevel = 1 in
      MyUtils.with_gzip_out_file clevel out_fn (fun output ->
          MyUtils.with_in_file fn (fun input ->
              try
                while true do
                  let mol = mol_reader input in
                  mol_tap output mol;
                  incr index;
                  res := mol :: !res
                done
              with End_of_file -> ())
        )
    | BIN ->
      MyUtils.with_gzip_in_file fn (fun input ->
          try
            while true do
              let mol = from_chan input in
              incr index;
              res := mol :: !res;
            done
          with End_of_file -> ())
    | MOL2 -> failwith "from_some_file: .mol2 not supported"
    | SMI -> failwith "from_some_file: .smi not supported"
  end;
  Log.info "read %d molecules from %s" !index fn;
  if !index = 0 then
    failwith "check the file; only molecules are allowed in it";
  !res

let from_file_fp (fp_type: Flags.fingerprint) (fn: string): t list =
  match fp_type with
  | Flags.MOP2D -> failwith "from_file_fp: MOP2D unsupported"
  | Flags.MACCS ->
    Log.info "MACCS fp";
    let maybe_ob_maccs = do_we_have_ob_maccs () in
    let ob_maccs = BatOption.get maybe_ob_maccs in
    let maccs_fn = fn ^ ".maccs" in
    let convert = sprintf "%s %s > %s" ob_maccs fn maccs_fn in
    MyUtils.run_command convert;
    from_some_file CSV maccs_fn
  | Flags.ECFP4 ->
    Log.info "ECFP4 fp";
    let maybe_ecfp4_py = do_we_have_ecfp4_py () in
    let ecfp4_py = BatOption.get maybe_ecfp4_py in
    let ecfp4_fn = fn ^ ".ecfp4" in
    let convert = sprintf "%s %s > %s" ecfp4_py fn ecfp4_fn in
    MyUtils.run_command convert;
    from_some_file ECFP4 ecfp4_fn

let from_file (fn: string): t list =
  match do_we_have_ob_maccs (), file_format_of_filename fn with
  | _, BIN -> from_some_file BIN fn
  | _, ECFP4 -> from_some_file ECFP4 fn
  | _, PUBCH -> from_some_file PUBCH fn
  | _, MOP2D -> from_some_file MOP2D fn
  | None, SDF -> from_some_file SDF fn
  | None, MOL2 -> from_some_file MOL2 fn
  | _, CSV -> from_some_file CSV fn
  | Some ob_maccs, (SDF | MOL2 | SMI) ->
    begin match !Flags.curr_fingerprint with
      | Flags.MOP2D -> failwith "not implemented yet"
      | Flags.MACCS ->
        let maccs_fn = fn ^ ".maccs" in
        let convert = sprintf "%s %s > %s" ob_maccs fn maccs_fn in
        MyUtils.run_command convert;
        from_some_file CSV maccs_fn
      | Flags.ECFP4 ->
        let maybe_ecfp4_py = do_we_have_ecfp4_py () in
        let ecfp4_py = BatOption.get maybe_ecfp4_py in
        let ecfp4_fn = fn ^ ".ecfp4" in
        let convert = sprintf "%s %s > %s" ecfp4_py fn ecfp4_fn in
        MyUtils.run_command convert;
        from_some_file ECFP4 ecfp4_fn
    end
  | _ -> assert(false)

(* downsize a database (both actives and inactives are downsized) *)
let from_downsized_file downsize fn =
  let mols = from_file fn in
  assert(downsize > 0.0 && downsize < 1.0);
  let fresh_rands = BatRandom.State.make_self_init () in
  let actives, inactives = L.partition is_active (L.shuffle ~state:fresh_rands mols) in
  let n, m = Pair.map L.length (actives, inactives) in
  let i = int_of_float (MyUtils.round (downsize *. float n)) in
  let j = int_of_float (MyUtils.round (downsize *. float m)) in
  Log.warn "%d molecules after downsizing DB" (i + j);
  let actives_dsized = BatList.take i actives in
  let inactives_dsized = BatList.take j inactives in
  (actives_dsized, actives_dsized @ inactives_dsized)

(* read molecules from a pre-computed fingerprints file *)
let from_fp_file (fn: string): t list =
  match file_format_of_filename fn with
  | BIN ->   from_some_file BIN   fn
  | ECFP4 -> from_some_file ECFP4 fn
  | PUBCH -> from_some_file PUBCH fn
  | MOP2D -> from_some_file MOP2D fn
  | CSV   -> from_some_file CSV   fn
  | _ -> failwith ("from_fp_file: unsupported: " ^ fn)

let logarithmic_potency_scale (queries': t list)
  : ((float * float) list * (string, float) Ht.t) =
  let queries = most_potent_first queries' in
  let least_active =
    (* the least_active has the largest ic50 float(mol/L) *)
    (* (compound needs to be present in high concentration) *)
    get_ic50 (BatList.last queries) in
  let log_least_active = log least_active in
  let name2weight = Ht.create 11 in
  let ic50_weights =
    L.map (fun mol ->
      (* Bajorath potency scaling from:
         "Analysis of a High-Throughput Screening Data Set
         Using Potency-Scaled Molecular Similarity Algorithms".
         DOI: 10.1021/ci6005432 *)
      let ic50 = get_ic50 mol in
      let weight = log_least_active -. (log ic50) +. 1.0 in
      Ht.replace name2weight mol.name weight;
      (ic50, weight)
      ) queries in
  (ic50_weights, name2weight)

(* Francois Berenger's creation *)
let linear_potency_scale (queries': t list)
  : ((float * float) list * (string, float) Ht.t) =
  let queries = most_potent_first queries' in
  let n = L.length queries in
  let weights = L.frange 10.0 `Downto 1.0 n in
  let names = L.map get_name queries in
  let ic50s = L.map get_ic50 queries in
  let name_weights = L.combine names weights in
  let name2weight = Ht.of_list name_weights in
  let ic50_weights = L.combine ic50s weights in
  (ic50_weights, name2weight)

(* transform each IC50 into a weighting factor *)
let potency_scale (queries: t list): ((float * float) list * (string, float) Ht.t) =
  match !Flags.potency_scaling with
  | Flags.Logarithmic -> logarithmic_potency_scale queries
  | Flags.Linear -> linear_potency_scale queries

(* load compound name to cluster ID mapping from file
   each line must be
---
mol_name BM_framework cluster_id
--- *)
let load_clusters (fn: string): (string, int) Ht.t =
  let res = Ht.create 11 in
  Utls.iter_on_lines_of_file fn (fun line ->
      (* skip comments *)
      if not (BatString.starts_with line "#") then
        Scanf.sscanf line "%s@ %s@ %d" (fun name _framework cluster_id ->
            Ht.replace res name cluster_id
          )
    );
  res

let tanimoto_score m1 m2 =
  Score.fp_tanimoto_score (get_fp m1) (get_fp m2)

let to_score_label (score: float) (m: t): Score_label.t =
  Score_label.create m.name score m.index (is_active m)
