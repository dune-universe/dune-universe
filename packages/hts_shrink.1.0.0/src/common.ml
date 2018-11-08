(* functionalities common to several modules *)

(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

module Fp = Fingerprint
module IntMap = MyIntMap
module L = MyList

let mol_is_active line =
  BatString.starts_with line "active"

let mol_get_name line =
  fst (BatString.split line ~by:",")

(* create the corresponding tsv line for the data file and the labels file
   (for GistSVM) -> (data_line, label_line) *)
let to_tsv_lines line =
  (* put a tab before each char of 's' *)
  let tab_prepend_all s =
    let buff = Buffer.create 1024 in
    BatString.iter (fun c ->
        assert(c = '1' || c = '0');
        Buffer.add_char buff '\t';
        Buffer.add_char buff c
      ) s;
    Buffer.contents buff in
  match BatString.split_on_char ',' line with
  | [name; _ic50; bitstring] ->
    let data_csv_line = sprintf "%s%s\n" name (tab_prepend_all bitstring) in
    let label_csv_line =
      if mol_is_active line then
        sprintf "%s\t1.0\n" name
      else
        sprintf "%s\t-1.0\n" name in
    (data_csv_line, label_csv_line)
  | _ -> failwith ("to_tab_sep_csv_lines: cannot parse: " ^ line)

(* lookup fingerprint length in file *)
let get_fp_length fn =
  let fst_line = Utls.get_first_line fn in
  match BatString.split_on_char ',' fst_line with
  | [_name; _ic50; bitstring] ->
    let n = String.length bitstring in
    assert(n = Flags.maccs_length ||
           n = Flags.ecfp4_length ||
           n = Flags.pubch_length);
    Utls.tap (Log.info "FP length: %d") n
  | _ -> failwith ("get_fp_length: cannot parse: " ^ fst_line)

let convert_FP_file_to_tsv_files fp_fn data_tsv_fn labels_tsv_fn =
  let fst_line = Utls.get_first_line fp_fn in
  let fp_bitstring = Utls.cut ',' 2 fst_line in
  let nb_cols = 1 + String.length fp_bitstring in
  Utls.with_out_file data_tsv_fn (fun data_out ->
      fprintf data_out "name";
      for i = 0 to nb_cols - 2 do
        fprintf data_out "\t%d" i
      done;
      fprintf data_out "\n";
      Utls.with_out_file labels_tsv_fn (fun labels_out ->
          fprintf labels_out "name\tlabel\n";
          Utls.iter_on_lines_of_file fp_fn (fun line ->
              let data_line, label_line = to_tsv_lines line in
              let n = 1 + MyString.count_char data_line '\t' in
              assert(n = nb_cols);
              fprintf data_out "%s" data_line;
              fprintf labels_out "%s" label_line
            );
        )
    )

(* create n-folds CV sets *)
let create_CV_sets n actives decoys =
  (* create all training and test sets needed to perform n folds CV *)
  let loop all_test_sets =
    L.map (fun test_set ->
        let train_sets = L.filter (fun x -> x <> test_set) all_test_sets in
        let train_set =
          L.fold_left (fun (acts_acc, decs_acc) (acts, decs) ->
              (L.rev_append acts acts_acc, L.rev_append decs decs_acc)
            ) ([], []) train_sets in
        (train_set, test_set)
      ) all_test_sets in
  let act_lists = L.nparts n actives in
  let dec_lists = L.nparts n decoys in
  let test_sets = L.combine act_lists dec_lists in
  loop test_sets

type split_style =
  | Train_portion of float (* portion of the whole dataset used to train *)
  | Nb_folds of int (* number of folds of cross validation *)

type 'a split_result =
  | Train_test of 'a list * 'a list (* (train_set, test_set) *)
  | Folds of 'a list list (* each slice can be used to test;
                             all the others are used to train *)

let int_of_split_style = function
  | Train_portion _ -> 1
  | Nb_folds n -> n

(* cut a list of molecules into a training and a test set; while
   preserving the ratio decoys/active
   Example: train_s, test_s = train_test_split 0.8 whole_s *)
let train_test_split style lines =
  (* randomize molecules *)
  let lines = L.shuffle ~state:(BatRandom.State.make_self_init ()) lines in
  let all_actives, all_decoys = L.partition mol_is_active lines in
  let n_acts = L.length all_actives in
  let n_decs = L.length all_decoys in
  match style with
  | Train_portion p ->
    assert(p >= 0.0 && p <= 1.0);
    let nb_act_train = BatFloat.round_to_int (p *. float n_acts) in
    let nb_dec_train = BatFloat.round_to_int (p *. float n_decs) in
    let acts_train, acts_test = L.takedrop nb_act_train all_actives in
    let decs_train, decs_test = L.takedrop nb_dec_train all_decoys in
    Train_test (L.rev_append acts_train decs_train,
                L.rev_append acts_test decs_test)
  | Nb_folds nfolds ->
    let act_lists = L.nparts nfolds all_actives in
    let dec_lists = L.nparts nfolds all_decoys in
    let act_dec_lists = L.combine act_lists dec_lists in
    let test_sets =
      L.map (fun (acts, decs) ->
          L.rev_append acts decs
        ) act_dec_lists in
    Folds test_sets

(* read one molecule from an FP file *)
let read_one_mol line =
  Scanf.sscanf line "%s@,%f,%s" (fun name _ic50 bitstring ->
      (name, bitstring)
    )

let binary_of_char = function
  | '0' -> 0
  | '1' -> 1
  | c -> failwith (sprintf "Rf_train.binary_of_char: not 0 or 1: %c" c)

let read_mol_raw (nb_features: int) (mol_line: string): int array =
  match BatString.split_on_char ',' mol_line with
  | [name; _ic50; unfolded_fp] ->
    let active_flag = Utls.int_of_bool (mol_is_active name) in
    let imap = MyIntMap.of_string unfolded_fp in
    let res = Array.make (nb_features + 1) 0 in
    (* Ranger needs the value to predict as one of the features;
       we put it last *) 
    res.(nb_features) <- active_flag;
    MyIntMap.iter (fun k v ->
        assert(k >= 0 && k < nb_features && v > 0);
        res.(k) <- v
      ) imap;
    res
  | _ -> failwith ("Common.read_mol_raw: cannot parse: " ^ mol_line)

(* read the mol raw but forget its activity flag *)
let read_mol_raw_no_act (nb_features: int) (mol_line: string): int array =
  let a = read_mol_raw nb_features mol_line in
  Array.sub a 0 nb_features

let get_nb_features fn =
  Utls.with_in_file fn (fun input ->
      let radius, index_fn = Mop2d_env.parse_comment input in
      let radius', mop2d_index = Mop2d_env.restore_mop2d_index index_fn in
      assert(radius = radius');
      Hashtbl.length mop2d_index
    )

(* actives % is preserved in each split *)
let random_half_split rng is_active mols' =
  let mols = L.shuffle ~state:rng mols' in
  let actives, decoys = L.partition is_active mols in
  let card_act, card_dec = L.length actives, L.length decoys in
  let act_train, act_test = L.takedrop (card_act / 2) actives in
  let dec_train, dec_test = L.takedrop (card_dec / 2) decoys in
  let train = L.rev_append act_train dec_train in
  let test = L.rev_append act_test dec_test in
  (L.shuffle ~state:rng train, L.shuffle ~state:rng test)
