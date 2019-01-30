(* Distance-Based Boolean Applicability Domain (DBBAD)
   reference implementation.

   Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

module CLI = Minicli.CLI
module A = MyArray
module Ht = BatHashtbl
module L = MyList
module Mol = Molecules

let find_best_d dscan_fn global_res ds =
  (* get actives_tot and decoys_tot from HT *)
  let actives_tot, decoys_tot = Ht.find global_res 1.0 in
  Log.info "A: %d D: %d" actives_tot decoys_tot;
  let best_d, best_delta = ref 0.0, ref 0.0 in
  Utls.with_out_file dscan_fn (fun out ->
      fprintf out "#d afrac dfrac\n";
      L.iter (fun d ->
          let acard, dcard = Ht.find global_res d in
          let afrac, dfrac = float acard /. float actives_tot,
                             float dcard /. float decoys_tot in
          let delta = afrac -. dfrac in
          fprintf out "%f %f %f\n" d afrac dfrac;
          if delta > !best_delta then
            (best_delta := delta;
             best_d := d)
        ) ds;
      fprintf out "#best_d: %f\n" !best_d;
      Log.info "best_d: %f" !best_d;
    );
  !best_d

let apply_DBBAD ncores best_d train test =
  let actives_train = L.filter FpMol.is_active train in
  let actives_bst = Bstree.(create 1 Two_bands (A.of_list actives_train)) in
  let maybe_ok_test_mols =
    L.parmap ncores (fun test_mol ->
        (Dbad_common.mol_is_inside_global_AD test_mol best_d actives_bst,
         test_mol)
      ) test in
  let ok_test_mols =
    L.fold (fun acc (maybe_ok, mol) ->
        if maybe_ok then mol :: acc else acc
      ) [] maybe_ok_test_mols in
  let ok_card = L.length ok_test_mols in
  let test_card = L.length test in
  Log.info "passed AD: %d / %d" ok_card test_card;
  ok_test_mols

let demux input () =
  try Mol.read_one input
  with End_of_file -> raise Parany.End_of_input

let work best_d actives_bst test_mol =
  if Dbad_common.mol_is_inside_global_AD test_mol best_d actives_bst then
    Some test_mol
  else
    None

(* counters maintained by the demuxer *)
let mol_count = ref 0
let glob_ok_card = ref 0

let mux output maybe_mol =
  (match maybe_mol with
   | None -> ()
   | Some test_mol ->
     let () = incr glob_ok_card in
     FpMol.to_out output test_mol);
  incr mol_count;
  if !mol_count mod 1000 = 0 then
    (* user feedback *)
    eprintf "processed: %d\r%!" !mol_count

let apply_DBBAD_large_test ncores best_d train test_in test_out =
  let actives_train = L.filter FpMol.is_active train in
  let actives_bst = Bstree.(create 1 Two_bands (A.of_list actives_train)) in
  (* Parany *)
  Parany.run ~verbose:false ~csize:1 ~nprocs:ncores
    ~demux:(demux test_in)
    ~work:(work best_d actives_bst)
    ~mux:(mux test_out);
  Log.info "passed AD: %d / %d" !glob_ok_card !mol_count

let rand_split_in_three rng train' =
  let train = L.shuffle ~state:rng train' in
  let train_card = L.length train in
  let train_0, rest_0 = L.takedrop (train_card / 3) train in
  let train_1, train_2 = L.takedrop (train_card / 3) rest_0 in
  (train_0, train_1, train_2)

let main () =
  Log.color_on ();
  Log.set_log_level Log.DEBUG;
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage:\n\
              %s\n\
              --train <file>: file with encoded training set molecules\n\
              --test <file>: file with encoded test set molecules\n\
              [--seed <int>]: random seed\n\
              [-np <int>]: number of processors\n\
              [--large]: if test set does not fit in memory\n\
              [--dscan <file>]: where to store the scan\n"
       Sys.argv.(0);
     exit 1);
  let rng = match CLI.get_int_opt ["--seed"] args with
    | None -> BatRandom.State.make_self_init ()
    | Some seed -> BatRandom.State.make [|seed|] in
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let train_fn = CLI.get_string ["--train"] args in
  let test_fn = CLI.get_string ["--test"] args in
  let train_dbbad_fn = train_fn ^ ".dbbad" in
  let test_dbbad_fn = test_fn ^ ".dbbad" in
  let nb_features, train = Molecules.from_file train_fn in
  let dscan_fn = CLI.get_string_def ["--dscan"] args "/dev/null" in
  (* train the DBBAD on the training set *)
  let ds =
    let nb_steps = 101 in
    L.frange 1.0 `Downto 0.0 nb_steps in
  let global_res = Ht.create 11 in
  let train_0, train_1, train_2 = rand_split_in_three rng train in
  Dbad_common.global_dscan
    ds global_res (L.rev_append train_0 train_1) train_2;
  Dbad_common.global_dscan
    ds global_res (L.rev_append train_1 train_2) train_0;
  Dbad_common.global_dscan
    ds global_res (L.rev_append train_0 train_2) train_1;
  let best_d = find_best_d dscan_fn global_res ds in
  Log.info "best_d: %f" best_d;
  let large_testset = CLI.get_set_bool ["--large"] args in
  (* apply the DBBAD on the training set,
     for before and before-after modes *)
  let train_DBBAD = apply_DBBAD ncores best_d train train in
  Utls.with_out_file train_dbbad_fn (fun out ->
      L.iter (FpMol.to_out out) train_DBBAD
    );
  Log.info "train_DBBAD written to: %s" train_dbbad_fn;
  (* apply the DBBAD on the test set, for after and before-after modes *)
  if large_testset then
    Utls.with_in_file test_fn (fun input ->
        (* parse format header on 1st line *)
        let radius, index_fn = Mop2d_env.parse_comment input in
        let radius', mop2d_index = Mop2d_env.restore_mop2d_index index_fn in
        assert(radius = radius');
        let nb_features' = Hashtbl.length mop2d_index in
        assert(nb_features' = nb_features);
        (* process all molecules in // *)
        Utls.with_out_file test_dbbad_fn (fun output ->
            apply_DBBAD_large_test ncores best_d train input output
          );
        Log.info "test_DBBAD written to: %s" test_dbbad_fn
      )
  else
    let nb_features', test = Molecules.from_file test_fn in
    (* check molecules use the same encoding *)
    assert(nb_features' = nb_features);
    let test_DBBAD = apply_DBBAD ncores best_d train test in
    Utls.with_out_file test_dbbad_fn (fun out ->
        L.iter (FpMol.to_out out) test_DBBAD
      );
    Log.info "test_DBBAD written to: %s" test_dbbad_fn;
    (* report actives proportion before/after DBBAD *)
    let card_act_train, card_dec_train =
      L.filter_counts FpMol.is_active train in
    let card_act_test, card_dec_test =
      L.filter_counts FpMol.is_active test_DBBAD in
    let old_rate =
      let atrain = float card_act_train in
      let dtrain = float card_dec_train in
      atrain /. (atrain +. dtrain) in
    let new_rate =
      let atest = float card_act_test in
      let dtest = float card_dec_test in
      atest /. (atest +. dtest) in
    Log.info "A_train: %d D_train: %d AD_A_test: %d AD_D_test: %d \
              old: %f new: %f EF: %.3f"
      card_act_train card_dec_train
      card_act_test card_dec_test
      old_rate new_rate (new_rate /. old_rate)

let () = main ()
