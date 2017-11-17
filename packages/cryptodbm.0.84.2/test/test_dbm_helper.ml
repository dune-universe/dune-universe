(* Configuration for one test shot. 
 * A test shot basically consists in:
 *   - create a database file.
 *   - create a few subtables, fill them with data.
 *   - while filling with data, try occasionally to close the file then open it in append mode,
 *     and try occasionally to flush it. Also, put more bindings than expected, then delete the extra bindings.
 *   - before closing the file, compare the content with what is expected.
 *   - close the file.
 *   - reopen the file and compare the content with what is expected. Try both open_only_uncrypted, open_read, open_append.
 * *)
type test_config =
  { 
    (* Table config *)
    filename: string ;

    (* Sort of random number. *)
    random: int ;

    iterations: int option ;

    passwd: string ;
    signwd: string ;
    max_extra_key: int ;
    max_extra_data: int ;
    max_extra_bindings: int ;

    (* Subtables *)
    subtables: subtable_config list ;

    (* Number of flushes to test (typically between 0 and 3). *)
    flush_nb: int ;

    (* Number of close/append to test (typically between 0 and 3). *)
    append_nb: int ;

  }

and subtable_config =
  { name: string ;
    crypted: string option ; (* None means explicitly uncrypted subtable. *)
    signed: string ;

    st_iterations: int option ;
    max_key_pad: int option ;
    max_data_pad: int option ;
    content: (string * string) list ;

    (* Number of subtable close/append to test (typically between 0 and 2). *)
    sub_append_nb: int ;

    (* Number of parasite bindings, to be deleted at some moment. *)
    parasite: int ; }

(* Instructions corresponding to a test configuration. *)
type instruction =
  (* Creates a subtable. *)
  | Create of subtable_config

  (* Insert or remove (subtable, key, data). *)
  | Insert of subtable_config * string * string
  | Remove of subtable_config * string

  (* Close the file then reopen in append mode. *) 
  | Reopen

  (* Similarly for a subtable. *)
  | Subreopen of subtable_config

  | Flush

(* State when playing a sequence of instructions. *)
type state =
  { mutable table: Cryptodbm.full Cryptodbm.table ;
    mutable subs: (subtable_config * Cryptodbm.full Cryptodbm.subtable) list ;
    mutable contents: (subtable_config * (string, string) Hashtbl.t) list }

let esc = String.escaped

let instruction2s = function 
  | Create sconf -> Printf.sprintf "Create %s" sconf.name
  | Insert (sconf, key, data) -> Printf.sprintf "Insert [%s] => [%s] in %s" (esc key) (esc data) sconf.name
  | Remove (sconf, key) -> Printf.sprintf "Remove [%s] from %s" (esc key) sconf.name
  | Reopen -> "Reopen"
  | Subreopen sconf -> Printf.sprintf "Subreopen %s" sconf.name
  | Flush -> "Flush"

let sep map sp l = List.fold_left (fun acu x -> if acu = "" then map x else acu ^ sp ^ (map x)) "" l

let sequence2s seq = sep instruction2s ",\n  " seq

let it2s = function
  | None -> "none"
  | Some n -> string_of_int n

let show_subconf sconf =
  Printf.printf "=== Subtable %s ===\n" sconf.name ;

  begin match sconf.crypted with
    | None -> Printf.printf "  UNCRYPTED\n"
    | Some p -> Printf.printf "  Encrypted with '%s'\n" (esc p)
  end ;

  if sconf.signed = "" then Printf.printf "  Unsigned\n"
  else Printf.printf "  Signed with '%s'\n" (esc sconf.signed) ;

  Printf.printf "  Key_pad = " ;
  begin match sconf.max_key_pad with
    | None -> Printf.printf "(none), "
    | Some d -> Printf.printf "%d, " d
  end ;

  Printf.printf "  data_pad = " ;
  begin match sconf.max_data_pad with
    | None -> Printf.printf "(none)\n"
    | Some d -> Printf.printf "%d\n" d
  end ;

  Printf.printf "  Iterations : %s\n" (it2s sconf.st_iterations) ;
  Printf.printf "  Number of close/append: %d\n" sconf.sub_append_nb ;
  Printf.printf "  Number of parasite bindings: %d\n" sconf.parasite ;

  Printf.printf "  Content : %s\n\n" (Helper.sep (fun (k,d) -> (esc k) ^ " => " ^ (esc d)) ", " sconf.content) ;
  ()

let show_conf tconf =
  Printf.printf "======  FILE %s  ======\n\n" tconf.filename ;
  Printf.printf "  Seed: %d\n" tconf.random ;
  Printf.printf "  Iterations: %s\n" (it2s tconf.iterations) ;
  Printf.printf "  Passwd: '%s'\n" (esc tconf.passwd) ;
  Printf.printf "  Signwd: '%s'\n" (esc tconf.signwd) ;
  Printf.printf "  Key_pad = %d,  data_pad = %d,  extra = %d\n" tconf.max_extra_key tconf.max_extra_data tconf.max_extra_bindings ;
  Printf.printf "  Number of flushes: %d,   number of append: %d\n" tconf.flush_nb tconf.append_nb ;
  Printf.printf "\n" ;
  if tconf.subtables = [] then Printf.printf " [no subtables]\n"
  else List.iter show_subconf tconf.subtables ;
  Printf.printf "\n" ;
  ()

(* Open a subtable in append mode. *)
let append_subtable table sconf =
  let subtable =
    match sconf.crypted with
    | None -> Cryptodbm.append_uncrypted_subtable ?iterations:sconf.st_iterations table ~name:sconf.name ~signwd:sconf.signed ~check_signature:(sconf.signed <> "") ()
    | Some passwd -> Cryptodbm.append_subtable ?iterations:sconf.st_iterations table ~name:sconf.name ~passwd ~signwd:sconf.signed ~check_signature:(sconf.signed <> "") ()
  in
  (sconf, subtable)

(* Execute an instruction *)
let execute state tconf = function
  | Create sconf ->
    (* Not already created. *)
    assert (not (List.mem_assq sconf state.subs)) ;

    let subtable =
      match sconf.crypted with
      | None ->
        Cryptodbm.create_uncrypted_subtable ?iterations:sconf.st_iterations state.table ~name:sconf.name ~signwd:sconf.signed ()
      | Some passwd ->
        Cryptodbm.create_subtable ?iterations:sconf.st_iterations state.table ~name:sconf.name ~passwd ~signwd:sconf.signed 
          ?max_extra_key:sconf.max_key_pad ?max_extra_data:sconf.max_data_pad ()
    in

    let content = Hashtbl.create 10 in

    state.subs <- (sconf, subtable) :: state.subs ;
    state.contents <- (sconf, content) :: state.contents ;
    ()

  | Insert (sconf, key, data) ->
    let subtable = List.assq sconf state.subs
    and content  = List.assq sconf state.contents in

    Cryptodbm.add ~may_overwrite:true subtable ~key ~data ;
    Hashtbl.replace content key data ;
    ()

  | Remove (sconf, key) ->
    let subtable = List.assq sconf state.subs
    and content  = List.assq sconf state.contents in

    Cryptodbm.delete subtable key ;

    assert (Hashtbl.mem content key) ;
    Hashtbl.remove content key ;
    ()

  (* Close the file then reopen in append mode. *) 
  | Reopen ->
    Cryptodbm.close state.table ;

    let table = 
      Cryptodbm.open_append ?iterations:tconf.iterations  ~file:(Cryptodbm.get_rootfile state.table) ~passwd:tconf.passwd ~signwd:tconf.signwd ~check_signature:(tconf.signwd <> "") ()
    in
    state.table <- table ;

    (* Reopen all subtables in append mode. *)
    let subs = List.map (fun (subconf, _) -> append_subtable table subconf) state.subs in

    state.subs <- subs ;
    ()

  (* Similarly for a subtable. *)
  | Subreopen sconf ->
    let subtable = List.assq sconf state.subs in
    Cryptodbm.close_subtable subtable ;
    let subs = List.remove_assq sconf state.subs in
    state.subs <- (append_subtable state.table sconf) :: subs ;
    ()

  | Flush -> Cryptodbm.flush ~backup:true state.table


(* Check conformity of a subtable. *)
let check_subconformity table state sconf =
  (* Open the subtable *)
  let subtable = 
    match sconf.crypted with
    | None -> Cryptodbm.open_uncrypted_subtable ?iterations:sconf.st_iterations table ~name:sconf.name ~signwd:sconf.signed ()
    | Some passwd -> Cryptodbm.open_subtable ?iterations:sconf.st_iterations table ~name:sconf.name ~passwd ~signwd:sconf.signed ()
  in

  (* Get the hashtable. *)
  let hasht = List.assq sconf state.contents in

  (* Check the content: iter over the subtable and remove each binding from the hashtable. 
   * We deliberately use iterkey + find instead of iter. *)
  Cryptodbm.iterkey subtable
    begin fun key ->
      let data = Cryptodbm.find subtable key in
      let hdata = Hashtbl.find hasht key in
      assert (data = hdata) ;
      Hashtbl.remove hasht key ;
    end ;

  (* Check that the hashtable is empty. *)
  assert (Hashtbl.length hasht = 0) ;

  Cryptodbm.close_subtable subtable ;
  ()

(* Check conformity of the whole table. *)
let check_conformity ~only_uncrypted tconf state =

  let table =
    if only_uncrypted then Cryptodbm.open_only_uncrypted ?iterations:tconf.iterations ~file:tconf.filename ~signwd:tconf.signwd ()
    else Cryptodbm.open_read ?iterations:tconf.iterations ~file:tconf.filename ~passwd:tconf.passwd ~signwd:tconf.signwd ()
  in

  (* Check all subtables. *)
  let conf_count = ref 0 in
  Common.myiter tconf.subtables 
    (fun subt -> if subt.crypted = None || not only_uncrypted then 
        begin
          check_subconformity table state subt ;
          incr conf_count ;
        end) ;

  (* Check that there is no more subtables. *)
  let count = ref 0 in
  Cryptodbm.iter_uncrypted_subtables table (fun _ _ -> incr count) ;
  if only_uncrypted then () else Cryptodbm.iter_subtables table (fun _ _ -> incr count) ;

  assert (!count = !conf_count) ;
  Cryptodbm.close table ;
  ()


(* Runs a sequence of instructions. *)
let run_sequence tconf sequence =

  (*  Printf.printf "Sequence = %s\n%!" (sequence2s sequence) ; *)

  let table = Cryptodbm.open_create ?iterations:tconf.iterations ~file:tconf.filename ~overwrite:true ~passwd:tconf.passwd ~signwd:tconf.signwd
      ~max_extra_key:tconf.max_extra_key ~max_extra_data:tconf.max_extra_data ~max_extra_bindings:tconf.max_extra_bindings
      ~perm:0o644 ()
  in

  let state = 
    { table ;
      subs = [] ;
      contents = [] }
  in

  List.iter (execute state tconf) sequence ;

  (* state.table is not necessarily == table. *)
  Cryptodbm.close state.table ;

  (* Now we check that the database is as expected. *)
  check_conformity ~only_uncrypted:true tconf state ;
  check_conformity ~only_uncrypted:false tconf state ;
  ()

open Cryptodbm_internals

let random_string gen maxlen = Utils.random_string gen (Utils.random_int gen maxlen)

(* Creates a pseudo-random parasite : key/data *)
let init_parasite gen nb = ("z" ^ random_string gen 20, random_string gen 40)

(* Computes a straight sequence corresponding to the given subtable configuration. *)
let compute_sub_sequence gen acu sconf =
  let reopens = Array.to_list (Array.make sconf.sub_append_nb (Subreopen sconf))
  and parasites = Array.to_list (Array.init sconf.parasite (init_parasite gen))
  in

  let com_parasites = List.fold_left (fun acu (key, data) -> Insert (sconf, key, data) :: Remove (sconf, key) :: acu) [] parasites 
  and seq = List.map (fun (key, data) -> Insert (sconf, key, data)) sconf.content in

  acu @ [Create sconf] @ seq @ com_parasites @ reopens

(* Indicate if two elements commutes *)
let commutes a b =
  match (a,b) with
  | (Flush, _) | (_, Flush) -> true
  | (Reopen, _) | (_, Reopen) -> true

  (* Subreopen *)
  | (Subreopen sconf1, Create sconf2) -> sconf1 != sconf2
  | (Create sconf1, Subreopen sconf2) -> sconf1 != sconf2
  | (Subreopen _, _) | (_, Subreopen _) -> true

  (* Create *)
  | (Create sconf1, ( Insert (sconf2, _, _) | Remove (sconf2, _))) -> sconf1 != sconf2

  | ((Insert (sconf1, _ ,_) | Remove (sconf1, _) | Create sconf1), Create sconf2) -> assert (sconf1 != sconf2) ; true

  (* Insert, Remove *)
  | ((Insert (sconf1, key1, _) | Remove (sconf1, key1)), 
     (Insert (sconf2, key2, _) | Remove (sconf2, key2))) -> sconf1 != sconf2 || key1 <> key2

(* Shifts to the right *)
let rec shifts nb elt = function
  | [] -> assert (nb = 0) ; [elt]
  | (x :: xs) as list ->
    if nb > 0 && commutes elt x then x :: (shifts (nb-1) elt xs)
    else elt :: list

(* Randomly shifts elements to the right *)
let random_shift gen (acu, len) elt =
  (* Number of shifts *)
  let nb = Random.State.int gen (len+1) in
  (shifts nb elt acu, len + 1)

(* Shuffle it (in a deterministic way). *)
let shuffle gen seq =

  let source = List.rev seq in

  (* Take each element in source, move it randomly to the right. 
   * Elements with dependencies cannot cross each other. *)
  let (result, _) = List.fold_left (random_shift gen) ([], 0) source in
  result

(* Computes a suitable sequence corresponding to the given test. *)
let compute_sequence tconf =

  let gen = Random.State.make [| Hashtbl.hash tconf |] in

  (* Builds an ordered sequence. *)
  let seq_subtables = List.fold_left (compute_sub_sequence gen) [] tconf.subtables
  and flushes = Array.to_list (Array.make tconf.flush_nb Flush)
  and reopens = Array.to_list (Array.make tconf.flush_nb Reopen) in

  let ordered = seq_subtables @ flushes @ reopens in

  shuffle gen ordered


(* Runs a test configuration. *)
let run tconf = run_sequence tconf (compute_sequence tconf)

