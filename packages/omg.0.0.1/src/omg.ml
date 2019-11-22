let _ = Random.self_init ()

type generator =
  { forward_cache: (string * string, (string, int) Hashtbl.t) Hashtbl.t
  ; backward_cache: (string * string, (string, int) Hashtbl.t) Hashtbl.t
  ; words: (int, string) Hashtbl.t
  ; last_two_words: (string * string) ref }

let add_word generator w =
  generator.last_two_words := (snd !(generator.last_two_words), w) ;
  let n = Hashtbl.length generator.words in
  Hashtbl.add generator.words n w

let get_last_two_words generator =
  match !(generator.last_two_words) with
  | "", "" ->
      []
  | x, "" | "", x ->
      [x]
  | x, y ->
      [x; y]

let init () =
  let gen =
    { forward_cache= Hashtbl.create 4096
    ; backward_cache= Hashtbl.create 4096
    ; words= Hashtbl.create 4096
    ; last_two_words= ref ("", "") }
  in
  add_word gen "\n" ; gen

let triples = function
  | w1 :: w2 :: s ->
      let _, _, acc =
        List.fold_left
          (fun (w1, w2, acc) el -> (w2, el, (w1, w2, el) :: acc))
          (w1, w2, []) s
      in
      List.rev acc
  | _ ->
      []

let rec add_key cache k v =
  match Hashtbl.find cache k with
  | exception Not_found ->
      Hashtbl.add cache k (Hashtbl.create 64) ;
      add_key cache k v
  | tbl -> (
    match Hashtbl.find tbl v with
    | exception Not_found ->
        Hashtbl.add tbl v 0
    | n ->
        Hashtbl.replace tbl v (n + 1) )

let feed generator msg =
  let splitted = String.split_on_char ' ' msg in
  let splitted = splitted @ ["\n"] in
  let triples = triples (get_last_two_words generator @ splitted) in
  List.iter
    (fun (w1, w2, w3) -> add_key generator.forward_cache (w1, w2) w3)
    triples ;
  List.iter
    (fun (w1, w2, w3) -> add_key generator.backward_cache (w3, w2) w1)
    triples ;
  List.iter (fun el -> add_word generator el) splitted

let select_seed generator seed_word backward =
  let dir = if backward then -1 else 1 in
  match seed_word with
  | None ->
      let seed_word = ref "\n" in
      let next_word = ref "\n" in
      while !seed_word = "\n" || !next_word = "\n" do
        let seed = 1 + Random.int (Hashtbl.length generator.words - 2) in
        seed_word := Hashtbl.find generator.words seed ;
        next_word := Hashtbl.find generator.words (seed + dir)
      done ;
      (!seed_word, !next_word)
  | Some w ->
      let possible_indexes =
        Hashtbl.fold
          (fun k v acc -> if v = w then k :: acc else acc)
          generator.words []
      in
      if possible_indexes = [] then failwith "select_seed" ;
      let index = Random.int (List.length possible_indexes) in
      let index = List.nth possible_indexes index in
      (w, Hashtbl.find generator.words (index + dir))

let generate_markov_text generator max_size seed backward =
  let seed_word, next_word =
    match seed with
    | None, None ->
        select_seed generator None backward
    | Some x, None | None, Some x ->
        select_seed generator (Some x) backward
    | Some x, Some y ->
        (x, y)
  in
  let cache =
    if backward then generator.backward_cache else generator.forward_cache
  in
  let w1, w2 =
    if Random.int 3 = 0 && Hashtbl.mem cache ("\n", seed_word) then
      ("\n", seed_word)
    else (seed_word, next_word)
  in
  let w1 = ref w1 in
  let w2 = ref w2 in
  let gen_words = ref [] in
  let exception Stop in
  ( try
      for _ = 0 to max_size do
        gen_words := !w1 :: !gen_words ;
        let tbl =
          match Hashtbl.find cache (!w1, !w2) with
          | exception Not_found ->
              raise Stop
          | tbl ->
              tbl
        in
        let cache_n = Hashtbl.fold (fun _ v acc -> acc + v) tbl 0 in
        let i = if cache_n = 0 then 0 else Random.int cache_n in
        let exception Found of string in
        let new_word =
          match
            Hashtbl.fold
              (fun k v acc ->
                let acc = acc + v in
                if i <= acc then raise (Found k) else acc)
              tbl 0
          with
          | exception Found s ->
              s
          | _ ->
              raise Stop
        in
        w1 := !w2 ;
        w2 := new_word
      done
    with Stop -> () ) ;
  if not (!w2 = "\n") then gen_words := !w2 :: !gen_words ;
  let gen_words = !gen_words in
  let gen_words = List.filter (fun el -> not (el = "\n")) gen_words in
  let buff = Buffer.create 512 in
  ( match if not backward then List.rev gen_words else gen_words with
  | [x] ->
      Buffer.add_string buff x
  | x :: s ->
      Buffer.add_string buff x ;
      List.iter (fun el -> Buffer.add_string buff (" " ^ el)) s
  | [] ->
      () ) ;
  Buffer.contents buff
