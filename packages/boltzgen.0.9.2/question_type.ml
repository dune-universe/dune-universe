type qcm = A | B | C | D

let string_of_qcm = function A -> "A" | B -> "B" | C -> "C" | D -> "D"

let qcm_of_string = function
  | "A" -> A
  | "B" -> B
  | "C" -> C
  | "D" -> D
  | _ -> failwith "bad value"

let sep_char s sep =
  let l = ref [ s ] in
  for i = 0 to String.length sep - 1 do
    l :=
      !l
      |> List.map (String.split_on_char sep.[i])
      |> List.flatten |> List.map String.trim
      |> List.filter (fun x -> x <> "")
  done;
  List.map int_of_string !l

(*let l = sep_char "[2;1 ; 5]" ";[]";;*)

type qlist = int list

let qlist_of_string s = sep_char s ";[]"

let string_of_qlist s =
  List.fold_left
    (fun a b -> (if a <> "[" then a ^ "; " else "[") ^ string_of_int b)
    "[" s
  ^ "]"

let canonize_qlist l =
  List.fold_right
    (fun v acc ->
      match acc with
      | [] -> [ v ]
      | t :: q when t = v -> t :: q
      | t :: q -> v :: t :: q)
    (List.sort compare l) []

let iter_hash x y = Hashtbl.hash (x, y)

let hash_string s = Hashtbl.hash s

type question_value = {
  text : string;
  proposed_answer : string array;
  rtype : Type.compo_type;
  answer : string;
  test_effort : int;
  test_max : int;
}

type question_type = { text : string; answer : string }

type question = Value of question_value | Type of question_type

type question_bank = {
  questions : question array;
  digest : Digest.t;
  shuffle : string option;
}

let mkqcm text vec answer =
  match vec with
  | _ :: _ ->
      Value
        {
          text;
          proposed_answer = Array.of_list vec;
          rtype = Type.Name ("qcm", []);
          answer = string_of_qcm answer;
          test_effort = 1;
          test_max = 10;
        }
  | _ -> failwith "ill-formed question"

let mkfun ?(test_effort = 20) ?(test_max = 20) text rtype answer al =
  let _, f = Parse_from_compiler.parse_string ("a:" ^ rtype) in
  Value
    {
      text;
      proposed_answer = Array.of_list al;
      rtype = Type.ano_func f;
      answer;
      test_effort;
      test_max;
    }

let mkdeftype text answer = Type { text; answer }

let int_of_qcm = function A -> 0 | B -> 1 | C -> 2 | D -> 3

let qcm_of_int x = match x mod 4 with 0 -> A | 1 -> B | 2 -> C | _ -> D

(*let is_qcm = function
    Type.Name ("qcm",[]) -> true
  | _ -> false*)

let swap tab i j =
  let t = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <- t

let rec perm_of_hash_aux hash tab i =
  if i > 0 then (
    swap tab (hash mod (i + 1)) i;
    perm_of_hash_aux (hash / (i + 1)) tab (i - 1) )

let perm_of_hash hash n =
  let tab = Array.init n (fun i -> i) in
  perm_of_hash_aux hash tab (n - 1);
  tab

let inv_perm perm =
  let tab = Array.make (Array.length perm) 0 in
  Array.iteri (fun i j -> tab.(j) <- i) perm;
  tab

let perm_of_qcm perm a =
  let i = int_of_qcm a in
  qcm_of_int perm.(i)

let shuffleq hash qv =
  match qv with
  | Value q ->
      let n = Array.length q.proposed_answer in
      let perm = perm_of_hash hash n in
      let invperm = inv_perm perm in
      let nvec = Array.init n (fun i -> q.proposed_answer.(perm.(i))) in
      let answer =
        match q.rtype with
        | Type.Name ("qcm", []) ->
            string_of_qcm @@ perm_of_qcm invperm (qcm_of_string q.answer)
        | Type.Name ("qlist", []) ->
            q.answer |> qlist_of_string
            |> List.map (fun i -> invperm.(i - 1) + 1)
            |> string_of_qlist
        | _ -> q.answer
      in
      Value { q with proposed_answer = nvec; answer }
  | _ -> qv

let shuffle login qbank =
  assert (qbank.shuffle = None);
  let hash = hash_string login in
  let n = Array.length qbank.questions in
  let tabo = Array.init n (fun i -> (i, iter_hash hash i)) in
  Array.sort (fun (_, x) (_, y) -> compare x y) tabo;
  let tab2 =
    Array.init n (fun i -> shuffleq hash qbank.questions.(fst tabo.(i)))
  in
  { qbank with questions = tab2; shuffle = Some login }

let save_qbank name qbank =
  let f = open_out name in
  output_value f qbank;
  close_out f

let load_qbank name =
  let f = open_in name in
  let q = (input_value f : question_bank) in
  close_in f;
  q
