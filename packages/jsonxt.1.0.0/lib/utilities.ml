let to_basic json : Basic.json =
  let rec map node =
    match node with
    | `Null -> `Null
    | `Bool _ as v -> v
    | `Int _ as v -> v
    | `Intlit v -> `Int (int_of_string v)
    | `Float _ as v -> v
    | `Floatlit v -> `Float (float_of_string v)
    | `String _ as v -> v
    | `Stringlit s -> begin
      match String.length s with
      | 0 | 1 -> `String s         (* malformed, should have double-quotes at start and end *)
      | _ -> `String (String.sub s 1 (String.length s - 2))
      end
    | `List l -> `List (List.map map l)
    | `Assoc a -> `Assoc (List.map (fun (id, v) -> (id, map v)) a)
    | `Tuple tpl -> `List (List.map map tpl)
    | `Variant (name, jopt) ->
       match jopt with 
       | None -> `String name
       | Some v -> `List [ `String name; (map v) ]
  in
  map json

let to_strict json : Strict.json =
  let rec map node =
    match node with
    | `Null -> `Null
    | `Bool _ as v -> v
    | `Int v -> `Float (float_of_int v)
    | `Intlit v -> `Float (float_of_string v)
    | `Float _ as v -> v
    | `Floatlit v -> `Float (float_of_string v)
    | `String _ as v -> v
    | `Stringlit s -> begin
      match String.length s with
      | 0 | 1 -> `String s         (* malformed, should have double-quotes at start and end *)
      | _ -> `String (String.sub s 1 (String.length s - 2))
      end
    | `List l -> `List (List.map map l)
    | `Assoc a -> `Assoc (List.map (fun (id, v) -> (id, map v)) a)
    | `Tuple tpl -> `List (List.map map tpl)
    | `Variant (name, jopt) ->
       match jopt with 
       | None -> `String name
       | Some v -> `List [ `String name; (map v) ]
  in
  map json

let json_to_string_repr json = 
  let buf = Buffer.create 100 in
  let add_char = Buffer.add_char buf in
  let add_string = Buffer.add_string buf in
  let add_quote_string s = add_char '"'; Utils.escape ~add_char ~add_string s; add_char '"' in
  let add_int i = add_string (string_of_int i) in
  let add_float f = add_string (Json_float.string_of_float_json f) in
  let rec fmt ldr value =
    match value with
    | `Assoc o ->
      add_string "`Assoc [\n";
      begin match o with | [] -> () | _ -> json_assoc (ldr ^ "  ") o; add_char '\n' end;
      add_string ldr; add_char ']'
    | `List l ->
      add_string "`List [\n";
      begin match l with | [] -> () | _ -> json_list (ldr ^ "  ") l; add_char '\n' end;
      add_string ldr; add_char ']'
    | `Null -> add_string "`Null"
    | `Bool b -> add_string "`Bool "; add_string (string_of_bool b)
    | `Int i -> add_string "`Int "; add_int i
    | `Intlit s -> add_string "`Intlit "; add_string s
    | `Float f -> add_string "`Float "; add_float f
    | `Floatlit s -> add_string "`Floatlit "; add_string s
    | `String s -> add_string "`String "; add_quote_string s
    | `Stringlit s -> add_string "`Stringlit "; add_string s
    | `Tuple t ->
      add_string "`Tuple (\n"; json_list (ldr ^ "  ") t;
      add_char '\n'; add_string ldr; add_char ')'
    | `Variant v ->
      add_string "`Variant <";  variant (ldr ^ "  ") v;
      add_char '\n'; add_string ldr; add_char '>'
  and json_assoc ldr o =
    let sep = ref ldr in
    let newsep = ",\n" ^ ldr in
    List.iter (fun v -> add_string !sep; sep := newsep; pair ldr v ) o
  and pair ldr (k, v) = add_quote_string k; add_string ": "; fmt ldr v
  and json_list ldr l =
    let sep = ref ldr in
    let newsep = ",\n" ^ ldr in
    List.iter (fun v -> add_string !sep; sep := newsep; fmt ldr  v ) l
  and variant ldr (k, j) =
    add_quote_string k;
    match j with
    | Some j -> add_string ": "; fmt (ldr ^ "  ") j
    | None -> ()
  in
  fmt "" json;
  Buffer.contents buf

let json_stream_to_string_repr = function
  | `Null -> "`Null"
  | `Bool b -> "`Bool:" ^ (if b then "true" else "false")
  | `Int i -> "`Int:" ^ (string_of_int i)
  | `Intlit is -> "`Intlit:" ^  is
  | `Float f -> "`Float:" ^ (string_of_float f)
  | `Floatlit fs -> "`Floatlit:" ^ fs
  | `String s -> "`String:\"" ^ s ^ "\""
  | `Stringlit s -> "`Stringlit:" ^ s
  | `As -> "`As"
  | `Ae -> "`Ae"
  | `Os -> "`Os"
  | `Oe -> "`Oe"
  | `Ts -> "`Ts"
  | `Te -> "`Te"
  | `Vs -> "`Vs"
  | `Ve -> "`Ve"
  | `Name n -> "`Name:" ^ n
  | `Infinity -> "`Infinity"
  | `Neg_infinity -> "`Neg_infinity"
  | `Nan -> "`Nan"
  

let json_to_string json = 
  let buf = Buffer.create 100 in
  let add_char = Buffer.add_char buf in
  let add_string = Buffer.add_string buf in
  let add_quote_string s = add_char '"'; Utils.escape ~add_char ~add_string s; add_char '"' in
  let add_int i = add_string (string_of_int i) in
  let add_float f = add_string (Json_float.string_of_float_json f) in
  let rec fmt value =
    match value with
    | `Assoc o -> add_char '{'; json_assoc o; add_char '}'
    | `List l -> add_char '['; json_list l; add_char ']'
    | `Null -> add_string "null"
    | `Bool b -> add_string (string_of_bool b)
    | `Int i -> add_int i
    | `Intlit s -> add_string s
    | `Float f -> add_float f
    | `Floatlit s -> add_string s
    | `String s -> add_quote_string s
    | `Stringlit s -> add_string s
    | `Tuple t -> add_char '('; json_list t; add_char ')'
    | `Variant v -> add_char '<';  variant v; add_char '>'
  and json_assoc o =
    let sep = ref "" in
    let newsep = "," in
    List.iter (fun v -> add_string !sep; sep := newsep; pair v) o
  and pair (k, v) = add_quote_string k; add_string ":"; fmt v
  and json_list l =
    let sep = ref "" in
    let newsep = "," in
    List.iter (fun v -> add_string !sep; sep := newsep; fmt v ) l
  and variant (k, j) =
    add_quote_string k;
    match j with
    | Some j -> add_string ":"; fmt j
    | None -> ()
  in
  fmt json;
  Buffer.contents buf

let rec equal json1 json2 =
  match json1, json2 with
  | `Null, `Null -> true
  | `Bool b1, `Bool b2 -> b1 = b2
  | `Int i1, `Int i2 -> i1 = i2
  | `Intlit s1, `Intlit s2 -> String.equal s1 s2
  | `Float f1, `Float f2 -> f1 = f2
  | `Floatlit s1, `Floatlit s2 -> String.equal s1 s2
  | `String s1, `String s2 -> String.equal s1 s2
  | `Stringlit s1, `Stringlit s2 -> String.equal s1 s2
  | `Assoc o1, `Assoc o2 -> begin
      let cmpk (k1, _) (k2, _) = String.compare k1 k2 in
      let cmpkv (k1, v1) (k2, v2) =
        match String.compare k1 k2 with
        | 0 -> equal v1 v2
        | _ -> false
      in
      let o1 = List.stable_sort cmpk o1 in
      let o2 = List.stable_sort cmpk o2 in
      match List.for_all2 cmpkv o1 o2 with
      | res -> res
      | exception Invalid_argument _ -> false (* different lengths *)
    end
  | `Tuple l1, `Tuple l2
  | `List l1, `List l2 -> begin
      match List.for_all2 equal l1 l2 with
      | res -> res
      | exception Invalid_argument _ -> false (* different lengths *)
    end
  | `Variant (n1, v1), `Variant (n2, v2) -> begin
      match String.compare n1 n2 with
      | 0 -> begin
        (* match (v1:'a Json_internal.constrained option), (v2:'a option) with *)
        match v1, v2 with
        | Some v1, Some v2 -> equal v1 v2
        | None, None -> true
        | _ -> false
      end
      | _ -> false
    end
  | (_:'a Json_internal.constrained), (_:'a Json_internal.constrained) -> false
