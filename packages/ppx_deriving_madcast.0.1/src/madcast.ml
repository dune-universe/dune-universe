
open Parsetree
open Asttypes
open Longident
open Ast_helper
open Location

let mkpatvar i = Pat.var (mknoloc ("c"^(string_of_int i)))
let mkident i = Exp.ident (mknoloc (Lident ("c"^(string_of_int i))))

(* ============================= [ Base types ] ============================= *)

let () =
  [ ( "bool -> float",
      [%type: bool], [%type: float],
      [%expr function
          | false -> 0.
          | true -> 1.] );

    ( "bool -> int",
      [%type: bool], [%type: int],
      [%expr function
          | false -> 0
          | true -> 1] );

    ( "bool -> string",
      [%type: bool], [%type: string],
      [%expr string_of_bool] );

    ( "char -> int",
      [%type: char], [%type: int],
      [%expr int_of_char] );

    ( "char -> string" ,
      [%type: char], [%type: string],
      [%expr String.make 1] );

    ( "float -> string",
      [%type: float], [%type: string],
      [%expr string_of_float] );

    ( "int -> bool",
      [%type: int], [%type: bool],
      [%expr function
          | 0 -> false
          | 1 -> true
          | _ -> failwith "madcast: int -> bool"] );

    ( "int -> char",
      [%type: int], [%type: char],
      [%expr fun i ->
          try
            char_of_int i
          with
            Failure _ -> failwith "madcast: int -> char"] );

    ( "int -> float",
      [%type: int], [%type: float],
      [%expr float_of_int] );

    ( "int -> string",
      [%type: int], [%type: string],
      [%expr string_of_int] );

    ( "string -> bool",
      [%type: string], [%type: bool],
      [%expr fun s ->
          try
            bool_of_string s
          with
            Failure _ -> failwith "madcast: string -> bool"] );

    ( "string -> char",
      [%type: string], [%type: char],
      [%expr fun s ->
          if String.length s = 1 then
            s.[0]
          else
            failwith "madcast: string -> char"] );

    ( "string -> float",
      [%type: string], [%type: float],
      [%expr fun s ->
          try
            float_of_string s
          with
            Failure _ -> failwith "madcast: string -> float"] );

    ( "string -> int",
      [%type: string], [%type: int],
      [%expr fun s ->
          try
            int_of_string s
          with
            Failure _ -> failwith "madcast: string -> int"] ) ]
  |>
    List.iter
      (fun (name, itype, otype, expr) ->
        let matcher (itype', otype') =
          if Parsetree_utils.equal_core_type itype itype'
             && Parsetree_utils.equal_core_type otype otype'
          then Some []
          else None
        in
        let builder casts =
          assert (casts = []);
          expr
        in
        RuleSet.register (Rule.make ~name ~matcher ~builder ()))

(* ============================== [ Options ] =============================== *)

let () =
  let name = "'a option -> 'b option" in
  let matcher = function
    | [%type: [%t? itype] option], [%type: [%t? otype] option] ->
       Some [itype, otype]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr function
        | None -> None
        | Some x -> Some ([%e List.hd casts] x)]
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

let () =
  let name = "'a -> 'b option" in
  let matcher = function
    | itype, [%type: [%t? otype] option] ->
       Some [itype, otype]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr fun x -> Some ([%e List.hd casts] x)]
  in
  RuleSet.(register
    ~applies_after:[lookup "'a option -> 'b option"]
    (Rule.make ~name ~matcher ~builder ()))

let () =
  let name = "'a option -> 'b" in
  let matcher = function
    | [%type: [%t? itype] option], otype ->
       Some [itype, otype]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr function
        | None -> failwith "madcast: 'a option -> 'b"
        | Some x -> [%e List.hd casts] x]
  in
  RuleSet.(register
    ~applies_after:[lookup "'a option -> 'b option"]
    (Rule.make ~name ~matcher ~builder ()))

(* =============================== [ Arrays ] =============================== *)

let () =
  let name = "'a array -> 'b array" in
  let matcher = function
    | [%type: [%t? itype] array], [%type: [%t? otype] array] ->
       Some [itype, otype]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr Array.map [%e List.hd casts]]
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

let () =
  let name = "'a -> 'b array" in
  let matcher = function
    | itype, [%type: [%t? otype] array] ->
       Some [itype, otype]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr fun x -> [|[%e List.hd casts] x|]]
  in
  RuleSet.(register
    ~applies_after:[lookup "'a array -> 'b array"]
    (Rule.make ~name ~matcher ~builder ()))

let () =
  let name = "'a array -> 'b" in
  let matcher = function
    | [%type: [%t? itype] array], otype ->
       Some [itype, otype]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr fun a ->
        if Array.length a = 1 then
          [%e List.hd casts] a.(0)
        else
          failwith "madcast: 'a array -> 'b"]
  in
  RuleSet.(register
    ~applies_after:[lookup "'a array -> 'b array"; lookup "'a -> 'b array"]
    (Rule.make ~name ~matcher ~builder ()))

let () =
  let name = "<tuple> -> 'b array" in
  let matcher = function
    | {ptyp_desc=Ptyp_tuple itypes}, [%type: [%t? otype] array] ->
       Some (List.map (fun itype -> (itype, otype)) itypes)
    | _ -> None
  in
  let builder casts =
    (* fun (c0,...ck) -> [|cast0 c0; ... castk ck|] *)
    Exp.fun_
      Nolabel None
      (Pat.tuple (List.mapi (fun i _ -> mkpatvar i) casts))
      (Exp.array (List.mapi (fun i cast -> Exp.apply cast [Nolabel, mkident i]) casts))
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

let () =
  let name = "'a array -> <tuple>" in
  let matcher = function
    | [%type: [%t? itype] array], {ptyp_desc=Ptyp_tuple otypes} ->
       Some (List.map (fun otype -> (itype, otype)) otypes)
    | _ -> None
  in
  let builder casts =
    (* function
       | [|c0;...ck|] -> (cast0 c0, ... castk ck)
       | _ -> failwith ... *)
    Exp.function_
      [ Exp.case
          (Pat.array (List.mapi (fun i _ -> mkpatvar i) casts))
          (Exp.tuple (List.mapi (fun i cast -> Exp.apply cast [Nolabel, mkident i]) casts)) ;
        Exp.case
          (Pat.any ())
          [%expr failwith "madcast: 'a array -> <tuple>"] ]
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

let () =
  let name = "<tuple> array -> 'a array" in
  let matcher = function
    | [%type: [%t? {ptyp_desc=Ptyp_tuple itypes}] array], [%type: [%t? otype] array] ->
       Some [Typ.tuple itypes, [%type: [%t otype] array]]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr fun a ->
        Array.map [%e List.hd casts] a
        |> Array.to_list
        |> Array.concat]
  in
  RuleSet.(register
    ~applies_before:[lookup "'a -> 'b array"; lookup "'a array -> 'b"]
    (Rule.make ~name ~matcher ~builder ()))

let () =
  let name = "'a array -> <tuple> array" in
  let matcher = function
    | [%type: [%t? itype] array], [%type: [%t? {ptyp_desc=Ptyp_tuple otypes}] array] ->
       Some (List.map (fun otype -> (itype, otype)) otypes)
    | _ -> None
  in
  let builder casts =
    let l = List.length casts in
    let exp_int n = Exp.constant (Const.int n) in
    [%expr fun a ->
        if Array.length a mod [%e exp_int l] <> 0 then
          failwith "madcast: 'a array -> <tuple> array"
        else
          Array.init (Array.length a / [%e exp_int l])
            (fun i ->
              [%e Exp.tuple
                  (List.mapi
                     (fun j cast ->
                       [%expr [%e cast] a.([%e exp_int j] + i * [%e exp_int l])])
                     casts)])]
  in
  RuleSet.(register
    ~applies_before:[lookup "'a -> 'b array"; lookup "'a array -> 'b"]
    ~applies_after:[lookup "<tuple> array -> 'a array"]
    (Rule.make ~name ~matcher ~builder ()))

(* =============================== [ Lists ] ================================ *)
(* using the rules for arrays *)

let () =
  let name = "'a list -> 'a array -> 'b" in
  let matcher = function
    | [%type: [%t? itype] list], otype ->
       Some [[%type: [%t itype] array], otype]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr fun l -> Array.of_list l |> [%e List.hd casts]]
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

let () =
  let name = "'a -> 'b array -> 'b list" in
  let matcher = function
    | itype, [%type: [%t? otype] list] ->
       Some [itype, [%type: [%t otype] array]]
    | _ -> None
  in
  let builder casts =
    assert (List.length casts = 1);
    [%expr fun x -> [%e List.hd casts] x |> Array.to_list]
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

(* =============================== [ Tuples ] =============================== *)

let () =
  let name = "<tuple> -> <tuple>" in
  let matcher = function
    | {ptyp_desc=Ptyp_tuple itypes} , {ptyp_desc=Ptyp_tuple otypes}
         when List.length itypes = List.length otypes ->
       Some (List.combine itypes otypes)
    | _ -> None
  in
  let builder casts =
    (* fun (c0,...ck) -> (cast0 c0, ... castk ck) *)
    Exp.fun_
      Nolabel None
      (Pat.tuple (List.mapi (fun i _ -> mkpatvar i) casts))
      (Exp.tuple (List.mapi (fun i cast -> Exp.apply cast [Nolabel, mkident i]) casts))
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

(* ============================= [ Functions ] ============================== *)

let () =
  let name = "('a -> 'b) -> ('c -> 'd)" in
  let matcher = function
    | [%type: [%t? iitype] -> [%t? iotype]], [%type: [%t? oitype] -> [%t? ootype]] ->
       Some [(oitype, iitype); (iotype, ootype)]
    | _ -> None
  in
  let builder = function
    | [icast; ocast] ->
       [%expr fun f x -> x |> [%e icast] |> f |> [%e ocast]]
    | _ -> assert false
  in
  RuleSet.register (Rule.make ~name ~matcher ~builder ())

let () =
  let name = "currying" in
  let matcher (itype, otype) =
    match itype with
    | [%type: [%t? {ptyp_desc=Ptyp_tuple iitypes}] -> [%t? iotype]] ->
       (
         let rec matcher = function
           | ([], ootype) -> [(iotype, ootype)] (* this is the right order *)
           | (iitype :: iitypes, [%type: [%t? oitype] -> [%t? ootype]]) ->
              (oitype, iitype) :: matcher (iitypes, ootype)
           | _ -> failwith "matcher"
         in
         try Some (matcher (iitypes, otype))
         with Failure _ -> None
       )
    | _ -> None
  in
  let builder casts =
    let ocast = ExtList.ft casts in
    let icasts = ExtList.bd casts in
    [%expr fun f ->
        [%e ExtList.foldi_right
              (* imbricated functions *)
              (fun i _ exp ->
                Exp.fun_ Nolabel None (mkpatvar i) exp)
              icasts
              (
                (* the body of the function *)
                Exp.apply ocast [Nolabel,
                  Exp.apply [%expr f] [Nolabel,
                    Exp.tuple (
                      List.mapi
                        (fun i icast ->
                          Exp.apply icast [Nolabel, mkident i])
                        icasts
                  )]]
              )]]
  in
  RuleSet.register
    ~applies_after:[RuleSet.lookup "('a -> 'b) -> ('c -> 'd)"]
    (Rule.make ~name ~matcher ~builder ())

let () =
  let name = "uncurrying" in
  let matcher (itype, otype) =
    match otype with
    | [%type: [%t? {ptyp_desc=Ptyp_tuple oitypes}] -> [%t? ootype]] ->
       (
         let rec matcher = function
           | (iotype, []) -> [(iotype, ootype)] (* this is the right order *)
           | ([%type: [%t? iitype] -> [%t? iotype]], oitype :: ootypes) ->
              (oitype, iitype) :: matcher (iotype, ootypes)
           | _ -> failwith "matcher"
         in
         try Some (matcher (itype, oitypes))
         with Failure _ -> None
       )
    | _ -> None
  in
  let builder casts =
    let ocast = ExtList.ft casts in
    let icasts = ExtList.bd casts in
    [%expr fun f ->
        [%e Exp.fun_ Nolabel None
              (Pat.tuple (List.mapi (fun i _ -> mkpatvar i) icasts))
              (Exp.apply ocast [Nolabel, (Exp.apply [%expr f]
                 (List.mapi (fun i icast -> (Nolabel, Exp.apply icast [Nolabel, mkident i])) icasts))])]]
  in
  RuleSet.register
    ~applies_after:[RuleSet.lookup "('a -> 'b) -> ('c -> 'd)"]
    (Rule.make ~name ~matcher ~builder ())





(* ======================= [ And now, the main loop ] ======================= *)

let rec reverse_possibles = function
  (* changes a list of possibilities in possibilities of lists *)
  | [] -> [[]]
  | possible_heads :: tail_of_possibles ->
     List.map
       (fun possible_tail ->
         List.map
           (fun possible_head ->
             possible_head :: possible_tail)
           possible_heads)
       (reverse_possibles tail_of_possibles)
     |> List.flatten

let rec derive (itype, otype) : Parsetree.expression list =
  RuleSet.fold_by_priority
    (fun rules -> function
      | [] ->
         (* Empty means that the stronger priorities have found
            nothing. We go through all the rules at our priority,
            apply them and see which ones did succeed. *)
         List.fold_left
           (fun casts rule ->
             match Rule.match_ rule (itype, otype) with
             | None -> (* the rule found nothing *) casts
             | Some premises ->
                (
                  List.map derive premises
                  |> reverse_possibles
                  |> List.map
                       (fun premises ->
                         Rule.build_ rule premises)
                ) @ casts)
           []
           rules
      | _ as casts ->
         (* Non-empty means that the previous priorities have found
            something already, so we let that and do nothing. *)
         casts)
    []

let derive itype otype =
  (* We ask derive to derive expressions for itype -> otype. We then
     annotate them with that type where type variables are universally
     quantified. Since this can syntactically only happen in a let, we
     return something like:

         let cast : [vars]. [itype -> otype] = [expr] in cast
   *)
  let t = Parsetree_utils.universal_closure_of_core_type [%type: [%t itype] -> [%t otype]] in
  derive (itype, otype)
  |> List.map (fun expr -> [%expr let (cast : [%t t]) = [%e expr] in cast])
