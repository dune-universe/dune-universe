open Smtlib_typed_env
open Smtlib_ty
open Options

type th_def = {
  sorts :
    (string *
     ((int * int) *
      (string -> (ty list * int list)  -> desc))) list;
  funs :
    (string * fun_def) list;
  par_funs :
    (string *
     ((string list) -> fun_def)) list;
}

type theory =
  | Core
  | Ints
  | Reals
  | Reals_Ints
  | FloatingPoint
  | Arrays
  | BitVectors

let new_fun params return assoc =
  {params =
     Smtlib_ty.new_type (Smtlib_ty.TFun (params,return));
   assoc}

let core = {
  sorts = [ "Bool",((0,0),(fun _s (l1,l2) ->
      assert (l1 == [] && l2 == []); TBool))];
  funs = [
    "true", new_fun [] (new_type TBool) None;
    "false", new_fun [] (new_type TBool) None;
    "not", new_fun
      [(new_type TBool)]
      (new_type TBool) None;
    "=>", new_fun
      [(new_type TBool);
       (new_type TBool)]
      (new_type TBool) (Some Right);
    "and", new_fun
      [(new_type TBool);
       (new_type TBool)]
      (new_type TBool) (Some Left);
    "or", new_fun
      [(new_type TBool);
       (new_type TBool)]
      (new_type TBool) (Some Left);
    "xor", new_fun
      [(new_type TBool);
       (new_type TBool)]
      (new_type TBool) (Some Left);
    (let a = new_type(TVar("A")) in
     "=", new_fun [a;a]
       (new_type TBool) (Some Chainable));
    (let a = new_type(TVar("A")) in
     "distinct", new_fun [a;a]
       (new_type TBool) (Some Pairwise));
    (let a = new_type(TVar("A")) in
     "ite", new_fun
       [(new_type TBool); a; a] a None);
  ];
  par_funs = []
}

let ints = {
  sorts = ["Int",((0,0),(fun _s (l1,l2) ->
      assert (l1 == [] && l2 == []); TInt))];
  funs = [
    "-", new_fun
      [(new_type TInt)]
      (new_type TInt) None;
    "-", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TInt) (Some Left);
    "+", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TInt) (Some Left);
    "*", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TInt) (Some Left);
    "div", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TInt) (Some Left);
    "mod", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TInt) None;
    "abs", new_fun
      [(new_type TInt)]
      (new_type TInt) None;
    "<=", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TBool) (Some Chainable);
    "<", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TBool) (Some Chainable);
    ">=", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TBool) (Some Chainable);
    ">", new_fun
      [(new_type TInt);
       (new_type TInt)]
      (new_type TBool) (Some Chainable);
  ];
  par_funs = []

}

let reals = {
  sorts = ["Real",((0,0),(fun _s (l1,l2) ->
      assert (l1 == [] && l2 == []); TReal))];
  funs = [
    "-", new_fun
      [(new_type TReal)]
      (new_type TReal) None;
    "-", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TReal) (Some Left);
    "+", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TReal) (Some Left);
    "*", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TReal) (Some Left);
    "/", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TReal) (Some Left);
    "<=", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TBool) (Some Chainable);
    "<", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TBool) (Some Chainable);
    ">=", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TBool) (Some Chainable);
    ">", new_fun
      [(new_type TReal);
       (new_type TReal)]
      (new_type TBool) (Some Chainable);
  ];
  par_funs = []
}
let reals_ints = {
  sorts = List.rev_append ints.sorts reals.sorts;
  funs = List.rev_append (List.rev_append ints.funs reals.funs) [
      "to_real", new_fun
        [(new_type TInt)]
        (new_type TReal) None;
      "to_int", new_fun
        [(new_type TReal)]
        (new_type TInt) None;
      "is_int", new_fun
        [(new_type TReal)]
        (new_type TBool) None;
    ];
  par_funs = []
}

let arrays =
  {
    sorts = ["Array",((2,0),
                      (fun _s (l1,l2) ->
                         let t1,t2 = List.hd l1, List.hd (List.tl l1) in
                         assert (List.length l1 = 2 && l2 == []);
                         TArray (t1,t2)))];
    funs = [
      (let x = new_type(TVar("X")) in
       let y = new_type(TVar("Y")) in
       "select", new_fun
         [new_type (TArray (x,y));x] y None);
      (let x = new_type(TVar("X")) in
       let y = new_type(TVar("Y")) in
       "store", new_fun
         [new_type (TArray (x,y));x;y]
         (new_type (TArray (x,y))) None);
    ];
    par_funs = []
  }


let floating_point = {
  sorts = [
    "RoundingMode",((0,0), (fun _s (l1,l2) ->
        assert (l1 == [] && l2 == []); TRoundingMode));
    "FloatingPoint",((0,2),
                     (fun _s (l1,l2) ->
                        match l1,l2 with
                        | [], [n1;n2] -> TFloatingPoint(n1,n2)
                        | _, _ -> assert false
                     ));
    "Float16",((0,0), (fun _s (l1,l2) ->
        assert (l1 == [] && l2 == []); TFloatingPoint(5,11)));
    "Float32",((0,0), (fun _s (l1,l2) ->
        assert (l1 == [] && l2 == []); TFloatingPoint(8,24)));
    "Float64",((0,0), (fun _s (l1,l2) ->
        assert (l1 == [] && l2 == []); TFloatingPoint(11,53)));
    "Float128",((0,0), (fun _s (l1,l2) ->
        assert (l1 == [] && l2 == []); TFloatingPoint(15,113)));

  ];
  funs = [
    "roundNearestTiesToEven", new_fun
      [] (new_type TRoundingMode) None;
    "RNE", new_fun
      [] (new_type TRoundingMode) None;
    "roundNearestTiesToAway", new_fun
      [] (new_type TRoundingMode) None;
    "RNA", new_fun
      [] (new_type TRoundingMode) None;
    "roundTowardPositive", new_fun
      [] (new_type TRoundingMode) None;
    "RTP", new_fun
      [] (new_type TRoundingMode) None;
    "roundTowardNegative", new_fun
      [] (new_type TRoundingMode) None;
    "RTN", new_fun
      [] (new_type TRoundingMode) None;
    "roundTowardZero", new_fun
      [] (new_type TRoundingMode) None;
    "RTZ", new_fun
      [] (new_type TRoundingMode) None;

    "fp", new_fun
      [(new_type (TBitVec 0));
       (new_type (TBitVec 0));
       (new_type (TBitVec 0))]
      (new_type (TFloatingPoint (0,0))) None;

    "fp", new_fun
      [(new_type TInt);
       (new_type TInt);
       (new_type TInt)]
      (new_type (TFloatingPoint (0,0))) None;

    "fp.to_real", new_fun
      [new_type (TFloatingPoint (0,0))]
      (new_type TReal) None;

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.abs", new_fun
       [x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.neg", new_fun
       [x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.add", new_fun
       [(new_type TRoundingMode);x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.sub", new_fun
       [(new_type TRoundingMode);x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.mul", new_fun
       [(new_type TRoundingMode);x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.div", new_fun
       [(new_type TRoundingMode);x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.fma", new_fun
       [(new_type TRoundingMode);x;x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.sqrt", new_fun
       [(new_type TRoundingMode);x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.rem", new_fun
       [x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.roundToIntegral", new_fun
       [(new_type TRoundingMode);x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.min", new_fun
       [x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.max", new_fun
       [x;x] x None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.leq", new_fun
       [x;x]
       (new_type TBool) (Some Chainable));

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.lt", new_fun
       [x;x]
       (new_type TBool) (Some Chainable));

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.geq", new_fun
       [x;x]
       (new_type TBool) (Some Chainable));

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.gt", new_fun
       [x;x]
       (new_type TBool) (Some Chainable));

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.eq", new_fun
       [x;x]
       (new_type TBool) (Some Chainable));

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.isNormal", new_fun
       [x] (new_type TBool) None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.isSubnormal", new_fun
       [x] (new_type TBool) None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.isZero", new_fun
       [x] (new_type TBool) None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.isInfinite", new_fun
       [x] (new_type TBool) None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.isNaN", new_fun
       [x] (new_type TBool) None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.isNegative", new_fun
       [x] (new_type TBool) None);

    (let x = new_type (TFloatingPoint (0,0)) in
     "fp.isPositive", new_fun
       [x] (new_type TBool) None);
  ];
  par_funs = [

    ("to_fp", (fun l ->
         match l with
         | [a;b] ->
           let a = int_of_string a in
           let b = int_of_string b in
           new_fun [(new_type (TBitVec (a+b)))]
             (new_type (TFloatingPoint (a,b))) None;
         | _ -> assert false
       )
    );

    ("to_fp", (fun l ->
         match l with
         | [a;b] ->
           let a = int_of_string a in
           let b = int_of_string b in
           new_fun [(new_type TRoundingMode);
                    (new_type (TFloatingPoint (0,0)))]
             (new_type (TFloatingPoint (a,b))) None;
         | _ -> assert false
       )
    );

    ("to_fp", (fun l ->
         match l with
         | [a;b] ->
           let a = int_of_string a in
           let b = int_of_string b in
           new_fun [(new_type TRoundingMode);
                    (new_type TReal)]
             (new_type (TFloatingPoint (a,b))) None;
         | _ -> assert false
       )
    );

    ("to_fp", (fun l ->
         match l with
         | [a;b] ->
           let a = int_of_string a in
           let b = int_of_string b in
           new_fun [(new_type TRoundingMode);
                    (new_type (TBitVec 0))]
             (new_type (TFloatingPoint (a,b))) None;
         | _ -> assert false
       )
    );

    ("to_fp_unsigned", (fun l ->
         match l with
         | [a;b] ->
           let a = int_of_string a in
           let b = int_of_string b in
           new_fun [(new_type TRoundingMode);
                    (new_type (TBitVec 0))]
             (new_type (TFloatingPoint (a,b))) None;
         | _ -> assert false
       )
    );

    ("fp.to_ubv", (fun l ->
         match l with
         | [m] ->
           let m = int_of_string m in
           new_fun [(new_type TRoundingMode);
                    (new_type (TFloatingPoint (0,0)))]
             (new_type (TBitVec m))
             None;
         | _ -> assert false
       )
    );

    ("fp.to_sbv", (fun l ->
         match l with
         | [m] ->
           let m = int_of_string m in
           new_fun [(new_type TRoundingMode);
                    (new_type (TFloatingPoint (0,0)))]
             (new_type (TBitVec m))
             None;
         | _ -> assert false
       )
    );

    ("+oo", fun l ->
        match l with
        | [a;b] ->
          let a = int_of_string a in
          let b = int_of_string b in
          new_fun [] (new_type (TFloatingPoint (a,b))) None;
        | _ -> assert false
    );

    ("-oo", fun l ->
        match l with
        | [a;b] ->
          let a = int_of_string a in
          let b = int_of_string b in
          new_fun [] (new_type (TFloatingPoint (a,b))) None;
        | _ -> assert false
    );

    ("+zero", fun l ->
        match l with
        | [a;b] ->
          let a = int_of_string a in
          let b = int_of_string b in
          new_fun [] (new_type (TFloatingPoint (a,b))) None;
        | _ -> assert false
    );

    ("-zero", fun l ->
        match l with
        | [a;b] ->
          let a = int_of_string a in
          let b = int_of_string b in
          new_fun [] (new_type (TFloatingPoint (a,b))) None;
        | _ -> assert false
    );

    ("NaN", fun l ->
        match l with
        | [a;b] ->
          let a = int_of_string a in
          let b = int_of_string b in
          new_fun [] (new_type (TFloatingPoint (a,b))) None;
        | _ -> assert false
    );
  ] }

let bit_vectors = {
  sorts = ["BitVec",((0,1),
                     (fun _s (l1,l2) ->
                        assert (List.length l2 = 1 && l1 == []);
                        TBitVec(List.hd l2)
                     ))];
  funs = [];
  par_funs = []
}


let add_theories env ths =
  let aux env th =
    let th_def =
      match th with
      | Core -> core
      | Ints -> ints
      | Reals ->
        (*let sorts = if get_is_real () then
            ("Int",((0,0),(fun s (l1,l2) ->
                 assert (l1 == [] && l2 == []); TReal)))
            :: reals.sorts
          else reals.sorts
          in sorts*)
        reals
      | Reals_Ints -> reals_ints
      | FloatingPoint -> floating_point
      | Arrays -> arrays
      | BitVectors -> bit_vectors
    in
    let env = Smtlib_typed_env.add_sorts env th_def.sorts in
    let env = Smtlib_typed_env.add_funs env th_def.funs in
    Smtlib_typed_env.add_par_funs env th_def.par_funs
  in
  List.fold_left (fun env th -> aux env th) env ths


let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let set_logic env s =
  let logic = s.Smtlib_syntax.c in
  let theories = ref [Core] in
  let all = contains logic "ALL" in
  if contains logic "QF" then
    set_is_qf true;
  if all || contains logic "UF" then
    set_is_uf true;

  if contains logic "BV" then
    check_command "Bitvector";
  if contains logic "FP" then begin
    theories := FloatingPoint :: !theories;
    set_is_fp true;
  end;

  if all || contains logic "AX" || contains logic "A" then
    theories := Arrays :: !theories;

  if all || contains logic "IRA" then begin
    set_is_int_real true;
    theories := Reals_Ints :: !theories
  end
  else if contains logic "IA" || contains logic "IDL" then
    theories := Ints :: !theories
  else if contains logic "RA" || contains logic "RDL" then begin
    set_is_real true;
    theories := Reals :: !theories
  end;

  if all || contains logic "LIRA" || contains logic "LIA" ||
     contains logic "LRA" then
    set_is_linear true;
  if contains logic "NIRA" || contains logic "NIA" || contains logic "NRA" then
    set_is_non_linear true;

  if contains logic "DT" then
    set_is_dt true;

  add_theories env !theories
