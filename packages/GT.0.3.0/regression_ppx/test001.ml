open GT

type 'l t =
    R 
  | W 
  | L  of string 
  | S  of string 
  | B  of (int -> int -> int) * string
  | E
  | C  of int
  | J  of 'l
  | JT of 'l
  | JF of 'l
[@@deriving gt]

class toString fl =
  object
    inherit [int, unit, string, unit, string, 'extra] class_t
    method c_R  ()     = "R"
    method c_W  ()     = "W"
    method c_L  () x   = "L " ^ x
    method c_S  () x   = "S " ^ x
    method c_B  () _ x = "B " ^ x
    method c_E  ()     = "E"
    method c_C  () x   = "C "  ^ (string_of_int x)
    method c_J  () x   = "J "  ^ (fl () x)
    method c_JT () x   = "JT " ^ (fl () x)
    method c_JF () x   = "JF " ^ (fl () x)
  end

class resolve fself fa =
  object
    inherit [string, unit, int, unit, int t, _] class_t
    method c_R          = R
    method c_W _        = W
    method c_L inh x    = L x
    method c_S inh x    = S x
    method c_B inh f x  = B (f, x)
    method c_E inh      = E
    method c_C inh  x   = C x

    method c_J inh  x   = J  (fself inh x)
    method c_JT inh x   = JT (fself inh x)
    method c_JF inh x   = JF (fself inh x)
  end

let resolve p = 
  let symbols = ref [] in
  let p = Array.mapi (fun i (s, c) -> if s != "" then symbols := (s, i) :: !symbols; c) p in
  Array.map (fun i -> transform(t) (fun _ i -> List.assoc i !symbols) (new resolve) () i) p

let toString i  = transform(t) (fun _ i -> string_of_int i) (new toString) () i

type env  = int list * (string -> int) * int list * int list * int

class interpret =
  object
    inherit [int, env, int, env, env option] class_t
    method c_R  (      s, m, x::i, o, p) _     = Some (x::s, m, i, o, p+1)
    method c_W  (   x::s, m,    i, o, p) _     = Some (s, m, i, x::o, p+1)
    method c_L  (      s, m,    i, o, p) _ x   = Some ((m x)::s, m, i, o, p+1)
    method c_S  (   y::s, m,    i, o, p) _ x   = Some (s, (fun z -> if z = x then y else m z), i, o, p+1)
    method c_B  (y::z::s, m,    i, o, p) _ f _ = Some ((f z y)::s, m, i, o, p+1)
    method c_E   _ _                           = None
    method c_C  (      s, m,    i, o, p) _ n   = Some (n::s, m, i, o, p+1)
    method c_J  (      s, m,    i, o, p) _ n   = Some (s, m, i, o, ~:n)
    method c_JT (   x::s, m,    i, o, p) _ n   = Some (s, m, i, o, if x != 0 then ~:n else p+1)
    method c_JF (   x::s, m,    i, o, p) _ n   = Some (s, m, i, o, if x  = 0 then ~:n else p+1)   
  end

class debug callback =
  object (this)
    inherit interpret as super
    method c_R  c i     = callback i c; super#c_R  c i
    method c_W  c i     = callback i c; super#c_W  c i
    method c_L  c i x   = callback i c; super#c_L  c i x
    method c_S  c i x   = callback i c; super#c_S  c i x
    method c_B  c i x y = callback i c; super#c_B  c i x y
    method c_E  c i     = callback i c; super#c_E  c i 
    method c_C  c i x   = callback i c; super#c_C  c i x
    method c_J  c i x   = callback i c; super#c_J  c i x
    method c_JT c i x   = callback i c; super#c_JT c i x
    method c_JF c i x   = callback i c; super#c_JF c i x
  end

let interpret ii p i =
  let rec inner ((_, _, _, o, i) as conf) =
    match transform(t) (fun _ i -> i) ii conf p.(i) with
    | None      -> List.rev o
    | Some conf -> inner conf
  in
  inner ([], (fun x -> invalid_arg (Printf.sprintf "Variable %s undefined" x)), i, [], 0)

let sum  = [|R; R; B ((+), "+"); W; E|]
let sumN = [|
  R;
  S  "n";
  C   0 ;
  S  "s";
  L  "n"; (* cont *)
  JF  15;
  L  "n";
  C   1 ;
  B  ((-), "-");
  S  "n";
  R;
  L  "s";
  B  ((+), "+");
  S  "s";
  J   4 ;
  L  "s"; (* end *)
  W;
  E
|]

let sumS = [|"", R; "", R; "", B ((+), "+"); "", W; "", E|]
let sumNS = [|
  ""    , R;
  ""    , S  "n";
  ""    , C   0 ;
  ""    , S  "s";
  "cont", L  "n"; 
  ""    , JF  "end";
  ""    , L  "n";
  ""    , C   1 ;
  ""    , B  ((-), "-");
  ""    , S  "n";
  ""    , R;
  ""    , L  "s";
  ""    , B  ((+), "+");
  ""    , S  "s";
  ""    , J  "cont";
  "end" , L  "s"; 
  ""    , W;
  ""    , E
|]

let _ = 
  let ii = new interpret in
  let dd = new debug (fun i (_, _, _, _, p) -> Printf.printf "%s @ %d\n" (toString ~:i) p) in
  let main name xx p i = 
    Printf.printf "%s:\n" name;
    List.iter (fun x -> Printf.printf "%d\n" x) (interpret xx p i) 
  in  
  main "sum"  ii sum [2; 3];
  main "sumN" ii sumN [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
  main "sum with debug"  dd sum [2; 3];
  main "sumN with debug" dd sumN [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
  main "sum"  ii (resolve sumS) [2; 3];
  main "sumN" ii (resolve sumNS) [10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];

