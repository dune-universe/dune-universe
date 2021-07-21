(*i $Id: polka.ml,v 1.5 2004/02/27 10:34:10 bjeannet Exp $ i*)

exception Overflow of string
let _ =
  Callback.register_exception
    "camlidl_polka_overflow"
    (Overflow "any string")

type dimsup = {
  pos: int;
  nbdims: int;
}

(* %========================================================================= *)
(* \section{Initialization and finalization of the library} *)
(* %========================================================================= *)
let strict = ref false
and dec = ref 0
and print_limit = ref 30

external _initialize: bool -> int -> int -> unit
  = "camlidl_polka_initialize"
external _finalize: unit -> unit
  = "camlidl_polka_finalize"
let initialize ~strict:pstrict ~maxdims ~maxrows =
  _initialize pstrict maxdims maxrows;
  strict := pstrict;
  dec := if pstrict then 3 else 2

let finalize () =
  _finalize ();
  dec := 0

external set_gc : int -> unit = "camlidl_polka_set_gc"
external set_widening_affine : unit -> unit = "camlidl_polka_set_widening_affine"
external set_widening_linear : unit -> unit = "camlidl_polka_set_widening_linear"

(* %========================================================================= *)
(* \section{Input functions, from string to vectors} *)
(* %========================================================================= *)

type cons = Egal | SupEgal | Sup
type gen = Vertex | Ray | Line

let denominator_of_list list =
  let rec parcours res = function
    | [] -> res
    | (_n,d,_v)::suite -> parcours (Big_int.mult_big_int res d) suite
  in
  parcours Big_int.unit_big_int list

(* %========================================================================= *)
(* \section{Output functions, from vectors to string} *)
(* %========================================================================= *)

let to_constraint assoc get size =
  let munit = Big_int.minus_big_int Big_int.unit_big_int in
  let gauche = ref ""
  and droite = ref ""
  and sens_normal = ref true in
  for i = !dec to size-1 do
    let coeff = get i and nom = assoc (i - !dec) in
    let sgn = Big_int.sign_big_int coeff in
    if sgn>0 then begin
      gauche :=
	(if !gauche = "" then !gauche else !gauche ^"+") ^
	(if Big_int.eq_big_int coeff Big_int.unit_big_int then "" else Big_int.string_of_big_int coeff) ^ nom
    end
    else if sgn<0 then begin
      if !gauche = "" then sens_normal := false;
      droite :=
	(if !droite = "" then !droite else (!droite)^"+") ^
	  (if Big_int.eq_big_int coeff munit then "" else Big_int.string_of_big_int (Big_int.minus_big_int coeff)) ^ nom
    end
  done;
  let cst = get 1 in
  let sgn = Big_int.sign_big_int cst in

  if sgn > 0 then
    gauche := (if !gauche = "" then !gauche else !gauche ^"+") ^ (Big_int.string_of_big_int cst)
  else if sgn < 0 then
    droite := (if !droite = "" then !droite else (!droite)^"+") ^ (Big_int.string_of_big_int (Big_int.minus_big_int cst));

  if !gauche="" && !droite="" then (* aucun coefficient non nul, a part (peut-etre) $\epsilon$ *)
    if !strict then
      let epsilon = get 2 in
      if Big_int.eq_big_int epsilon Big_int.unit_big_int then
	"$>=0"
      else if Big_int.eq_big_int epsilon Big_int.zero_big_int then
	"0"
      else
	let msg = Format.sprintf "Vector._to_constraint: anomaly, constraint %s$ >= 0 !" (Big_int.string_of_big_int epsilon) in
	failwith msg
    else
      "0"
  else begin
    if !gauche="" then gauche := "0"
    else if !droite="" then droite := "0";
    let signe =
      let c0 = get 0 in
      if Big_int.eq_big_int c0 Big_int.zero_big_int then "="
      else begin
	let c2 = get 2 in
	let sgn2 = Big_int.sign_big_int c2 in
	if !strict && sgn2<0 then
	if !sens_normal then ">" else "<"
      else if !strict && sgn2>0 then
	if !sens_normal then ">" else "<"
      else
	if !sens_normal then ">=" else "<="
      end
    in
    if !sens_normal then
      !gauche ^ signe ^ !droite
    else
      !droite ^ signe ^ !gauche
  end

let to_frame assoc get size =
  let chaine = ref "" in
  if !strict then begin
    let epsilon = get 2 in
    let sgn = Big_int.sign_big_int epsilon in
    if sgn > 0 then
      chaine := (if Big_int.eq_big_int epsilon Big_int.unit_big_int then "" else (Big_int.string_of_big_int epsilon)) ^ "$" ^ !chaine
    else if sgn < 0 then
      failwith "Vector._to_constraint: anomaly, vertex with \\epsilon < 0 !"
  end;
  for i = !dec to size-1 do
    let coeff = get i and nom = assoc (i - !dec) in
    let ajout =
      let sgn = Big_int.sign_big_int coeff in
      if sgn>0 then
	(^)
	  (if !chaine="" then "" else "+")
	  (if Big_int.eq_big_int coeff Big_int.unit_big_int then nom else (Big_int.string_of_big_int coeff)^nom)
      else if sgn<0 then
	let coeff = Big_int.minus_big_int coeff in
	(^)
	  "-"
	  (if Big_int.eq_big_int coeff Big_int.unit_big_int then nom else (Big_int.string_of_big_int coeff)^nom)
      else
	""
    in
    chaine := !chaine ^ ajout
  done;
  let x0 = get 0
  and x1 = get 1
  in
  if Big_int.eq_big_int x1 Big_int.zero_big_int then begin
    (* line or ray *)
    if Big_int.eq_big_int x0 Big_int.zero_big_int then
      (* line *)
      "L:" ^ !chaine
    else
       (* ray *)
      "R:" ^ !chaine
  end else begin
    (* vertex *)
    if !chaine="" then
      chaine := "0"
    else if not (Big_int.eq_big_int x1 Big_int.unit_big_int) then
      chaine := "(" ^ !chaine ^ ")/" ^ (Big_int.string_of_big_int x1);
    if !strict && not (Big_int.eq_big_int (get 2) Big_int.zero_big_int) then
      "Vs:" ^ !chaine
    else
      "V:" ^ !chaine
  end

let to_expr assoc get size =
  if (size < !dec) then
    "vector of size "^(string_of_int size)
  else
  let chaine = ref "" in
  for i = !dec to size-1 do
    let coeff = get i and nom = assoc (i - !dec) in
    let sgn = Big_int.sign_big_int coeff in
    let coeff = Big_int.abs_big_int coeff in
    if sgn<>0 then
      chaine :=
	!chaine ^
	  (if sgn < 0 then "-" else (if !chaine="" then "" else "+")) ^
	  (if Big_int.eq_big_int coeff Big_int.unit_big_int then "" else Big_int.string_of_big_int coeff) ^
	  nom
  done;
  let cst = get 1 in
  let sgn = Big_int.sign_big_int cst in
  if sgn<>0 then
      chaine :=
	!chaine ^
	(if sgn < 0 then "-" else (if !chaine="" then "" else "+")) ^
	(Big_int.string_of_big_int (Big_int.abs_big_int cst))
	  ;
  if !chaine="" then chaine := "0";
  let denom = get 0 in
  if Big_int.eq_big_int denom Big_int.unit_big_int then
    !chaine
  else
    "(" ^ !chaine ^ ")/" ^ (Big_int.string_of_big_int denom)

let print_list fmt
  (deb:(unit,Format.formatter,unit) format)
  (sep:(unit,Format.formatter,unit) format)
  (fin:(unit,Format.formatter,unit) format)
  func liste
  =
  if liste=[] then begin
    Format.fprintf fmt deb;
    Format.fprintf fmt fin;
  end
  else begin
    Format.fprintf fmt deb;
    let rec do_sep = function
      [e] -> func fmt e
      | e::l -> (func fmt e ; Format.fprintf  fmt sep; do_sep l)
    | [] -> failwith "matrix.nw: pretty_list"
    in
    do_sep liste;
    Format.fprintf fmt fin;
  end
