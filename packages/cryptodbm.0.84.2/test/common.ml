
(***** CONVENIENT SMALL FUNCTIONS *****)

let (//) = Filename.concat
let (++) f g = fun x -> f (g x)

let foi = float_of_int
let iof = int_of_float
let soi = string_of_int

let cst x = fun _ -> x
let id x = x

let () = Random.self_init ()

(***** EXCEPTIONS *****)

(*
let check cond msg = 
  if not cond then
    begin
      LOG "%s" msg LEVEL ERROR ;
      failwith msg
    end
*)

(***** OPERATIONS ON OPTION TYPE *****)

let option_iter o f = match o with
  | None -> ()
  | Some s -> f s

let option_map o f = match o with
  | None -> None
  | Some s -> Some (f s)

let option_default o d = match o with
  | None -> d
  | Some x -> x

let option_get = function
  | None -> raise Not_found
  | Some x -> x

let option_apply default o f = match o with
  | None -> default
  | Some y -> f y


(***** OPERATIONS ON LISTS *****)

let myfold l a f   = List.fold_left f a l
let myiter l f     = List.iter f l

let mymap  l f     = List.map f l
let myrevmap l f   = List.rev_map f l
let mymap2 l1 l2 f = List.map2 f l1 l2
let myrevmap2 l1 l2 f  = List.rev_map2 f l1 l2

(* Maps and append *)
let rec revmapappend f acu = function
  | [] -> acu
  | x :: xs -> revmapappend f ((f x) :: acu) xs

(* Maps and filter. If map returns None, the element is discarded. If Some x, x is appended to the result. *)
let rec revmapfilter' f acu = function
  | [] -> acu
  | x :: xs ->
      begin match f x with
      | None -> revmapfilter' f acu xs
      | Some y -> revmapfilter' f (y :: acu) xs
      end

let revmapfilter f l = revmapfilter' f [] l 

(* Finds an element for which the function returns Some x. *)
let rec mapfind f = function
  | [] -> None
  | x :: xs ->
      begin match f x with
      | None -> mapfind f xs
      | res -> res
      end

let rec ifold_aux f a index = function
  | [] -> a
  | x :: xs -> ifold_aux f (f index a x) (index + 1) xs

let ifold l a f = ifold_aux f a 0 l

(* Like &&, but for a list of items. *)
let rec and_list l f = match l with
  | [] -> true
  | x :: xs -> f x && and_list xs f

(* Like ||, but for a list of items. Equivalent to List.exists f l *)
let rec or_list l f = match l with
  | [] -> false
  | x :: xs -> f x || or_list xs f

(*** Arrays ***)
let matrix_init nb_lines nb_cols f =
  Array.init nb_lines (fun line -> Array.init nb_cols (fun col -> f line col))

let matrix_fold mat acu f =
  fst (Array.fold_left
	 begin fun (acu, linenb) row ->
	   let (acu', _) = Array.fold_left (fun (acu, colnb) cell -> (f linenb colnb acu cell, colnb + 1)) (acu, 0) row in
	   (acu', linenb + 1)
	 end
	 (acu, 0) mat)

(* Shuffles a list (returns an array) *)
let ashuffle l =
  match l with
  | [] -> [||]
  | el :: _ ->
      let src = Array.of_list l in
      let len = Array.length src in

      (* Result *)
      let res = Array.make len el

      (* Bit array to indicate which cells are used. *)
      and used = Bitarray.create len in

      (* Find an unused cell, starting at position pos. 
       * count = number of full cells already considered. *)
      let rec find count pos =
	assert(count < len) ;
	if Bitarray.get used pos then find (count+1) ((pos + 1) mod len)
	else pos	  
      in
      
      (* Fills the result. *)
      for index = 0 to len - 1 do
	let pos = find 0 (Random.int len) in
	Bitarray.set used pos true ;
	res.(pos) <- src.(index) ;
      done ;

      res

(* Shuffles a list. *)
let shuffle l = Array.to_list (ashuffle l)
  

(***** OPERATIONS ON STRINGS *****)

(* Builds a string from a list of items *)
let sep map sp l = List.fold_left (fun acu x -> if acu = "" then map x else acu ^ sp ^ (map x)) "" l

(* Indicates if string b starts with string a, that is, a is a prefix of b. *)
let starts_with a b =
  let l = String.length a in
  (String.length b >= l) && (String.sub b 0 l = a)

(* Indicates if string b ends with string a, that is, a is a suffix of b. *)
let ends_with a b =
  let la = String.length a
  and lb = String.length b in
  (lb >= la) && (String.sub b (lb - la) la = a)


