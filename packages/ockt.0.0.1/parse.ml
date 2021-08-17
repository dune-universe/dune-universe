open Token ;;

exception Invalid_character ;;
exception Unexpected_eof ;;

type key = string
type value = 
    | String of string
    | Table  of (key, value) Hashtbl.t
;;

let get_string = function
    | String s  -> s
    | Table _   -> raise Not_found
;; 

let get_table = function
    | String _  -> raise Not_found
    | Table t   -> t 
;;

let string_of_value v = 
    let rec str i = function
        | String s -> s
        | Table t -> 
            "[\n" 
            ^ (fun h -> (Hashtbl.fold (fun k v s -> i ^ "  " ^ k ^ " = " ^ str (i ^ "  ") v ^ "\n" ^ s) h "")) t
            ^ i ^ "]"
    in str "" v 
;;

let rec table_of_tokens tokens =
    let table : (key, value) Hashtbl.t = Hashtbl.create 1 in
    let rec fnext = function
    | Separator     :: ts   -> fnext ts
    | Comment _     :: ts   -> fnext ts
    | BraceClosed   :: _    -> raise Invalid_character
    | EOF           :: _    -> raise Unexpected_eof
    | t             :: ts   -> t :: ts
    | []                    -> raise Unexpected_eof
    in 
    let rec rtkn i = function 
    | Key k :: kts -> begin
        match (fnext kts) with 
        | Value v   :: ts -> Hashtbl.add table (k) (String v); (rtkn i ts)
        | BraceOpen :: ts ->
            let (tb, ts') = table_of_tokens ts in 
            Hashtbl.add table k tb; 
            rtkn i ts'
        | _ -> raise Invalid_character
        end
    | Value v       :: ts   -> Hashtbl.add table (string_of_int i) (String v); (rtkn (i+1) ts)
    | BraceOpen     :: ts   ->
        let (tb, ts') = table_of_tokens ts in 
        Hashtbl.add table (string_of_int i) tb; 
        rtkn (i+1) ts'
    | BraceClosed   :: ts   -> ts
    | [EOF]                 -> []  
    | _             :: ts   -> (rtkn i ts)
    | [] -> [] 
    in (Table table), (rtkn 0 tokens)
;;

let parse s = let (tbl, _) = table_of_tokens (Token.tokenize_str s) in tbl
