open Util ;;

exception Unexpected_newline ;;
exception Unexpected_eof ;;
exception Empty_key ;;
exception Invalid_escape_char ;;

type token =
    | Equals

    | BraceOpen
    | BraceClosed
    | Separator

    | Comment   of string

    | Key       of string
    | Value     of string

    | EOF
;;

let string_of_token t =
    match t with
    | Equals            -> "="

    | BraceOpen         -> "["
    | BraceClosed       -> "]"
    | Separator         -> "\n"
    
    | Comment s         -> "#" ^ s

    | Key s             -> "\"" ^ s ^ "\"" ^ " = "
    | Value s           -> "\"" ^ s ^ "\"" 

    | EOF               -> "<EOF>"
;;

let read_comment cl =  
    let rec rcmt cmt = function
    | '\n'  :: cs   -> (cmt, cs)
    | c     :: cs   -> let (cmt', cs') = rcmt cmt cs in (c::cmt', cs')
    | []            -> (cmt, [])
    in rcmt [] cl
;;

let hex_value_of a b =
    let hex c = 
    match c with
    | '0'..'9'  -> Char.code c - 48
    | 'A'..'F'  -> Char.code c - 55
    | 'a'..'f'  -> Char.code c - 87
    | _         -> raise Invalid_escape_char    
    in Char.chr (((hex a) * 16) + (hex b))
;;

type read_quoted_str_state = Root | Backslash ;;
let rqs cl = 
    let rec rstr qs = function
    | Root -> (function
        | '\\'  :: cs   -> rstr qs Backslash cs
        | '\n'  :: _    -> raise Unexpected_newline
        | '"'   :: cs   -> (qs, cs) 
        | c     :: cs   -> let (qs', cs') = rstr qs Root cs in (c::qs', cs')
        | [] -> raise Unexpected_eof
    )
    | Backslash -> (function
        | 'n'   :: cs   -> let (qs', cs') = rstr qs Root cs in ('\n'::qs', cs')
        | 'r'   :: cs   -> let (qs', cs') = rstr qs Root cs in ('\r'::qs', cs')
        | 't'   :: cs   -> let (qs', cs') = rstr qs Root cs in ('\t'::qs', cs')
        | '"'   :: cs   -> let (qs', cs') = rstr qs Root cs in ('"' ::qs', cs')
        | '\\'  :: cs   -> let (qs', cs') = rstr qs Root cs in ('\\'::qs', cs')
        
        | 'x'   :: a :: b :: cs -> let (qs', cs') = rstr qs Root cs in ((hex_value_of a b)::qs', cs')

        | _             -> raise Invalid_escape_char
    )
    in rstr [] Root cl
;;

type read_multiline_str_state = Root | Pipe ;;
let rms cl =
    let rec rstr ms = function
    | Root -> (function
        | (' '|'\t') :: cs  -> rstr ms Root cs
        | '|' :: cs         -> rstr ms Pipe cs
        | c   :: cs         -> (ms, c::cs)
        | []                -> (ms, [])
    )
    | Pipe -> (function
        | '\\' :: '\n' :: cs    -> rstr ms Root cs
        | '\n' :: cs            -> let (ms', cs') = rstr ms Root cs in ('\n'::ms', cs')
        | c    :: cs            -> let (ms', cs') = rstr ms Pipe cs in (c::ms', cs')
        | []                    -> (ms, ['a'])
    )
    in rstr [] Pipe cl
;;

(* TODO: a better way to do this?
 * i wrote this at midnight while i was tired af
 * so this might be really bad *)
let rus cl =
    let rec rstr us = function
        | c :: cs -> begin function
            | ('\n'|'='|';'|','|']') -> (us, c::cs)
            | _ -> let (us', cs') = rstr us cs in (c::us', cs')
        end c
        | [] -> (us, [])
    in let rec trim = function
        | (' '|'\t') :: ts  -> trim ts
        | []                -> []
        | ts                -> ts
    in let (qs, cs) = (rstr [] cl)
    in let qs' = trim (List.rev qs)
    in (List.rev qs', cs)
;;

let rec tokenize = function
    | (' '|'\t') :: cs  -> tokenize cs
    
    | '=' :: cs             -> Equals       :: tokenize cs
    | '[' :: cs             -> BraceOpen    :: tokenize cs
    | ']' :: cs             -> BraceClosed  :: tokenize cs
    | (';'|','|'\n') :: cs  -> Separator    :: tokenize cs

    | '#' :: cs -> 
        let (cmt, cs') = (read_comment cs) in
        (Comment (string_of_charlist cmt)) :: tokenize cs'

    | c :: cs -> begin
        let (cl, cs') = match c with '"' -> rqs cs | '|' -> rms cs | _ -> rus (c::cs) in
        let str = string_of_charlist cl in

        match (tokenize cs') with
            | Equals :: ts  -> (Key str)    :: ts
            | ts            -> (Value str)  :: ts
        end

    | [] -> [EOF]
;;

let tokenize_str str = tokenize (charlist_of_string str)
