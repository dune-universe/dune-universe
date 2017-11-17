
(* Inserts a 16-bit value in string s, at position p. Big endian. *)
let insert16 s ~pos x =
  assert (0 <= x && x <= 0xffff) ;
  assert (0 <= pos) ; 
  assert (String.length s >= pos + 2) ;
  let s = Bytes.of_string s in
  Bytes.set s pos (Char.chr (x lsr 8)) ;
  Bytes.set s (pos+1) (Char.chr (x mod 256)) ;
  Bytes.to_string s

let read16 s ~pos =
  assert (String.length s >= pos + 2) ;
  (Char.code s.[pos+0]) lsl 8 + (Char.code s.[pos+1])


(* Add padding and padding length. *)
let pad phrase padding_len =
  assert (padding_len >= 0) ;
  (* Header = an integer k (2 bytes) + padding chars (k bytes) *)
  let header = String.make (padding_len + 2) 'x' in
      
  (* 2 bytes for padding_len. *)
  (insert16 header 0 padding_len) ^ phrase

(* Remove padding. *)
let unpad phrase =
  let len = String.length phrase in
  assert (len >= 2) ;
  (* k is the padding length. *)
  let k = read16 phrase 0 in
  let len = len - (k+2) in
  assert (len >= 0) ;
  String.sub phrase (k+2) len

let append_char s c = s ^ (String.make 1 c)

let get_last_char s =
  let len = String.length s in
  assert (len > 0) ;
  
  let last = s.[len-1]
  and sub = String.sub s 0 (len-1) in

  (last, sub)

let get_first_char s =
  let len = String.length s in
  assert (len > 0) ;
  
  let first = s.[0]
  and sub = String.sub s 1 (len-1) in
  
  (first, sub)



