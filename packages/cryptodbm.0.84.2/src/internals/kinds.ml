type location =
  | Table_Builtin
  | Subtable_Builtin of int
  | Subtable_User of int

let max_subtable = 1 lsl 14 - 1

let loc2s = function
  | Table_Builtin -> "Table_Builtin"
  | Subtable_Builtin n -> "Subtable_Builtin " ^ (string_of_int n)
  | Subtable_User n -> "Subtable_User " ^ (string_of_int n)

type 'a howstored =
  | Uncrypted
  | Encrypted of 'a

(* Encrypting function. Only for KEYs. *)
type hidden = 
    { table_encrypt : string -> string ;
      subtable_encrypt : string -> string }

type key_kind = 
    { key_loc : location ;
      key_how : (Cipher.passwd * Cipher.passwd * int) howstored ;
      cryptf  : hidden }

type data_kind =
   { data_how : (Cipher.passwd * int) howstored }

let mk_data how = { data_how = how }

(* Optimization: pre-compute encrypting functions when necessary. *)
let encrypt_error _ = (Printf.printf "Unexpectedly calling encrypt_error in kinds.ml\n%!" ; assert false)
let nocryptf = { table_encrypt = encrypt_error ; subtable_encrypt = encrypt_error }
let mkcrypt p = 
  if p == Cipher.empty_passwd then encrypt_error else 
  begin
    (* Printf.printf "[[mkcrypt%!" ; *)
    let res = Cipher.encrypt ~passwd:p in
    (* Printf.printf "]]\n%!" ; *)
    res
  end

let mk_key loc how =
  let cryptf =
    match how with
    | Uncrypted -> nocryptf
    | Encrypted (table_passwd, subtable_passwd, _) ->
	assert (not (table_passwd == Cipher.empty_passwd && subtable_passwd == Cipher.empty_passwd)) ;
	assert (loc <> Table_Builtin || subtable_passwd == Cipher.empty_passwd) ;
	{ table_encrypt = mkcrypt table_passwd ;
	  subtable_encrypt = mkcrypt subtable_passwd }
  in
  { key_loc = loc ;
    key_how = how ;
    cryptf }

(* These are just regular strings. No boxing. *)
type encoded_key = string
type encoded_data = string

let cmp_encoded_key ek1 ek2 = compare ek1 ek2

module LowerDB = LowerDB_impl

(* Randomly choose the number of padding bytes for data. *)
let choose_data_padding max_pad = Random.int (max_pad + 1)

open Strings

(* Encode the location as a string.
 *   Table_builtin -> 0x11000000
 *   Subtable_Builtin -> 0x01...+subtable(14 bits)
 *   Subtable_User -> 0x00...+subtable(14 bits)
 *)
let put_location = function
  | Table_Builtin -> "\xc0"
  | Subtable_Builtin subt -> assert (subt >= 0 && subt <= max_subtable) ; insert16 "oo" 0 (0x4000 lor subt)
  | Subtable_User subt -> assert (subt >= 0 && subt <= max_subtable) ; insert16 "oo" 0 subt

let loc2hash = put_location

let get_location s =
  let len = String.length s in
  assert (len > 0) ;
  match s.[0] with
  | '\xc0' -> (Table_Builtin, String.sub s 1 (len-1))
  | _ ->
      assert (len >= 2) ;
      let code = read16 s ~pos:0 in
      let subt = code land 0x3FFF
      and hd   = (code land 0xc000) lsr 14 in
      
      let loc = 
	match hd with
	| 0 -> Subtable_User subt
	| 1 -> Subtable_Builtin subt
	| _ -> assert false (* Incorrect encoding. *)
      in

      (loc, String.sub s 2 (len-2))

let encode_data data kind =
  match kind.data_how with
  | Uncrypted -> data
  | Encrypted (passwd, padlength) ->
      assert (passwd != Cipher.empty_passwd) ;
      let padded_data = if padlength = 0 then pad data 0 else pad data (choose_data_padding padlength) in
      Cipher.encrypt ~passwd padded_data

let decode_data encdata kind =
  match kind.data_how with
  | Uncrypted -> encdata
  | Encrypted (passwd, _) ->
      assert (passwd != Cipher.empty_passwd) ;
      let paddata = Cipher.decrypt ~passwd encdata in
      unpad paddata

let encode_key key kind =
  match kind.key_how with
  | Uncrypted -> append_char (put_location kind.key_loc ^ key) '0'

  | Encrypted (table_passwd, subtable_passwd, padlength) ->
      (*  encrypted case => table_encryption(location ^ char ^ subtable_encryption(location? ^ pad(KEY))) ^ lastchar
       *                    location? is not needed if the subtable password is empty
       *                    lastchar is '1' (no table encryption) or '2' (table encryption)
       *                    char is 'E' (subtable has its own encryption), 'T' (subtable does not have its own encryption). *)

      assert (not (table_passwd == Cipher.empty_passwd && subtable_passwd == Cipher.empty_passwd)) ;
      assert (kind.key_loc <> Table_Builtin || subtable_passwd == Cipher.empty_passwd) ; 

      (* pad(KEY) *)
      let padded_key = 
	if padlength = 0 then pad key 0
	else
	  let passwd = if table_passwd == Cipher.empty_passwd then subtable_passwd else table_passwd in
	  pad key (Cipher.compute_padding ~key ~passwd ~max_pad:padlength)
      in
      
      (*  char ^ subtable_encryption(location ^ pad(KEY)) 
       *  char is 'E' or 'T' *)
      let subtable_key =
        (* Note also that if this is Table_Builtin key, subtable_passwd is empty. *)
	if subtable_passwd == Cipher.empty_passwd then padded_key 
	else 
	  begin
(*	    Printf.printf "((%!" ; *)
	    let located = put_location kind.key_loc ^ padded_key in
(*	    Printf.printf "----%!" ; *)
	    let res = kind.cryptf.subtable_encrypt located in
(*	    Printf.printf "))%!" ; *)
	    res
	  end
      in
      
      (*  location ^ char ^ ... *)
      let located_key =
	let subtable_char = if subtable_passwd == Cipher.empty_passwd then 'T' else 'E' in
	(append_char (put_location kind.key_loc) subtable_char) ^ subtable_key
      in

      (*  table_encryption(...) *)
      let table_key =
	if table_passwd == Cipher.empty_passwd then located_key
	else kind.cryptf.table_encrypt located_key
      in

      (* ... ^ '1' or '2' *)
      append_char table_key (if table_passwd == Cipher.empty_passwd then '1' else '2')

let get_key_info passwd ~subt_pas enckey =
  let (last, rest_key) = get_last_char enckey in
  match last with
  | '0' ->
      (* Uncrypted case *)
      let (key_loc, key) = get_location rest_key in
      
      let kind = { key_loc ;
		   key_how = Uncrypted ;
		   cryptf  = nocryptf }
      in
      Some (kind, Some key)

  (* Encrypted cases *)	
  | '1' | '2' ->

      begin
	(* We should not find a table-uncrypted key when a table password was provided. *)
	assert (not (last = '1' && passwd != Cipher.empty_passwd)) ;
	
	try
	  let table_key =
	    if last = '1' then
	      (* No table encryption *)
	      rest_key
		
	    else
	      (* Table encryption *)
	      if passwd == Cipher.empty_passwd then
		(* The table password is unknown. We can't say anything. *)
		raise Not_found
		  
	      else
		Cipher.decrypt ~passwd rest_key
	  in
	  
	  let (loc, temp_key) = get_location table_key in
	  let (subchar, subtable_key) = get_first_char temp_key in
  
	  let (key, subpass) = 
	    begin match (loc, subchar) with
	    | Table_Builtin, 'T' -> (Some (unpad subtable_key), Cipher.empty_passwd)
	    | Table_Builtin, _ -> assert false (* Table_Builtin cannot be subtable-encrypted. *)
		  
	    | (Subtable_Builtin subt | Subtable_User subt), 'T' ->
		(Some (unpad subtable_key), Cipher.empty_passwd)
		    
	    | (Subtable_Builtin subt | Subtable_User subt), 'E' ->
		let subt_passwd = subt_pas subt in
		if subt_passwd == Cipher.empty_passwd then 
		  (* Don't have the password *)
		  (None, Cipher.empty_passwd)
		else
		  begin
		    let inner_key = Cipher.decrypt ~passwd:subt_passwd subtable_key in
		    let (loc2, padded_key) = get_location inner_key in
		    assert (loc = loc2) ;
		    (Some (unpad padded_key), subt_passwd)
		  end

	    | (Subtable_Builtin subt | Subtable_User subt), _ -> assert false (* Only 'T' or 'E' *)
	    end
	  in
	  
	  let kind =
	    { key_loc = loc ;
	      key_how = Encrypted (passwd, subpass, 0) ;
	      cryptf = nocryptf }
	  in
	  Some (kind, key)
	    
	with Not_found -> None

      end
	    
  | _ -> assert false (* Incorrect encoding *)

let sign passwd v = Cipher.digest ("$_" ^ Cipher.strong_passwd passwd ^ "\003M" ^ v ^ "=")

let sign_encoded_key = sign 
let sign_encoded_data = sign

let id x = x
let encodedkey2s = id
let encodeddata2s = id
let s2encodedkey = id
let s2encodeddata = id


