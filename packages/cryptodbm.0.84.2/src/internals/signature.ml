open Types

type signature = string

let equal s1 s2 = s1 = s2

(* Adds another binding to the current signature. 
 * Receives location and key, gets the data. *)
let add_item handler signwd (loc, encrypted_key) sign =
  let encrypted_data = Operations.get_encrypted handler encrypted_key in
  let signed_key = Kinds.sign_encoded_key signwd encrypted_key
  and signed_data = Kinds.sign_encoded_data signwd encrypted_data in
  Cipher.digest (sign ^ signed_key ^ signed_data ^ Kinds.loc2hash loc)

let subtable_signature handler ~subtable_salt ~passwd ~subt ~signwd =
  (* Get all relevant keys, sorted. *)
  let all_keys = ref (Setp.empty Pervasives.compare) in

  Operations.iter_subtable_encrypted handler passwd ~subt
    begin fun loc encoded_key ->
      match loc with
      | Kinds.Table_Builtin -> assert false
      | Kinds.Subtable_Builtin _ -> ()
      | Kinds.Subtable_User n ->
	  assert (n = subt) ;
	  all_keys := Setp.add (loc, encoded_key) !all_keys ;
    end ;

  (* Get start value to compute signature. *)
  let start = Config.add_salt subtable_salt (Cipher.strong_passwd signwd) in

  (* Iterate over these keys. *)
  Setp.fold (add_item handler signwd) !all_keys start

(* Adds another binding to the current signature. 
 * Reveives key and data. *)
let add_binding handler signwd (encrypted_key, encrypted_data) sign =
  let signed_key = Kinds.sign_encoded_key signwd encrypted_key
  and signed_data = Kinds.sign_encoded_data signwd encrypted_data in     
  Cipher.digest (sign ^ signed_key ^ signed_data)

let table_key_kind = Kinds.(mk_key Table_Builtin Uncrypted)

let table_signature handler ~table_salt ~signwd =
  (* Forbid the signature binding. *)
  let encoded_signature_key = Kinds.encode_key Config.signature_key table_key_kind in

  (* Get all bindings, sorted. *)
  let all_keys = ref (Setp.empty Pervasives.compare) in
  Operations.iter_all handler 
    (fun encoded_key encoded_data -> if encoded_key <> encoded_signature_key then all_keys := Setp.add (encoded_key, encoded_data) !all_keys) ;

  (* Get start value to compute signature. *)
  let start = Config.add_salt table_salt (Cipher.strong_passwd signwd) in

  (* Iterate over all bindings. *)
  Setp.fold (add_binding handler signwd) !all_keys start

let sign_subtable handler ~subtable_salt ~passwd key_kind ~subt ~signwd =
  assert (subt >= 0) ;
  let signature = subtable_signature handler ~subtable_salt ~passwd ~subt ~signwd in
  Operations.add ~may_overwrite:true handler key_kind ~max_extra_data:0 ~key:Config.signature_key ~data:signature

let read_subtable_signature handler key_kind ~subt = Operations.get handler key_kind ~key:Config.signature_key

let sign_table handler ~table_salt ~signwd =
  let signature = table_signature handler ~table_salt ~signwd in
  Operations.add ~may_overwrite:true handler table_key_kind ~max_extra_data:0 ~key:Config.signature_key ~data:signature

let read_table_signature handler = Operations.get handler table_key_kind ~key:Config.signature_key

let remove_table_signature handler =
  try Operations.remove handler table_key_kind Config.signature_key
  with Error (Unbound (_, _)) -> () (* Was not signed. *)

let remove_subtable_signature handler kind ~subt =
  try Operations.remove handler kind Config.signature_key
  with Error (Unbound (_, _)) -> () (* Was not signed. *)
