
(* The database contains several kinds of strings:
 *   keys (among which we distinguish several kinds of keys)
 *   data (encrypted or uncrypted data) 
 *
 * Encrypted keys and data may be padded, that is, their length is artificially made longer
 * in order to hide their real length.
 * This is different from encryption padding, which just ensures that the encrypted-string length is a multiple of (say) 16.
 *
 * The main table may have both builtin uncrypted bindings and builtin encrypted bindings.
 * A given subtable may have only uncrypted bindings or encrypted bindings using a common subtable password.
 *
 * *)

(* Subtable numbers must be in 0..max_subtable. Currently 2^14 - 1. *)
val max_subtable : int

(* Keys may have different logical 'locations' in the table. *)
type location =
  (* Builtin key for the whole table. *)
  | Table_Builtin

  (* Builtin key for a subtable. A subtable number must be >= 0. *)
  | Subtable_Builtin of int

  (* User key in a subtable. *)
  | Subtable_User of int

(* For debugging and for salting data passwords. *)
val loc2s : location -> string

(* For computing signatures *)
val loc2hash : location -> string

(* Keys (and data) may be stored in plain text or in encrypted form. *)
type 'a howstored =
  (* Not crypted. *)
  | Uncrypted

  (* Encrypted with the given information *)
  | Encrypted of 'a

(* Private type, used for internal optimization. *)
type hidden

(* Kinds of keys. *)
type key_kind = private
      { key_loc : location ;
	
	(* How the key is encrypted: table password, subtable password, and padding. 
	 * It cannot be the case that both passwords are empty (otherwise the key kind would just be 'Uncrypted'). *)
	key_how : (Cipher.passwd * Cipher.passwd * int) howstored ;
	
	(* Encrypting function, used internally as an optimization. *)
	cryptf : hidden }

(* Builds a key kind (builds the private field cryptf). *)
val mk_key : location -> (Cipher.passwd * Cipher.passwd * int) howstored -> key_kind

type data_kind = private
      (* password and padding. *)
      { data_how : (Cipher.passwd * int) howstored }

val mk_data : (Cipher.passwd * int) howstored -> data_kind

(* Strings in the database, once encoded. *)
type encoded_key
type encoded_data

val cmp_encoded_key : encoded_key -> encoded_key -> int

(*** Conversions ***)
val encode_key  : string -> key_kind  -> encoded_key
val encode_data : string -> data_kind -> encoded_data

(* Invariant: 
 * in order to prevent collisions, encode_key is guaranteed to be injective, that is,
 * assuming that all the keys of a given subtable use the same subtable password,
 *   (encode_key key1 kind1 = encode_key key2 kind2) => (key1 = key2 && location1 = location2 (padding may be different, though) )
 *
 * encode_key returns :
 *  uncrypted case => location ^ KEY ^ '0'
 *  encrypted case => table_encryption(location ^ char ^ subtable_encryption(location? ^ pad(KEY))) ^ lastchar
 *                    location? is not needed if the subtable password is empty
 *                    lastchar is '1' (no table encryption) or '2' (table encryption)
 *                    char is 'E' (subtable has its own encryption), 'T' (subtable does not have its own encryption).
 *
 * that is, the location is encoded using the table encryption, so that
 * every (encrypted) key location is known, although the subtable password is unknown.
 *
 * the location is inserted in the subtable_encryption as well, so that two equal keys in different subtables
 * with the same subtable password are encrypted differently.
 *
 *)

(* In contrast, there is no need for encode_data to be injective. *)

(* @raise Error (Bad_password Any) when something went wrong (most likely, a wrong password). *)
val decode_data : encoded_data -> data_kind -> string

(* Get as much information as possible about an encoded key. 
 * passwd is the table password, it may be empty, in which case only uncrypted keys will be recognized
 * (or all keys if there is no table password).
 * subt_pas provides a password (or empty string) for each subtable.
 * 
 * Returns None if nothing could be guessed about this key (which means the key is encrypted with a table password, which was not given).
 * Returns Some (kind, None) if the subtable has its own password, which is unknown.
 * Returns Some (kind, Some key) if the key could be totally decrypted.
 * 
 * Note that the cryptf field of key_kind is left uninitialized, as well as padding information.
 * *)
val get_key_info : Cipher.passwd -> subt_pas:(int -> Cipher.passwd) -> encoded_key -> (key_kind * (string option)) option


(* Used to compute signatures *)
val sign_encoded_key : Cipher.passwd -> encoded_key -> string
val sign_encoded_data : Cipher.passwd -> encoded_data -> string

(*** Operations on the lower-level database. ***)


(* All operations may raise DB_Errors. *)
module LowerDB :
  sig
    (* Depending on which part of the gdbm API is used, 
     * either one file is created, or two files '.dir' and '.pag'. *)

    (* File(s) containing the database, encapsulated in an abstract type. *)
    type dbfile

    (* Create an abstract dbfile. The argument is the filename without suffixes. *)
    val mk_file : string -> dbfile

    (* Reverse function *)
    val get_root : dbfile -> string

    (* Useful functions over dbfiles. 
     * @raise FileUtil errors *)

    (* At least one file exist. *)
    val exists : dbfile -> bool

    (* Remove all the files *)
    val delete : dbfile -> unit

    (* Get the permissions of the files (corresponding to the permissions that were given at creation time). *)
    val get_perm : dbfile -> int

    (* Make a copy with the given new (root) name. *)
    val copy : dbfile -> string -> unit

    val is_readable : dbfile -> bool
    val is_appendable : dbfile -> bool

    (* DBM handler *)
    type dbm

    val open_dbm : dbfile -> [`Read | `Write | `Append] -> perm:int -> dbm
    val close : dbm -> unit
	
    (* @raise Not_found *)
    val find    : dbm -> encoded_key -> encoded_data

    (* @raise Not_found *)
    val remove  : dbm -> encoded_key -> unit

    val replace : dbm -> encoded_key -> encoded_data -> unit


    (* @raise Error (Overwrite Any) if the key is already bound. *)
    val add     : dbm -> encoded_key -> encoded_data -> unit
	
    val iterkey : (encoded_key -> unit) -> dbm -> unit
	
    val iter    : (encoded_key -> encoded_data -> unit) -> dbm -> unit
  end



(* Only for very specific low-level purposes. *)
val encodedkey2s : encoded_key -> string
val encodeddata2s : encoded_data -> string
val s2encodedkey : string -> encoded_key
val s2encodeddata : string -> encoded_data

