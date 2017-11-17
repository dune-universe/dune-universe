(** 
   {2 Encrypted layer over the dbm library: serverless, key-value databases with symmetric encryption.}

   This library provides an encrypted layer on top of the {{:https://github.com/ocaml/dbm}Dbm} and {{:https://github.com/xavierleroy/cryptokit/}Cryptokit} packages. 
   The improvements over Dbm are:

   {ul
   {- A single database file may contain {b several independent subtables}, identified by a name (a string).}
   {- Each subtable can be {b signed and encrypted individually}, or encrypted using a global password.}
   {- The whole file can be signed.}
   {- {b Obfuscating data} is -optionally- appended to keys, data, and to the whole table, so that two databases with
    the same content look significantly different, once encrypted.}
   {- Encryption is symmetric: encryption and decryption both use the same password.}
   {- Signature is symmetric: signing and verifying the signature both use the same signword.}
   }

    As a quick example, the following uncrypted bindings (key => data):

   {v
        "john-doe"        => "age 36"
        "some secret"     => "The cake is a lie."
        "Motto"           => "For relaxing times, make it Suntory time" v}

    are stored as follows in the encrypted file (with variations depending on the password, and other parameters):

   {v 
 [S~j....O.Q..tk^.2] => [...F...).Hsl..tB]
 [...y;....~.:.6V.2] => [....I...JR..w.E9..G..q=...K....b]
 [..'.C...F.x.3K.y2] => [1.)9q..M...et.b.]
 [S.....5 Y....8..2] => [.D........2..u...q.......}Z.b..z.zo.}.l3l.....>.]
 [...xD;@.8..wV..P1....e}....u..`.2] => [hb..2.._B....Y?0....|.....tM....]
 [K.#i.7j..H.ZZ.^.2] => [..z....,........] v}

    Including several subtables in the same
    database file avoids having to deal with multiple files to store related information, 
    and also prevents information leak through the number and sizes of a set of database files.

    This library was primarily designed to store encrypted exam files on a university server. A common layout consists in
    several subtables encrypted with a global password, as well as an uncrypted subtable containing (public) meta-information.

    {b Install and compile}

    Install using opam: [opam install cryptodbm]

    Compile with ocamlbuild by adding the following to your _tags file: 
   {v <**/*> : package(cryptodbm) v}

    {b Performance}

    I have not benchmarked this library. 
    Keep in mind that every access (reading or writing a binding) requires to encrypt the key,
    and encrypt the data (when writing) or decrypt the data (when reading).
    Don't be pessimistic, though: it seems all right for non critical applications.

    Also, there is only a global key-index for the whole table, no key-index for individual subtables. As a consequence,
    subtable iterators actually iterate over the whole table index (selecting only the expected subtable indexes).

    {b A few technical details}

    When a database file is encrypted, 

    {ul    
    {- The only uncrypted binding in the file is the format version number, as well as bindings of uncrypted subtables.}
    {- Because of salt(s), it is not possible for an observer to see when two databases use the same password. }
    {- Because of salt(s), saving twice the same database leads to two files containing totally different bindings.
       An observer cannot make sure that two databases are equal. However, if the following options are not used, the
       total number of bindings is the same, as well as the sizes of bindings (which entails that the file sizes are the same too).}
    {- If "max_extra padding" is used, saving twice the same database leads to two files containing bindings with different sizes.
       If "extra bindings" is used, the total number of bindings is, moreover, different.
       It is harder for an observer to guess if two database files are possibly equal.}
    {- Because data is encrypted using the salt, password and key, data associated to different keys are encrypted differently.
       An observer cannot identify bindings with equal data.}
    {- The signature is computed by hashing the salt, signword, all (subtabled) keys and associated data. It is stored after
       encryption using the salt and the (sub)table password. The hashing function is currently sha256 (see the Cryptokit package).}
    }

    See also the {{:https://github.com/lebotlan/ocaml-cryptodbm} project homepage}.

    {i Contact: D. Le Botlan (github.lebotlan\@dfgh.met where you replace .met by .net.) }

*)

(** 
   {3 Typical example}

   {[
     let table = open_append ~file:"/path/to/myfile" ~passwd:"my-secret-passwd" in

     let subtable = append_subtable table ~name:"here the subtable name" () in

     add subtable ~key:"key1" ~data:"data1" () ;
     add subtable ~key:"key2" ~data:"data2" () ;

     close table ;
     ()
   ]}
*)

(** All errors that may occur. *)
module Error :
sig
  (** Location where an error occured. *)
  type error_location =
    | Subtable of string * int
    (** The error occured in the indicated subtable, with the given name (and number). *)

    | Table
    (** The error occured in the main table. *)

    | Any
    (** The location is undetermined. *)

  type error =
    | File_not_found of string
    (** The file does not exist or is not readable. *)

    | File_overwrite of string
    (** Overwriting an existing file is not allowed. *)

    | File_not_appendable of string
    (** Append mode: the file does not exist, or is not readable, or is not writeable. *)

    | File_not_writeable of string
    (** Write mode: the given permission does not allow writing to the file. *)

    | Bad_format of string * string
    (** The file format is wrong: Bad_format (expected, found) *)

    | Bad_password of error_location
    (** A wrong password was given to open a subtable or the database itself. *)

    | Bad_signature of error_location
    (** The signature found in the file or in a subtable is wrong. *)

    | No_signature of error_location
    (** No signature was found. *)

    | Is_Closed of error_location
    (** The table, or subtable, cannot be used because it is already closed. *)

    | Is_Already_Open of error_location
    (** This subtable cannot be opened because it has already been opened. *)

    | No_subtable of string
    (** No subtable with the given name exists. 
        This error occurs when trying to open a standard subtable with open_uncrypted_subtable,
        or when trying to open an explicitly uncrypted subtable with open_subtable. *)

    | Subtable_exists of string
    (** A subtable with the given name already exists. *)

    | Subtable_overflow
    (** Too many subtables created. Current maximum is 2^14 - 1. *)

    | Overwrite of string * error_location
    (** Trying to overwrite a key, while may_overwrite was false. *)

    | Unbound of string * error_location
    (** Trying to read a key that is not bound. *)

    | DB_Error of exn
    (** Error when accessing the underlying database. *)

    | Corrupted of error_location * string
    (** Corrupted file: it does not have the expected structure. (An error message is given). *)

    | Backup_failure of exn
    (** Error when trying to create the backup. *)

  (** Exception raised when an error occurs. *)
  exception Error of error

  (** Printing functions. *)
  val loc2s : error_location -> string
  val error2s : error -> string

end

(** Notice that all functions may raise [Error(DB_Error)] when accessing the underlying database. *)

(** {3 Basics} *)

(** The type of encrypted-dbm file descriptors. 
    'a is a phantom type precising the permission: read-only or full access. *)
type 'a table

(** Phantom type which represents read-only permission. *)
type read

(** Phantom type which represents read-write permission. *)
type full

(** Type of a subtable. 'a is the permission. *)
type 'a subtable

(** {3 Open, close, and flush files} *)

(** The database can be opened in three modes: 
    read mode, write (create) mode, and append mode.

    Note that operations are not thread-safe at the library level: do not share a table or subtable handler between threads.
    However, multiple processes might access the same database, whenever the low-level dbm permits it
    (which depends on the low-level dbm library actually used). 
    gdbm allows many readers in parallel, or only one writer and no reader. *)

(** {4 open_read} *)

(** Opens an encrypted-dbm file for reading.
    @param file The full path to the database file, but without the .pag or .dir extension (when applicable).
    @param passwd Use the given password to decrypt. Use the empty string "" if the file is not encrypted.
                  In order to access only uncrypted bindings of an encrypted file, consider {!open_only_uncrypted} instead.
    @param signwd Use the given signword to check the signature. If the signword is the empty string, do not check the signature.
    @param iterations  Number of iterations used to map the password/signword. A higher value means a longer time to open the database.
                       A default value is used if missing (around 12000).
                       The same value must be used when reading/writing the database - otherwise the password is not recognized (Bad_password).
                       The same value must be used when signing/checking the signature - oterwise the signature is not recognized (Bad_signature).
                       The number of iterations is NOT saved in the dbfile.
    @return A new handler to access this database.
    @raise Error(File_not_found) the file does not exist or is not readable.
    @raise Error(Bad_format) the database file uses an incompatible format.
    @raise Error(Bad_password) the non-empty given password is incorrect.
    @raise Error(Bad_signature the database signature does not match the expected signature, using the given non-empty signword.
    @raise Error(No_signature) the database is not signed but a signword is provided.
    @raise Error(Corrupted) the database file does not have the expected structure.
*)
val open_read : ?iterations:int -> file:string -> passwd:string -> signwd:string -> unit -> read table

(** {4 open_append} *)

(** Opens an existing encrypted-dbm file in append mode.
    @param file The full path to the database file, without the .pag or .dir extension
    @param passwd Use the given password to decrypt.
    @param signwd Use the given signword to sign the database.
    @param check_signature Whether to check the existing signature before appending new data. If the table was not signed, raises No_signature.
    @param iterations  Number of iterations used to map the password/signword. (See open_read)
    @return A new handler to access this database.
    @raise Error(File_not_appendable) the file does not exists or is not readable, or is not writeable.
    @raise Error(Bad_format) the database file uses an incompatible format.
    @raise Error(Bad_password) the non-empty given password is incorrect.
    @raise Error(Bad_signature) the database signature does not match the expected signature, and check_signature is true.
    @raise Error(No_signature) the database is not signed, and check_signature is true.
    @raise Error(Corrupted) the database file does not have the expected structure.
    @raise Failure(some message) check_signature is true but the signwd is empty.
*)
val open_append : ?iterations:int -> file:string -> passwd:string -> signwd:string -> check_signature:bool -> unit -> full table

(** {4 open_create} *)

(** Creates a new encrypted-dbm file. 
    @param file The full path to the database file, without the .pag or .dir extension
    @param overwrite Indicates if overwriting an existing file is allowed.
    @param passwd If a password is provided, use it to encrypt. An empty password means no encryption.
    @param signwd If a signword is provided, use it to sign. An empty signword means no signature.
    @param iterations  Number of iterations used to map the password/signword. (See open_read)
    @param max_extra_key Max length of random padding added to keys, default 0.
    @param max_extra_data Max length of random padding added to data, default 0. 
    @param max_extra_bindings (default 0): because the number of bindings of the table can be guessed without knowing the password,
                         random extra bindings can be added to obfuscate the table.
    @param perm Unix permission to be used to create the file. Must allow the user to write on this file. Beware that in OCaml, 644 is not equal to 0o644.
    @return A new handler to access this database.
    @raise Error(File_overwrite) the file already exists and overwriting is not explicitly allowed.
    @raise Error(File_not_writeable) the given permission does not allow writing.
*)
val open_create : 
  file:string -> ?overwrite:bool -> ?iterations:int -> passwd:string -> signwd:string ->
  ?max_extra_key:int -> ?max_extra_data:int -> ?max_extra_bindings:int ->
  perm:int -> unit -> full table

(** {4 open_only_uncrypted} *)

(** Opens a table to access only uncrypted subtables.
    @param file The full path to the database file, without the .pag or .dir extension
    @param signwd Use the given signword to check the signature. If the signword is the empty string, do not check the signature.
    @param iterations Used to check the signature (useless if the signwd is empty).
    @raise Error(File_not_found) the file does not exist or is not readable.
    @raise Error(Bad_format) the database file uses an incompatible format.
    @raise Error(Bad_signature) the database signature does not match the expected signature, using the given non-empty signword.
    @raise Error(No_signature) the database is not signed but a signword is provided.
    @raise Error(Corrupted) the database file does not have the expected structure.
*)
val open_only_uncrypted : ?iterations:int -> file:string -> signwd:string -> unit -> read table

(** {4 close, flush} *)

(** If in write mode, sign if necessary, add extra bindings if required, then flush and close the file. 
    In read mode, just close the file. 
    All the subtables are automatically closed.
    @raise Error(Is_Closed) the database is already closed. *)
val close : 'a table -> unit

(** Sign if necessary, and flush. 
    Optionally make a backup, that is, a copy of the current database file is made (default name is: 'filename'-backup-'date').
    @raise Error(Is_Closed) the database is already closed.
    @raise Error(Backup_failure) something went wrong when doing the backup.
*)
val flush : ?backup:bool -> ?backup_name: string -> full table -> unit

(** Returns the root filename (without the .pag or .dir extension). *)
val get_rootfile : 'a table -> string

(** {3 Open and create subtables} *)

(** {4 create_subtable} *)

(** Creates a standard subtable for writing.
    @param name the subtable name. It can be any string.
    @param passwd If the passwd is empty, use the global table password. If the global table password is also empty, this subtable will be uncrypted.
                  If the passwd is not empty, use it to encrypt this subtable.
                  Consider {!create_uncrypted_subtable} to create an uncrypted subtable in an encrypted table.
    @param signwd If a signwd is provided, use it to sign this subtable.
    @param iterations  Number of iterations used to map the password/signword. (See open_read). If unspecified, take a default value (not necessarily the value used to open the table itself).
    @param max_extra_key Use this max_extra_key (instead of the global table max_extra_key parameter).
    @param max_extra_data Use this max_extra_data (instead of the global table max_extra_data parameter).
    @raise Error(Is_Closed) the database is closed.
    @raise Error(Subtable_exists) a standard subtable with this name already exists.
*)
val create_subtable : full table -> name:string -> ?iterations:int -> passwd:string -> 
  signwd:string -> ?max_extra_key:int -> ?max_extra_data:int -> unit -> full subtable

(** Creates an uncrypted subtable (even when the global table is encrypted).
    @param name the subtable name. It can be virtually any string.
    @param signwd If a signwd is provided, use it to sign this subtable.
    @param iterations Used to check the signature (useless if the signwd is empty).
    @raise Error(Is_Closed) the database is closed.
    @raise Error(Subtable_exists) a subtable with this name already exists.
*)
val create_uncrypted_subtable : ?iterations:int -> full table -> name:string -> signwd:string -> unit -> full subtable


(** {4 open_subtable} *)

(** Open a standard subtable for reading. 
    @param name the subtable name.
    @param passwd If a non-empty password is provided, use it to decrypt.
    @param signwd If a non-empty signword is provided, use it to check the signature of this subtable.
    @param iterations  Number of iterations used to map the password/signword. (See open_read)
    @raise Error(Is_Closed) the database is closed.
    @raise Error(Bad_password) the given password does not match this subtable's password.
    @raise Error(Bad_signature) the subtable signature does not match the expected signature, using the given non-empty signword.
    @raise Error(No_signature) the subtable is not signed, but a signwd was provided.
    @raise Error(Is_Already_Open) this subtable (identified by its name) is already open.
    @raise Error(No_subtable) no subtable with this name exists (if the subtable is explicitly uncrypted, use {!open_uncrypted_subtable} instead).
*)
val open_subtable : 'a table -> name:string -> ?iterations:int -> passwd:string -> signwd:string -> unit -> read subtable

(** Open an uncrypted subtable for reading. 
    @param name the subtable name.
    @param signwd If a non-empty signword is provided, use it to check the signature of this subtable.
    @param iterations Used to check the signature (useless if the signwd is empty).
    @raise Error(Is_Closed) the database is closed.
    @raise Error(Bad_signature) the subtable signature does not match the expected signature, using the given non-empty signword.
    @raise Error(No_signature) the subtable is not signed, but a signwd was provided.
    @raise Error(Is_Already_Open) this subtable (identified by its name) is already open.
    @raise Error(No_subtable) no uncrypted subtable with this name exists.
*)
val open_uncrypted_subtable : ?iterations:int -> 'a table -> name:string -> signwd:string -> unit -> read subtable

(** {4 append_subtable} *)

(** Open a standard subtable for appending bindings.
    @param name the subtable name.
    @param passwd If a non-empty password is provided, use it to decrypt and encrypt.
    @param signwd Use the given signword to sign this subtable.
    @param check_signature Whether to check the existing signature before appending new data. If the subtable was not signed, this parameter has no effect.
    @param iterations  Number of iterations used to map the password/signword. (See open_read)
    @raise Error(Is_Closed) the database is closed.
    @raise Error(Bad_password) the given password does not match this subtable's password.
    @raise Error(Bad_signature) the subtable signature does not match the expected signature, using the given non-empty signword.
    @raise Error(No_signature) the subtable is not signed, but a signwd was provided.
    @raise Error(Is_Already_Open) this subtable (identified by its name) is already open.
    @raise Error(No_subtable) no standard subtable with this name exists (if the subtable is explicitly uncrypted, use {!append_uncrypted_subtable} instead).    
*)
val append_subtable : full table -> name:string -> ?iterations:int -> passwd:string ->
  signwd:string -> check_signature:bool -> unit -> full subtable

(** Open an uncrypted subtable for appending bindings.
    @param name the subtable name.
    @param signwd Use the given signword to sign this subtable.
    @param check_signature Whether to check the existing signature before appending new data. If the subtable was not signed, this parameter has no effect.
    @param iterations Used to check the signature (useless if the signwd is empty).
    @raise Error(Is_Closed) the database is closed.
    @raise Error(Bad_signature) the subtable signature does not match the expected signature, using the given non-empty signword.
    @raise Error(No_signature) the subtable is not signed, but a signwd was provided.
    @raise Error(Is_Already_Open) this subtable (identified by its name) is already open.
    @raise Error(No_subtable) no uncrypted subtable with this name exists.
*)
val append_uncrypted_subtable : ?iterations:int -> full table -> name:string -> signwd:string -> check_signature:bool -> unit -> full subtable

(** {4 close} *)

(** If in write mode, sign if necessary, then flush and close the subtable.
    In read mode, just close the subtable. 
    @raise Error(Is_Closed) the subtable is already closed.
*)
val close_subtable : 'a subtable -> unit


(** {3 Getters and iterators over subtables} *)

(** Returns this subtable's identifier (a number). *)
val get_number : 'a subtable -> int

(** Returns this subtable's name. *)
val get_name   : 'a subtable -> string

(** Iterate over standard subtables. The function is applied to the subtable name and number.
    @raise Error(Is_Closed) the table is already closed.
*)
val iter_subtables : 'a table -> (string -> int -> unit) -> unit

(** Iterate over uncrypted subtables. 
    @raise Error(Is_Closed) the table is already closed.
*)
val iter_uncrypted_subtables : 'a table -> (string -> int -> unit) -> unit

(** {3 Operations on bindings: add, find, delete, iterate} *)

(** [add subt key data] binds [key] to [data] in subtable [subt].
    By default, overwriting an existing binding is forbidden.
    @param may_overwrite if true, replacing an old binding by a new binding is permitted.
    @raise Error(Overwrite) the key is already bound and [may_overwrite] is false.
    @raise Error(Is_Closed) the subtable is already closed.
*)
val add : ?may_overwrite:bool -> full subtable -> key:string -> data:string -> unit

(** [find subt key] returns the data associated to [key] in subtable [subt].
    @raise Error(Unbound) the key is not bound in this subtable.
    @raise Error(Is_Closed) the subtable is already closed.
*)
val find : 'a subtable -> string -> string

(** [delete subt key] removes the binding associated to [key] in subtable [subt].
    @raise Error(Unbound) the key is not bound in this subtable.
    @raise Error(Is_Closed) the subtable is already closed.
*)
val delete : full subtable -> string -> unit

(** [clear subt] removes all the bindings in subtable [subt].
    @raise Error(Is_Closed) the subtable is already closed.
*)
val clear: full subtable -> unit

(** Iterate over all pairs (key, data) of the given subtable. 
    @raise Error(Is_Closed) the subtable is already closed.
*)
val iter : 'a subtable -> (string -> string -> unit) -> unit

(** Iterate over all keys of the given subtable. Faster than [iter] since data is not decrypted. 
    @raise Error(Is_Closed) the subtable is already closed. *)
val iterkey : 'a subtable -> (string -> unit) -> unit

(** See iter. *)
val fold : 'a subtable -> 'b -> (string -> string -> 'b -> 'b) -> 'b

(** For debugging. *)
val iter_extra_bindings : 'a table -> (string -> string -> unit) -> unit

(** {3 Durable backup} *)

(** The underlying database file is managed by the dbm library actually installed on the runtime platform. 
    For portability, or to ensure that your data does not depend on a particular version of the dbm library, 
    you can use these functions which convert dbm files to and from an ad-hoc, very simple binary format.
*)

(** Exports a dbfile to a durable binary format. 
    The dbfile must be opened for reading (open_only_uncrypted suffices, in case the passwords are not known). 
    Note that the binary file just mirrors the dbfile (that is, they both contain the same encrypted & uncrypted data). *)
val export : read table -> binfile:string -> unit

(** Imports a binary file to a dbfile. 
    [dbfile] is the output dbfile, without .pag or .dir extension *)
val import : binfile:string -> dbfile:string -> unit

(* Only for debug *)
(* val autotest : full table -> unit *)
