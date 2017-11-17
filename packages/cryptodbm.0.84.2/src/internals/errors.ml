type error_location =
  (* Concerns the subtable with the given name and number. *)
  | Subtable of string * int

  (* Concerns the main table. *)
  | Table

  (* Undetermined. *)
  | Any

type error =
  (* The file does not exist or is not readable. *)
  | File_not_found of string

  (* Overwriting an existing file is not allowed. *)
  | File_overwrite of string

  (* Append mode: the file does not exist, or is not readable, or is not writeable. *)
  | File_not_appendable of string

  (* Write mode: the given permission does not allow writing to the file. *)
  | File_not_writeable of string

  (* The file format is wrong. 
   * Bad_format (expected, found) *)
  | Bad_format of string * string

  (* A wrong password was given to open a subtable or the database itself. *)
  | Bad_password of error_location

  (* The signature found in the file or in a subtable is wrong. *)
  | Bad_signature of error_location

  (* No signature was found. *)
  | No_signature of error_location

  (* The table, or subtable, cannot be used because it is already closed. *)
  | Is_Closed of error_location

  (* This subtable cannot be opened because it has already been opened. *)
  | Is_Already_Open of error_location

  (* No subtable with the given name exists. 
   * This error occurs when trying to open a standard subtable with open_uncrypted_subtable,
   * or when trying to open an explicitly uncrypted subtable with open_subtable. *)
  | No_subtable of string

  (* A subtable with the given name already exists and cannot be overwritten. *)
  | Subtable_exists of string

  (* Too many subtables created. Current maximum is 2^14 - 1. *)
  | Subtable_overflow

  (* Trying to overwrite a key, while may_overwrite was false. *)
  | Overwrite of string * error_location

  (* Trying to read a key that is not bound. *)
  | Unbound of string * error_location

  (* Error when accessing the underlying database. *)
  | DB_Error of exn

  (* Corrupted file: it does not have the expected structure. (An error message is given). *)
  | Corrupted of error_location * string

  (* Error when trying to create the backup. *)
  | Backup_failure of exn

(* Constructors *)
let bad_password    l = Bad_password l
let bad_signature   l = Bad_signature l
let no_signature    l = No_signature l
let is_closed       l = Is_Closed l
let is_already_open l = Is_Already_Open l

exception Error of error

let raiserror e = raise (Error e)

let loc2s = function
  | Subtable (nm, nb) -> Printf.sprintf "Subtable #%d (%s)" nb nm
  | Table -> "database"
  | Any -> "Undetermined!"


let error2s = function
  | File_not_found file -> Printf.sprintf "File '%s' cannot be found" file
  | File_overwrite file -> Printf.sprintf "File '%s' cannot be overwritten" file
  | File_not_appendable file -> Printf.sprintf "File '%s' cannot be appended" file
  | File_not_writeable file -> Printf.sprintf "File '%s' cannot be written" file
  | Bad_format (fmt1, fmt2) -> Printf.sprintf "Expecting format %s, found format %s" fmt1 fmt2
  | Bad_password loc -> Printf.sprintf "Bad password in %s" (loc2s loc)
  | Bad_signature loc -> Printf.sprintf "Bad signature in %s" (loc2s loc)
  | No_signature loc -> Printf.sprintf "No signature in %s" (loc2s loc)
  | Is_Closed loc -> Printf.sprintf "The %s is closed." (loc2s loc)
  | Is_Already_Open loc -> Printf.sprintf "The %s is already open." (loc2s loc)
  | No_subtable sub -> Printf.sprintf "No subtable named '%s' exists." sub
  | Subtable_exists sub -> Printf.sprintf "Subtable %s already exists, it cannot be overwritten." sub
  | Overwrite (key, loc) -> Printf.sprintf "Trying to overwrite key '%s' in %s." key (loc2s loc)
  | Unbound (key, loc) -> Printf.sprintf "Key '%s' does not exist in %s." key (loc2s loc)
  | DB_Error exn -> Printf.sprintf "Error while accessing the database: %s." (Printexc.to_string exn)
  | Corrupted (loc, msg) -> Printf.sprintf "%s is corrupted: %s" (loc2s loc) msg
  | Subtable_overflow -> "Too many subtables in this table."
  | Backup_failure exn -> Printf.sprintf "Error while creating the backup: %s." (Printexc.to_string exn)

let () = Printexc.register_printer
    begin function
      | Error err -> Some ("Cryptodbm.Error: " ^ error2s err)
      | _ -> None
    end


