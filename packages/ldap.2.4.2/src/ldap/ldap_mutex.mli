(** functions for implementing mutexes on top of LDAP's built in test
    and set mechanism. In order to use this module you must load
    mutex.schema, which is an rfc2252 format schema file.  raised when
    a mutex operation fails. The string argument contains the name of
    the method which failed, and the exception contains details about
    what failed. *)
exception Ldap_mutex of string * exn

(** the class type of a single mutex, used for performing
    advisory locking of some action *)
class type mutex_t =
object
  method lock: unit
  method unlock: unit
end

(** the class type of an object lock table which allows for advisory
    locking of objects by dn *)
class type object_lock_table_t =
object
  method lock: Ldap_types.dn -> unit
  method unlock: Ldap_types.dn -> unit
end

(**  new mutex ldapurls binddn bindpw mutexdn *)
class mutex: string list -> string -> string -> string ->
object
  (** lock the mutex. This WILL block if the mutex is already locked *)
  method lock: unit

  (** unlock the mutex *)
  method unlock: unit
end

(** used to apply some function, first locking the mutex, unlocking it
    only after the function has been applied. If the function
    generates any exception, this wrapper catches that exception, and
    unlocks the mutex before reraising the exception. Generally
    garentees that the mutex will always be used consistantly when
    performing an action. *)
val apply_with_mutex: mutex -> (unit -> 'a) -> 'a

(** new object_lock_table ldapurls binddn bindpw mutexdn *)
class object_lock_table: string list -> string -> string -> string ->
object
  (** lock the specified dn, if it is already locked, then block until the lock can be aquired *)
  method lock: Ldap_types.dn -> unit

  (** unlock the specified dn, if it is not locked do nothing *)
  method unlock: Ldap_types.dn -> unit
end
