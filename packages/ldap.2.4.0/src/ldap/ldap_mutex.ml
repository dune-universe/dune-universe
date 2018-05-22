open Ldap_ooclient
open Ldap_types

(* ldap mutexes *)
exception Ldap_mutex of string * exn

class type mutex_t =
object
  method lock: unit
  method unlock: unit
end

class type object_lock_table_t =
object
  method lock: dn -> unit
  method unlock: dn -> unit
end

let addmutex ldap mutexdn =
  let mt = new ldapentry in
  let mtrdn = List.hd (Ldap_dn.of_string mutexdn) in
    mt#set_dn mutexdn;



    mt#add [("objectclass", ["top";"mutex"]);
            (mtrdn.attr_type, mtrdn.attr_vals)];
    try ldap#add mt
    with exn -> raise (Ldap_mutex ("addmutex", exn))

let rec lock (ldap:ldapcon) mutexdn lockval =
  try
    let obj =
      try
        ldap#search
          ~base:mutexdn
          ~scope:`BASE
          "objectclass=*"
      with LDAP_Failure (`NO_SUCH_OBJECT, _, _) -> []
    in
      if List.length obj = 0 then begin
        addmutex ldap mutexdn;
        lock ldap mutexdn lockval
      end
      else if List.length obj = 1 then
        while true
        do
          try
            ldap#modify (List.hd obj)#dn lockval;
            failwith "locked"
          with (* the mutex is locked already *)
              LDAP_Failure (`TYPE_OR_VALUE_EXISTS, _, _)
            | LDAP_Failure (`OBJECT_CLASS_VIOLATION, _, _) ->
                (* this is so evil *)
                ignore (Unix.select [] [] [] 0.25) (* wait 1/4 of a second *)
        done
      else failwith "huge error, multiple objects with the same dn"
  with
      Failure "locked" -> ()
    | (Ldap_mutex _) as exn -> raise exn
    | exn -> raise (Ldap_mutex ("lock", exn))

let rec unlock (ldap:ldapcon) mutexdn unlockval =
  try
    let obj =
      try
        ldap#search
          ~base:mutexdn
          ~scope:`BASE
          "objectclass=*"
      with LDAP_Failure (`NO_SUCH_OBJECT, _, _) -> []
    in
      if List.length obj = 0 then begin
        addmutex ldap mutexdn;
        unlock ldap mutexdn unlockval
      end
      else if List.length obj = 1 then
        try
          ldap#modify
            (List.hd obj)#dn unlockval
        with LDAP_Failure (`NO_SUCH_ATTRIBUTE, _, _) -> ()
  with
      (Ldap_mutex _) as exn -> raise exn
    | exn -> raise (Ldap_mutex ("unlock", exn))


class mutex ldapurls binddn bindpw mutexdn =
object (self)
  val ldap =
    let ldap = new ldapcon ldapurls in
      ldap#bind binddn ~cred:bindpw;
      ldap

  method private addmutex = addmutex ldap mutexdn
  method lock = lock ldap mutexdn [(`ADD, "mutexlocked", ["locked"])]
  method unlock = unlock ldap mutexdn [(`DELETE, "mutexlocked", [])]
end

let apply_with_mutex mutex f =
  mutex#lock;
  try
    let result = f () in
      mutex#unlock;
      result
  with exn -> (try mutex#unlock with _ -> ());raise exn

class object_lock_table ldapurls binddn bindpw mutextbldn =
object (self)
  val ldap =
    let ldap = new ldapcon ldapurls in
      ldap#bind binddn ~cred:bindpw;
      ldap
  method private addmutex = addmutex ldap mutextbldn
  method lock dn = lock ldap mutextbldn [(`ADD, "lockedObject", [Ldap_dn.to_string dn])]
  method unlock dn = unlock ldap mutextbldn [(`DELETE, "lockedObject", [Ldap_dn.to_string dn])]
end
