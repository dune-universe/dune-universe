open Ldap_mutex
open Ldap_ooclient
open Ldap_types

type txn = {
  mutable dead: bool;
  entries: (string, (ldapentry_t * ldapentry_t)) Hashtbl.t
}

exception Rollback of exn * ((ldapentry_t * ldapentry_t) list)
exception Txn_commit_failure of string * exn * ldapentry_t list option
exception Txn_rollback_failure of string * exn

class ldapadvisorytxcon
  ?(connect_timeout=1)
  ?(referral_policy=`RETURN)
  ?(version = 3)
  hosts binddn bindpw mutextbldn =
let copy_entry entry =
  let new_entry = new ldapentry in
    new_entry#set_dn (entry#dn);
    List.iter
      (fun attr -> new_entry#add [(attr, entry#get_value attr)])
      entry#attributes;
    new_entry
in
object (self)
  inherit ldapcon ~connect_timeout ~referral_policy ~version hosts as super
  initializer
    super#bind binddn ~cred:bindpw

  val lock_table = new object_lock_table hosts binddn bindpw mutextbldn

  method private check_dead txn =
    if txn.dead then
      raise
        (LDAP_Failure
           (`LOCAL_ERROR,
            "this transaction is dead, create a new one",
            {ext_matched_dn="";ext_referral=None}))

  method begin_txn = {dead=false;entries=Hashtbl.create 1}

  method associate_entry txn (entry: ldapentry_t) =
    self#check_dead txn;
    let dn = Ldap_dn.canonical_dn entry#dn in
      if Hashtbl.mem txn.entries dn then
        raise
          (LDAP_Failure
             (`LOCAL_ERROR,
              "dn: " ^ dn ^ " is already part of this transaction",
              {ext_matched_dn="";ext_referral=None}))
      else
        if entry#changes = [] then begin
          lock_table#lock (Ldap_dn.of_string dn);
          Hashtbl.add txn.entries dn ((copy_entry entry), (entry :> ldapentry_t))
        end else
          raise
            (LDAP_Failure
               (`LOCAL_ERROR,
                "this entry has been changed since it was downloaded " ^
                  "commit your current changes, and then add the entry to " ^
                  "this transaction",
                {ext_matched_dn="";ext_referral=None}))

  method associate_entries txn entries =
    List.iter (self#associate_entry txn) entries

  method disassociate_entry txn (entry: ldapentry_t) =
    self#check_dead txn;
    let dn = Ldap_dn.canonical_dn entry#dn in
      if Hashtbl.mem txn.entries dn then begin
        Hashtbl.remove txn.entries dn;
        lock_table#unlock (Ldap_dn.of_string dn);
      end else
        raise
          (LDAP_Failure
             (`LOCAL_ERROR,
              "dn: " ^ dn ^ " is not part of this transaction",
              {ext_matched_dn="";ext_referral=None}))

  method disassociate_entries txn entries =
    List.iter (self#disassociate_entry txn) entries

  method commit_txn txn =
    self#check_dead txn;
    txn.dead <- true;
    try
      List.iter
        (fun (_, e) -> lock_table#unlock (Ldap_dn.of_string e#dn))
        (Hashtbl.fold
           (fun _k (original_entry, modified_entry) successful_so_far ->
              try
                (match modified_entry#changetype with
                     `MODIFY -> super#update_entry modified_entry
                   | `ADD -> super#add modified_entry
                   | `DELETE -> super#delete modified_entry#dn
                   | `MODRDN ->
                       super#modrdn
                         original_entry#dn
                         (Ldap_dn.to_string
                            [(List.hd
                                (Ldap_dn.of_string modified_entry#dn))])
                   | `MODDN ->
                       let dn = Ldap_dn.of_string modified_entry#dn in
                         super#modrdn
                           original_entry#dn
                           (Ldap_dn.to_string [List.hd dn])
                           ~newsup:(Some (Ldap_dn.to_string (List.tl dn))));
                (original_entry, modified_entry) :: successful_so_far
              with exn ->
                raise (Rollback (exn, successful_so_far)))
           txn.entries
           [])
    with Rollback (exn, successful_so_far) ->
      (Hashtbl.iter (fun _k (_, e) -> e#flush_changes) txn.entries);
      (match
         ((Hashtbl.iter (* rollback everything in memory *)
             (fun _k (original_entry, modified_entry) ->
                match modified_entry#changetype with
                    `MODIFY -> modified_entry#modify (original_entry#diff modified_entry)
                  | `ADD -> ()
                  | `DELETE -> ()
                  | `MODRDN ->
                      if not (List.mem (original_entry, modified_entry) successful_so_far) then
                        modified_entry#set_dn original_entry#dn
                  | `MODDN ->
                      if not (List.mem (original_entry, modified_entry) successful_so_far) then
                        modified_entry#set_dn original_entry#dn)
             txn.entries);
          (List.fold_left (* rollback in the directory only what we commited *)
             (fun not_rolled_back (original_entry, modified_entry) ->
                try
                  (match modified_entry#changetype with
                       `MODIFY -> super#update_entry modified_entry
                     | `ADD -> super#delete modified_entry#dn
                     | `DELETE -> super#add modified_entry
                     | `MODRDN ->
                         super#modrdn
                           (modified_entry#dn)
                           (Ldap_dn.to_string
                              [List.hd (Ldap_dn.of_string original_entry#dn)])
                     | `MODDN ->
                         super#modrdn
                           (modified_entry#dn)
                           (Ldap_dn.to_string
                              [List.hd (Ldap_dn.of_string original_entry#dn)])
                           ~newsup:(Some
                                      (Ldap_dn.to_string
                                         (List.tl
                                            (Ldap_dn.of_string
                                               original_entry#dn)))));
                  not_rolled_back
                with _ -> modified_entry :: not_rolled_back)
             []
             successful_so_far))
       with
           [] ->
             Hashtbl.iter
               (fun _k (e, _) -> lock_table#unlock (Ldap_dn.of_string e#dn))
               txn.entries;
             (Hashtbl.iter (fun _k (_, e) -> e#flush_changes) txn.entries);
             raise (Txn_commit_failure ("rollback successful", exn, None))
         | not_rolled_back ->
             Hashtbl.iter
               (fun _k (e, _) -> lock_table#unlock (Ldap_dn.of_string e#dn))
               txn.entries;
             (Hashtbl.iter (fun _k (_, e) -> e#flush_changes) txn.entries);
             raise
               (Txn_commit_failure
                  ("rollback failed", exn,
                   Some not_rolled_back)))

  method rollback_txn txn =
    txn.dead <- true;
    Hashtbl.iter
      (fun _k (original_entry, modified_entry) ->
         try
           lock_table#unlock (Ldap_dn.of_string original_entry#dn);
           modified_entry#modify (original_entry#diff modified_entry);
           modified_entry#flush_changes
         with exn -> raise (Txn_rollback_failure ("rollback failed", exn)))
      txn.entries
end
