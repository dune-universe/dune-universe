(* an object oriented interface to ldap

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)

(** an object oriented ldap client interface *)

open Ldap_types

(** {2 Basic Data Types} *)

(** the type of an operation, eg. [("cn", ["foo";"bar"])] *)
type op = string * string list
type op_lst = op list

(** The policy the client should take when it encounteres a
    referral. This is currently not used *)
type referral_policy = [ `FOLLOW | `RETURN ]

(** The change type of an ldapentry. This controls some aspects of
    it's behavior *)
type changetype = [ `ADD | `DELETE | `MODDN | `MODIFY | `MODRDN ]

(** {2 Local Representation of LDAP Objects} *)

(** The base type of an ldap entry represented in memory. *)
class type ldapentry_t =
object
  method add : op_lst -> unit
  method attributes : string list
  method changes : (Ldap_types.modify_optype * string * string list) list
  method changetype : changetype
  method delete : op_lst -> unit
  method dn : string
  method diff : ldapentry_t -> (modify_optype * string * string list) list
  method exists : string -> bool
  method flush_changes : unit
  method get_value : string -> string list
  method modify :
    (Ldap_types.modify_optype * string * string list) list -> unit
  method print : unit
  method replace : op_lst -> unit
  method set_changetype : changetype -> unit
  method set_dn : string -> unit
end

(** this object represents a remote object within local memory. It
    records all local changes made to it (if it's changetype is set to
    `MODIFY), and can commit them to the server at a later time via
    {!Ldap_ooclient.ldapcon.update_entry}. *)
class ldapentry :
object
  (** add values to an attribute (or create a new attribute). Does
      not change the server until you update *)
    method add : op_lst -> unit

    (** return a list of the type (name) of all the attributes present
    on the object *)
    method attributes : string list

    (** return a list of changes made to the object in a the format of
        a modify operation. For example, you can apply the changes to another
        ldapentry object using the {!Ldap_ooclient.ldapentry.modify}
        method *)
    method changes : (Ldap_types.modify_optype * string * string list) list

    (** return the changetype of the object *)
    method changetype : changetype

    (** delete attributes from the object, does not change the
    directory until you update *)
    method delete : op_lst -> unit

    (** return the dn of the object *)
    method dn : string

    (** given an ldapentry, return the differences between the current
        entry and the specified entry in the form of a modify
        operation which would make the specified entry the same as the
        current entry. *)
    method diff : ldapentry_t -> (modify_optype * string * string list) list

    (** query whether the attribute type (name) exists in the object *)
    method exists : string -> bool

    (** clear all accumulated changes *)
    method flush_changes : unit

    (** get the value of an attribute @raise Not_found If the
        attribute does not exist. *)
    method get_value : string -> string list

    (** Apply modifications to object in memory, does not change the
        database until you update using
        {!Ldap_ooclient.ldapcon.update_entry} *)
    method modify :
      (Ldap_types.modify_optype * string * string list) list -> unit

    (** @deprecated print an ldif like representation of the object to stdout, see
        Ldif_oo for standards compliant ldif. Usefull for toplevel
        sessions. *)
    method print : unit

    (** replace values in the object, does not change the database
    until you call update *)
    method replace : op_lst -> unit

    (** set the changetype of the object *)
    method set_changetype : changetype -> unit

    (** set the dn of the object *)
    method set_dn : string -> unit
  end

(** {1 Miscallaneous} *)

(** toplevel formatter for ldapentry, prints the whole entry with a
    nice structure. Each attribute is in the correct syntax to be
    copied and pasted into a modify operation. *)
val format_entry :
  < attributes : string list; dn : string;
 get_value : string -> string list; .. > ->
   unit

(** format lists of entries, in this case only print the dn *)
val format_entries :
  < attributes : string list; dn : string;
 get_value : string -> string list; .. > list ->
   unit

(** The type of an ldap change record, used by extended LDIF *)
type changerec =
    [`Modification of string * ((Ldap_types.modify_optype * string * string list) list)
    | `Addition of ldapentry
    | `Delete of string
    | `Modrdn of string * int * string]

(** {0 Communication With {!Ldap_funclient}} *)

(** given a search_result_entry as returned by ldap_funclient, produce an
    ldapentry containing either the entry, or the referral object *)
val to_entry :
  [< `Entry of Ldap_types.search_result_entry | `Referral of string list ]
  -> ldapentry

(** given an ldapentry as returned by ldapcon, or constructed manually,
    produce a search_result_entry suitable for ldap_funclient, or
    ldap_funserver. *)
val of_entry : ldapentry -> search_result_entry

(** {2 Interacting with LDAP Servers} *)

(** This class abstracts a connection to an LDAP server (or servers),
    an instance will be connected to the server you specify and can be
    used to perform operations on that server.

    {0 Example}

    [new ldapcon ~connect_timeout:5 ~version:3
    ["ldap://first.ldap.server";"ldap://second.ldap.server"]].

    In addition to specifying multiple urls, if DNS names are given,
    and those names are bound to multiple addresses, then all possible
    addresses will be tried.

    {0 Example}

    [new ldapcon ["ldaps://rrldap.csun.edu"]]

    is equivelant to

    [new ldapcon ["ldap://130.166.1.30";"ldap://130.166.1.31";"ldap://130.166.1.32"]]

    This means that if any host in the rr fails, the ldapcon will
    transparently move on to the next host, and you will never know
    the difference.

    @raise LDAP_Failure All methods raise {!Ldap_types.LDAP_Failure} on error

    @param connect_timeout Default [1], an integer which specifies how
    long to wait for any given server in the list to respond before
    trying the next one. After all the servers have been tried for
    [connect_timeout] seconds [LDAP_Failure (`SERVER_DOWN, ...)]  will
    be raised.

    @param referral_policy In a future version of ocamldap this will
    be used to specify what you would like to do in the event of a
    referral. Currently it does nothing and is ignored see
    {!Ldap_ooclient.referral_policy}.

    @param version The protocol version to use, the default is [3],
    the other recognized value is [2].
*)
class ldapcon :
  ?connect_timeout:int ->
  ?referral_policy:[> `RETURN ] ->
  ?version:int ->
  string list ->
object
  (** {2 Authentication} *)

  (** bind to the database using dn.

      {0 Simple Bind Example}

      [ldap#bind ~cred:"password" "cn=foo,ou=people,ou=auth,o=bar"]

      To bind anonymously, omit ~cred, and leave dn blank eg.

      {0 Example}

      [ldap#bind ""]

      @param cred The credentials to provide for binding. Default [""].

      @param meth The method to use when binding See
      {!Ldap_funclient.authmethod} the default is [`SIMPLE]. If
      [`SASL] is used then [dn] and [~cred] Are interperted according
      to the chosen SASL mechanism. SASL binds have not been tested
      extensively. *)
  method bind :
    ?cred:string -> ?meth:Ldap_funclient.authmethod -> string -> unit

  (** Deauthenticate and close the connection to the server *)
  method unbind : unit

  (** {2 Searching} *)

  (** Search the directory syncronously for an entry which matches the
      search criteria.

      {0 Example}

      [ldap#search ~base:"dc=foo,dc=bar" ~attrs:["cn"] "uid=*"]

      @param scope Default [`SUBTREE], defines the scope of the
      search. see {!Ldap_types.search_scope}

      @param attrs Default [[]] (means all attributes)

      @param attrsonly Default [false] If true, asks the server to return
      only the attribute names, not their values.

      @param base Default [""], The search base, which is the dn of the
      object from which you want to start your search. Only that
      object, and it's children will be included in the
      search. Further controlled by [~scope].

      @param timelimit The time limit (in seconds) to allow the search
      to run for. Default [0l], which means there is no user specified
      time limit, the server may still impose one.

      @param sizelimit The max number of entries to return from the
      search (in number of entries) *)
  method search :
    ?scope:Ldap_types.search_scope ->
    ?attrs:string list ->
    ?attrsonly:bool -> ?base:string ->
    ?sizelimit:Int32.t -> ?timelimit:Int32.t ->
    string -> ldapentry list

  (** Search the directory asyncronously, otherwise the same as
      search. *)
  method search_a :
    ?scope:Ldap_types.search_scope ->
    ?attrs:string list ->
    ?attrsonly:bool -> ?base:string ->
    ?sizelimit:Int32.t -> ?timelimit:Int32.t ->
    string -> (?abandon:bool -> unit -> ldapentry)

  (** Fetch the raw (unparsed) schema from the directory using the
      standard mechanism (requires protocol version 3) *)
  method rawschema : ldapentry

  (** Fetch and parse the schema from the directory via the standard
      mechanism (requires version 3). Return a structured
      representation of the schema indexed by canonical name, and oid. *)
  method schema : Ldap_schemaparser.schema

  (** {2 Making Modifications} *)

  (** add an entry to the database *)
  method add : ldapentry -> unit

  (** Delete the object named by dn from the database *)
  method delete : string -> unit

  (** Modify the entry named by dn, applying mods

      {0 Example}

      [ldap#modify "uid=foo,ou=people,dc=bar,dc=baz" [(`DELETE, "cn", ["foo";"bar"])]]
  *)
  method modify :
    string ->
    (Ldap_types.modify_optype * string * string list) list -> unit

  (** Syncronize changes made locally to an ldapentry with the
      directory. *)
  method update_entry : ldapentry -> unit

  (** Modify the rdn of the object named by dn, if the protocol
      version is 3 you may additionally change the superior, the rdn
      will be changed to the attribute represented (as a string) by
      newrdn,

      {0 Example With New Superior}

      [ldap#modrdn ~newsup:(Some "o=csun") "cn=bob,ou=people,o=org" "uid=bperson"]

      After this example "cn=bob,ou=people,o=org" will end up as "uid=bperson,o=csun".

      @param deleteoldrdn Default [true], delete
      the old rdn value as part of the modrdn.

      @param newsup Default [None], only valid when the protocol
      version is 3, change the object's location in the tree, making
      its superior equal to the specified object. *)
  method modrdn : string -> ?deleteoldrdn:bool -> ?newsup:string option -> string -> unit
end

(** {1 Iterators Over Streams of ldapentry Objects} *)

(** given a source of ldapentry objects (unit -> ldapentry), such as
    the return value of ldapcon#search_a, apply f (first arg) to each entry
    See List.iter *)
val iter : (ldapentry -> unit) -> (?abandon:bool -> unit -> ldapentry) -> unit

(** given a source of ldapentry objects (unit -> ldapentry), such as
  the return value of ldapcon#search_a apply f (first arg) to each
  entry in reverse, and return a list containing the result of each
  application. See List.map *)
val rev_map : (ldapentry -> 'a) -> (?abandon:bool -> unit -> ldapentry) -> 'a list

(** same as rev_map, but does it in order *)
val map : (ldapentry -> 'a) -> (?abandon:bool -> unit -> ldapentry) -> 'a list

(** given a source of ldapentry objects (unit -> ldapentry), such as
  the return value of ldapcon#search_a compute (f eN ... (f e2 (f e1
  intial))) see List.fold_right. *)
val fold : (ldapentry -> 'a -> 'a) -> 'a -> (?abandon:bool -> unit -> ldapentry) -> 'a

(** {2 Schema Aware ldapentry Derivatives} *)

(** {1 General Schema Aware Entry} {!Ldap_ooclient.scldapentry}, A
    schema aware derivative of {!Ldap_ooclient.ldapentry}. It contains
    an rfc2252 schema checker, and given the database schema, it can
    be used to garentee that operations performed in memory are valid
    against a standards compliant database. It has numerious uses,
    translation between two databases with different schemas an
    example of where it finds natural usage. For an example
    application @see <http://tdir.sourceforge.net> tdir *)

(** an ordered oid type, for placing oids in sets *)
module OrdOid :
sig
  type t = Ldap_schemaparser.Oid.t
  val compare : t -> t -> int
end

(** A set of Oids @deprecated the name is historical, and may be changed *)
module Setstr :
sig
  type elt = OrdOid.t
  type t = Set.Make(OrdOid).t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val split : elt -> t -> t * bool * t
end

(** The type of schema checking to perform in
    {!Ldap_ooclient.scldapentry}. Normally this is picked
    automatically, however it can be overridden in some cases. *)
type scflavor =
    Optimistic
      (** Add missing attributes to make the object consistant, or add
          objectclasses in order to make illegal attribues legal *)
  | Pessimistic
      (** Delete objectclasses which must attributes which are
          missing, and delete illegal attributes. *)

(** given a name of an attribute name (canonical or otherwise), return
    its oid @raise Invalid_attribute If the attribute is not found in the schema. *)
val attrToOid :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Oid.t

(** given the oid of an attribute, return its canonical name @raise
    Invalid_attribute If the attribute is not found in the schema. *)
val oidToAttr : Ldap_schemaparser.schema -> Ldap_schemaparser.Oid.t -> string

(** given a name of an objectclass (canonical or otherwise), return
    its oid. @raise Invalid_objectclass If the objectclass is not
    found in the schema. *)
val ocToOid :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Oid.t

(** given the oid of an objectclass, return its canonical name @raise
    Invalid_objectclass If the objectclass is not found in the
    schema. *)
val oidToOc : Ldap_schemaparser.schema -> Ldap_schemaparser.Oid.t -> string

(** get an objectclass structure by one of its names (canonical or
    otherwise, however getting it by canonical name is currently much
    faster) @raise Invalid_objectclass If the objectclass is not found
    in the schema. *)
val getOc :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.objectclass

(** get an attr structure by one of its names (canonical or otherwise,
    however getting it by canonical name is currently much faster)
    @raise Invalid_attribute If the attribute is not found in the
    schema. *)
val getAttr :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.attribute

(** equate attributes by oid. This allows non canonical names to be
    handled correctly, for example "uid" and "userID" are actually the
    same attribute. @raise Invalid_attribute If either attribute is
    not found in the schema. *)
val equateAttrs :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Lcstring.t -> bool

exception Invalid_objectclass of string
exception Invalid_attribute of string
exception Single_value of string
exception Objectclass_is_required

class scldapentry :
  Ldap_schemaparser.schema ->
object
  (** {2 New Methods} *)

  (** Returns true if the attributed specified is allowed by the
      current set of objectclasses present on the entry. *)
  method is_allowed : string -> bool

  (** Returns true if the attribute specified is a must, but is not
      currently present. *)
  method is_missing : string -> bool

  (** Return a list of all attributes allowed on the entry (by oid) *)
  method list_allowed : Setstr.elt list

  (** Return a list of all missing attributes (by oid) *)
  method list_missing : Setstr.elt list

  (** Return a list of all present attributes. In contrast to the
      [attributes] method, this method ignores missing required
      attributes and just returns those attributes which are actually
      present. *)
  method list_present : Setstr.elt list

  (** Given an {!Ldap_ooclient.ldapentry} copy all of it's data into
      the current object, and perform a schema check.

      @param scflavor Default [Pessimistic] The schema checking
      bias, see {!Ldap_ooclient.scflavor} *)
  method of_entry : ?scflavor:scflavor -> ldapentry -> unit

  (** {2 Inherited Methods} *)

  (** Add values to the entry, just as
      {!Ldap_ooclient.ldapentry.add}, However, after the add is
      complete the schema checker is run in [Optimistic] mode. see
      {!Ldap_ooclient.scflavor} *)
  method add : op_lst -> unit

  (** Same as {!Ldap_ooclient.ldapentry.add}, except that the schema
      checker is run in [Pessimistic] mode after the operation is
      complete. see {!Ldap_ooclient.scflavor} *)
  method delete : op_lst -> unit

  (** Same as {!Ldap_ooclient.ldapentry.replace} except that once
      the replace has completed the schema checker is run again in
      [Optimistic] mode. See {!Ldap_ooclient.scflavor} *)
  method replace : op_lst -> unit

  (** Same as {!Ldap_ooclient.ldapentry.attributes}, except that the
      returned list contains attributes which may not yet exist on
      the entry. For example musts which are not yet present will be
      listed. *)
  method attributes : string list

  (** Same as {!Ldap_ooclient.ldapentry.exists} except that it
      refrences attributes which may not yet exist. For example musts
      which are not yet present. *)
  method exists : string -> bool

  (** Same as {!Ldap_ooclient.ldapentry.get_value}, except that
      attributes which do not yet exists may be referenced. For example
      a must which has not yet been satisfied will return [["required"]]
      when [get_value] is called on it. *)
  method get_value : string -> string list

  (** Same as {!Ldap_ooclient.ldapentry.modify} except that the
      schema checker is run in [Pessimistic] mode after the
      modification is applied. see {!Ldap_ooclient.scflavor}. *)
  method modify :
    (Ldap_types.modify_optype * string * string list) list -> unit

  (** Same as {!Ldap_ooclient.ldapentry.changes} except that changes
      made by the schema checker may also be listed. *)
  method changes : (Ldap_types.modify_optype * string * string list) list

  (** Same as {!Ldap_ooclient.ldapentry.changetype} *)
  method changetype : changetype

  (** Same as {!Ldap_ooclient.ldapentry.dn} *)
  method dn : string

  (** Same as {!Ldap_ooclient.ldapentry.flush_changes} *)
  method flush_changes : unit

  (** Same as {!Ldap_ooclient.ldapentry.diff} *)
  method diff : ldapentry_t -> (Ldap_types.modify_optype * string * string list) list

  (** @deprecated Same as {!Ldap_ooclient.ldapentry.print}, except
      that it prints attributes which may not yet be present on the
      object. For example, if the object has unsatisfied musts, it will
      print "attrname: required" for that attribute. *)
  method print : unit

  (** Same as {!Ldap_ooclient.ldapentry.set_changetype} *)
  method set_changetype : changetype -> unit

  (** Same as {!Ldap_ooclient.ldapentry.set_dn} *)
  method set_dn : string -> unit
end

(** {1 Schema Aware Entry for Account Managment} A derivative of
    {!Ldap_ooclient.scldapentry} which includes abstractions for
    managing user accounts in the directory. This class is
    experimantal, and may be drastically changed in the next version.
    As with all experimental code, use with caution. A few of its features.

    {ul
    {- Loosely dependant attributes: Many attributes are derived
    from others via a function. ldapaccount allows you to codify
    that relationship by providing an attribute generator
    ({!Ldap_ooclient.generator}) for the attribute, which will
    be used to derive it's value except in the case that it is
    specified explicitly}
    {- Attribute and Generator Grouping: via the service abstraction.
    Allows you to group attributes together with generators and
    default values in interesting ways. You can then assign the
    whole grouping a name, and refer to it by that name. See
    {!Ldap_ooclient.service}}
    {- Difference Based: Service operations are difference based,
    all applications of service operations compute the delta between
    the current object, and what the service requires. The minumum set
    of changes necessary to satisfy the service are applied to the object.}
    {- Idempotentcy: As a result of being difference based,
    Service operations are itempotent. For example,
    adding a service twice has no effect on the object. It will
    not queue changes for modification to the directory, and it
    will not change the object in memory. Deleting a service
    twice has no effect...etc}}

*)

(** The structure of a generator *)
type generator = {
  (** The name of the generator, this should also be its key in the hashtbl *)
  gen_name : string;

  (** A list of names of attributes which are required by this
      generator. The names need not be canonical. *)
  required : string list;

  (** A function which returns a list of values for the attribute,
      given the entire object. *)
  genfun : ldapentry_t -> string list;
}

(** The structure of a service *)
type service = {
  (** The name of the service, should also be its key in the hashtbl. *)
  svc_name : string;

  (** A list of attributes and values which must be present for the
      service to be satisfied. *)
  static_attrs : (string * string list) list;

  (** A list of attributes to generate. *)
  generate_attrs : string list;

  (** A list of services on which this service depends. *)
  depends : string list;
}

(** The type of error raised by attribute generators *)
type generation_error =
    Missing_required of string list
  | Generator_error of string

(** You've asked it to generate an attribute (in a service) which
    doesn't have a generator *)
exception No_generator of string

(** Generator has failed because of some kind of error *)
exception Generation_failed of generation_error

(** The service you're talking about doesn't exist *)
exception No_service of string

(** A service which the one you tried to add depends on doesn't exists *)
exception Service_dep_unsatisfiable of string

(** Your generator depends on an attribute which isn't in the schema *)
exception Generator_dep_unsatisfiable of string * string

(** You have detached cycles in your generator dependancy lists *)
exception Cannot_sort_dependancies of string list

class ldapaccount :
  Ldap_schemaparser.schema ->
    (string, generator) Hashtbl.t ->
    (string, service) Hashtbl.t ->
object

  (** {2 Account Manipulation Methods} *)

  (** add the named service to the object, this also adds all the
      services depended upon by the named service. *)
  method add_service : string -> unit

  (** Delete the named service. This will also delete all services
      which depend on it, either directly or indirectly *)
  method delete_service : string -> unit

  (** Run service through the delta engine to find out what changes
      would actually be applied to this object *)
  method adapt_service : service -> service

  (** Tests whether the named service is satisfied by the current
      entry. A service is satisfied if no changes would result from
      adding it to the entry. *)
  method service_exists : string -> bool

  (** Return a list of all the named services which are satisfied by
      the current entry. *)
  method services_present : string list

  (** add the named attribute to the list of attributes to be generated *)
  method add_generate : string -> unit

  (** Delete the named attribute from the list of attributes to generate *)
  method delete_generate : string -> unit

  (** Run the generation functions on the list of attributes to be
      generated, saving the results in the entry. You must run this
      method in order to run any generators at all.  *)
  method generate : unit

  (** {2 Inherited Methods} Unless explicitly stated, these methods
      do exactly the same thing as in {!Ldap_ooclient.scldapentry} *)

  (** Missing attributes may be marked for generation. *)
  method add : op_lst -> unit
  method attributes : string list
  method changes : (Ldap_types.modify_optype * string * string list) list
  method changetype : changetype
  method delete : op_lst -> unit
  method dn : string
  method diff : ldapentry_t -> (Ldap_types.modify_optype * string * string list) list
  method exists : string -> bool
  method flush_changes : unit

  (** If a missing attribute is marked for generation its value will
      be ["generate"] instead of ["required"] *)
  method get_value : string -> string list
  method is_allowed : string -> bool
  method is_missing : string -> bool
  method list_allowed : Setstr.elt list
  method list_missing : Setstr.elt list
  method list_present : Setstr.elt list
  method modify :
    (Ldap_types.modify_optype * string * string list) list -> unit
  method of_entry : ?scflavor:scflavor -> ldapentry -> unit

  (** @deprecated Missing required attributes which will be
      generated are shown as "attrname: generate" instead of
      "attrname: required" *)
  method print : unit
  method replace : op_lst -> unit
  method set_changetype : changetype -> unit
  method set_dn : string -> unit
end
