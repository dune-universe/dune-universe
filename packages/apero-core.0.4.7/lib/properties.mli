open Key_value

module Property : sig
  include module type of KeyValueF.Make (String) (String)
end

module Properties : sig
  include module type of Property.Map

  val get : key -> 'a t -> 'a option
  (** [get k p] returns [Some v] if [p] contains a property with name [k] where [v] is its value. 
      It returns [None] if [p] doesn't contain such property *)

  val get_or_default : key -> default:'a -> 'a t -> 'a
  (** [get_or_default def k p] returns [v] if [p] contains a property with name [k] where [v] is its value. 
      It returns [def] if [p] doesn't contain such property *)

  val of_list : (key * 'a) list -> 'a t
  (** [of_list kvs] returns a properties map made from the (key,value) list [kvs].
      Note that if [kvs] contains duplicate keys, only the first associated value
      from the list is added into the properties map *)
  val to_list : 'a t -> (key * 'a) list
  (** [to_list p] returns the (key,value) list of all properties from [p] *)

  val of_string : ?prop_sep:string -> ?kv_sep:string -> string -> string t
  (** [of_string s] parses the string [s] using [kv_sep] as separator between key and value ("=" by default)
      and [prop_sep] as separator between the (key,value) tuples (";" by default), and returns a properties map
      from the parsed (key, value) tuples *)
  val to_string : ?prop_sep:string -> ?kv_sep:string -> string t -> string
  (** [to_string p] returns a string representing [p] as a (key,value) list,
      with [prop_sep] as separator between the (key,value) tuples (";" by default)
      and [kv_sep] as separator between key and value ("=" by default) *)

  val contains_key : key -> 'a t -> bool
  (** [contains_key k p] returns true if [p] contains a property with [k] as a key *)
  val contains_property : key -> 'a -> 'a t -> bool
  (** [contains_property k v p] returns true if [p] contains a [(k*v)] property *)
  val contains_conflicting_property : key -> 'a -> 'a t -> bool
  (** [contains_conflicting_property k v p] returns true if [p] contains a property with [k] as a key and with a value different than [v] *)

  val is_subset : 'a t -> 'a t -> bool
  (** [is_subset p p'] return true if [p] is a subset of [p']. I.e.: 
       it doesn't exist in p a property (k,v) which is not included in p' *)
  val not_conflicting : 'a t -> 'a t -> bool
  (** [not_conflicting p p'] return true if [p] is not conflicting with [p']. I.e.:
      it doesn't exist in [p] a property with is conflicting with a property in [p'] *)

  val decode_property_value : ('a -> 'b) -> key -> 'a t -> 'b option
  val encode_property_value : ('a -> 'b) -> 'a -> 'b option
end

type properties = Property.Value.t Properties.t
