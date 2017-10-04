(*
Simple OCaml library for generating VCards per RFC-6350

Vadim Zaliva <lord@crocodile.org> https://github.com/vzaliva/vcardgen

Based on code by Dominic Price https://github.com/dominicjprice/sociaml-vcard
 *)

module Group : sig
  type t 
  val to_string : t -> string option
  val empty_group : t
  val group : string -> t
end = struct
  
  type t = string option
  let to_string g = g
  let empty_group = None
  let group s = Some s
end

module Name : sig
  type t =
    | SOURCE | KIND | FN | N | NICKNAME | PHOTO | BDAY | ANNIVERSARY | GENDER | ADR 
    | TEL | EMAIL | IMPP | LANG | TZ | GEO | TITLE | ROLE | LOGO | ORG | MEMBER 
    | RELATED | CATEGORIES | NOTE | PRODID | REV | SOUND | UID | CLIENTPIDMAP
    | URL | KEY | FBURL | CALADRURI | CALURI | XML | BIRTHPLACE | DEATHPLACE
    | DEATHDATE | EXPERTISE | HOBBY | INTEREST | ORG_DIRECTORY | X_NAME of string
  val to_string : t -> string
end = struct
  
  type t =
    | SOURCE | KIND | FN | N | NICKNAME | PHOTO | BDAY | ANNIVERSARY | GENDER | ADR 
    | TEL | EMAIL | IMPP | LANG | TZ | GEO | TITLE | ROLE | LOGO | ORG | MEMBER 
    | RELATED | CATEGORIES | NOTE | PRODID | REV | SOUND | UID | CLIENTPIDMAP
    | URL | KEY | FBURL | CALADRURI | CALURI | XML | BIRTHPLACE | DEATHPLACE
    | DEATHDATE | EXPERTISE | HOBBY | INTEREST | ORG_DIRECTORY | X_NAME of string
                                                                             
  let to_string = function
    |SOURCE -> "SOURCE" | KIND -> "KIND" | FN -> "FN" | N -> "N" | NICKNAME -> "NICKNAME" 
    | PHOTO -> "PHOTO" | BDAY -> "BDAY" | ANNIVERSARY -> "ANNIVERSARY" | GENDER -> "GENDER" 
    | ADR  -> "ADR" | TEL -> "TEL" | EMAIL -> "EMAIL" | IMPP -> "IMPP" | LANG -> "LANG" 
    | TZ -> "TZ" | GEO -> "GEO" | TITLE -> "TITLE" | ROLE -> "ROLE" | LOGO -> "LOGO" | ORG -> "ORG" 
    | MEMBER -> "MEMBER" | RELATED -> "RELATED" | CATEGORIES -> "CATEGORIES" | NOTE -> "NOTE"
    | PRODID -> "PRODID" | REV -> "REV" | SOUND -> "SOUND" | UID -> "UID" 
    | CLIENTPIDMAP -> "CLIENTPIDMAP" | URL -> "URL" | KEY -> "KEY" | FBURL -> "FBURL" 
    | CALADRURI -> "CALADRURI" | CALURI -> "CALURI" | XML -> "XML" | BIRTHPLACE -> "BIRTHPLACE"
    | DEATHPLACE -> "DEATHPLACE" | DEATHDATE -> "DEATHDATE" | EXPERTISE -> "EXPERTISE" 
    | HOBBY -> "HOBBY" | INTEREST -> "INTEREST" | ORG_DIRECTORY -> "ORG_DIRECTORY" | X_NAME xname -> xname
                                                                                                       
end

module Parameter : sig
  type t = { 
      name : string;
      values : string list;
    }
  val parameter : string -> string -> t
  val parameters : string -> string list -> t
end = struct
  type t = { 
      name : string;
      values : string list;
    }

  let parameter name value = {name=name; values=[value]}
  let parameters name values = {name=name; values=values}
end
        
module Value : sig
  type t
  val to_string : t -> string
  val string_value : string -> t
end = struct
  
  type t = string
  let to_string t = t
  let string_value s = s
end

module Content_line = struct
  type t = {
      group : Group.t;
      name : Name.t;
      parameters : Parameter.t list;
      value : Value.t;
    }
  let content_line ?group:(group=Group.empty_group) name parameters value =
    {
      group = group;
      name = name;
      parameters = parameters;
      value = value
    }
end

module Vcard : sig
  type t 
  val empty_vcard : t
  val append_content_line: t -> Content_line.t -> t
  val append: t -> ?group:Group.t -> Name.t -> (Parameter.t list) -> Value.t -> t
  val append_photo: t -> ?group:Group.t -> string -> string -> t
  val append_photo_from_file: t -> ?group:Group.t -> string -> string -> t
  val print: (string -> unit) -> t -> unit
                                   
end = struct
  
  type t = {
      content_lines : Content_line.t list;
    }
             
  let empty_vcard : t = {content_lines = [] }
                          
  let append_content_line vcard line =
    { content_lines = List.append vcard.content_lines [line] }
      
  let append vcard ?group:(group=Group.empty_group) name parameters value =
    append_content_line vcard
                        (Content_line.content_line ~group:group name parameters value)
                        
  (* convenience functions to add photo field *)
                        
  let append_photo vcard ?group:(group=Group.empty_group) data ptype =
    append vcard ~group:group Name.PHOTO [
             Parameter.parameter "type" ptype;
             Parameter.parameter "ENCODING" "b";
           ] (Value.string_value data)
           
  let append_photo_from_file vcard ?group:(group=Group.empty_group) filename ptype =
    let open Batteries in
    let ic = open_in filename in
    try
      let data = IO.read_all ic in
      let b64data = Base64.str_encode data in
      append_photo vcard ~group:group b64data ptype
    with e -> 
      close_in_noerr ic;
      raise e

  let escape_value value =
    let substr_replace_all pattern swith =
      Str.global_replace (Str.regexp_string pattern) swith in
    value |> substr_replace_all "\\" "\\\\" |> 
      substr_replace_all "\n" "\\n" |> 
      substr_replace_all "," "\\,"
                         
  let rec split s out =
    match String.length s with
    | l when l <= 75 -> out s
    | l -> 
       let e = Batteries.UTF8.prev s 76 in
       String.sub s 0 e |> out;
       out "\r\n ";
       split (String.sub s e (l - e)) out

  let print out vcard =
    let open Content_line in
    out "BEGIN:VCARD\r\nVERSION:4.0\r\n";
    vcard.content_lines |> List.iter (fun cl ->
                               let buf = Buffer.create 75 in
                               (match Group.to_string cl.group with 
                                | Some g -> Buffer.add_string buf g; Buffer.add_char buf '.'
                                | None -> ());
                               Name.to_string cl.name |> Buffer.add_string buf;
                               cl.parameters |> List.iter (fun p -> 
                                                    Buffer.add_char buf ';';
                                                    Buffer.add_string buf p.Parameter.name;
                                                    Buffer.add_char buf '=';
                                                    List.hd p.Parameter.values |> Buffer.add_string buf;
                                                    List.tl p.Parameter.values |> List.iter (fun v ->
                                                                                      Buffer.add_char buf ',';
                                                                                      Buffer.add_string buf v)
                                                  );
                               Buffer.add_char buf ':';
                               Value.to_string cl.value |> escape_value |> Buffer.add_string buf;
                               split (Buffer.contents buf) out;
                               out "\r\n");
    out "END:VCARD\r\n"
                  
end
        
        
