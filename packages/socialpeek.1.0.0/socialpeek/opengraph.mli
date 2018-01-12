open Base

module Image : sig
  type t = {
    url : string;
    secure_url : string;
    mime_type : string;
    width : int;
    height : int;
    alt : string;
  }
end

module Video : sig
  type t = {
    url : string;
    secure_url : string;
    mime_type : string;
    width : int;
    height : int;
    alt : string;
  }
end

module Audio : sig
  type t = {
    url : string;
    secure_url : string;
    mime_type : string;
  }
end

(** All the Opengraph data about a page, more info at: http://ogp.me/ *)
type t = {
  title: string;
  type_: string;
  url: string;
  description: string;
  determiner: string;
  locale: string; (** Default is "en_US" *)
  alternate_locales: string list;
  site_name: string;
  images: Image.t list;
  videos: Video.t list;
  audios: Audio.t list;
}

(** Return the OpenGraph data of the webpage given the grouped metatags. *)
val get_data : Metatags.grouped_metatags -> t