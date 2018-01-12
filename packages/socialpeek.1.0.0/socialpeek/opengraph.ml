open Base
open Metatags

module Media = struct
  type t = {
    url: string;
    secure_url: string;
    mime_type: string;
    width: int;
    height: int;
    alt: string;
  }
end

module Image = struct
  include Media

  let of_tags og =
    let groups = List.filter og ~f:(fun (x, _) ->
        String.is_prefix x "og:image")
                 |> List.group ~break:(fun _ (x, _) -> 
                     String.equal x "og:image")
    in
    List.map groups ~f:(fun g ->
        let url = ref "" in
        let secure_url = ref "" in
        let type_ = ref "" in
        let width = ref 0 in
        let height = ref 0 in
        let alt = ref "" in
        List.iter g ~f:(fun (k, v) ->
            match k with
            | "og:image" -> url := v
            | "og:image:url" -> url := v
            | "og:image:secure_url" -> secure_url := v
            | "og:image:type" -> type_ := v
            | "og:image:width" -> width := Caml.int_of_string v
            | "og:image:height" -> height := Caml.int_of_string v
            | "og:image:alt" -> alt := v
            | _ -> ()
          );
        {
          url = !url;
          secure_url = !secure_url;
          mime_type = !type_;
          width = !width;
          height = !height;
          alt = !alt
        }
      )
end

module Video = struct
  include Media

  let of_tags og =
    let groups = List.filter og ~f:(fun (x, _) ->
        String.is_prefix x "og:video")
                 |> List.group ~break:(fun _ (x, _) -> 
                     String.equal x "og:video")
    in
    List.map groups ~f:(fun g ->
        let url = ref "" in
        let secure_url = ref "" in
        let type_ = ref "" in
        let width = ref 0 in
        let height = ref 0 in
        let alt = ref "" in
        List.iter g ~f:(fun (k, v) ->
            match k with
            | "og:video" -> url := v
            | "og:video:url" -> url := v
            | "og:video:secure_url" -> secure_url := v
            | "og:video:type" -> type_ := v
            | "og:video:width" -> width := Caml.int_of_string v
            | "og:video:height" -> height := Caml.int_of_string v
            | "og:video:alt" -> alt := v
            | _ -> ()
          );
        {
          url = !url;
          secure_url = !secure_url;
          mime_type = !type_;
          width = !width;
          height = !height;
          alt = !alt
        }
      )
end

module Audio = struct
  type t = {
    url: string;
    secure_url: string;
    mime_type: string;
  }

  let of_tags og =
    let groups = List.filter og ~f:(fun (x, _) ->
        String.is_prefix x "og:audio")
                 |> List.group ~break:(fun _ (x, _) -> 
                     String.equal x "og:audio")
    in
    List.map groups ~f:(fun g ->
        let url = ref "" in
        let secure_url = ref "" in
        let type_ = ref "" in
        List.iter g ~f:(fun (k, v) ->
            match k with
            | "og:audio" -> url := v
            | "og:audio:url" -> url := v
            | "og:audio:secure_url" -> secure_url := v
            | "og:audio:type" -> type_ := v
            | _ -> ()
          );
        {
          url = !url;
          secure_url = !secure_url;
          mime_type = !type_;
        }
      )
end

type t = {
  title: string;
  type_: string;
  url: string;
  description: string;
  determiner: string;
  locale: string;
  alternate_locales: string list;
  site_name: string;
  images: Image.t list;
  videos: Video.t list;
  audios: Audio.t list;
}

let get_media og = 
  (Image.of_tags og, Video.of_tags og, Audio.of_tags og)

let get_base_data og = {
  title = last_of og "og:title" |> or_empty;
  type_ = last_of og "og:type" |> or_empty;
  url = last_of og "og:url" |> or_empty;
  description = last_of og "og:description" |> or_empty;
  determiner = last_of og "og:determiner" |> or_empty;
  locale = last_of og "og:locale" |> Option.value ~default:"en_US";
  alternate_locales = Map.find og "og:locale:alternate" |> Option.value ~default:[];
  site_name = last_of og "og:site_name" |> or_empty;
  images = [];
  videos = [];
  audios = [];
}

let get_data {opengraph; opengraph_ordered} =
  let (images, videos, audios) = get_media opengraph_ordered in
  let base_data = get_base_data opengraph in
  { base_data with images = images; videos = videos; audios = audios}