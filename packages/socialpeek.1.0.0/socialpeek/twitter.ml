open Metatags
open Base

let with_fallback group fallback prev_val =
  match prev_val with
  | None -> last_of group fallback
  | v -> v

let get_site twitter = last_of twitter "twitter:site"

let get_site_id twitter = last_of twitter "twitter:site:id"

let get_creator twitter = last_of twitter "twitter:creator"

let get_creator_id twitter = last_of twitter "twitter:creator:id"

let get_title twitter opengraph = last_of twitter "twitter:title"
                                  |> with_fallback opengraph "og:title"
                                  |> or_empty

let get_description twitter opengraph = last_of twitter "twitter:description"
                                        |> with_fallback opengraph "og:description"
                                        |> or_empty

let get_image twitter opengraph = last_of twitter "twitter:image"
                                  |> with_fallback opengraph "og:image"

let get_image_alt twitter = last_of twitter "twitter:image:alt"
                            |> or_empty

module Summary = struct
  type t = {
    site : string option;
    site_id : string option;
    creator_id : string option;
    description : string;
    title : string;
    image : string option;
    image_alt : string;
  }

  let build {twitter; opengraph} = {
    site = get_site twitter;
    site_id = get_site_id twitter;
    creator_id = get_creator_id twitter;
    description = get_description twitter opengraph;
    title = get_title twitter opengraph;
    image = get_image twitter opengraph;
    image_alt = get_image_alt twitter
  }
end

module Summary_large_image = struct
  type t = {
    site : string option;
    site_id : string option;
    creator : string option;
    creator_id : string option;
    title : string;
    description : string;    
    image : string option;
    image_alt : string;
  }

  let build {twitter; opengraph} = {
    site = get_site twitter;
    site_id = get_site_id twitter;
    creator = get_creator twitter;
    creator_id = get_creator_id twitter;
    description = get_description twitter opengraph;
    title = get_title twitter opengraph;
    image = get_image twitter opengraph;
    image_alt = get_image_alt twitter
  }
end

module App = struct
  type app_info = {
    name : string;
    id : string;
    url : string;
  }

  type t = {
    site : string;
    iphone : app_info;
    ipad : app_info;
    google_play : app_info;
  }

  let get_app_info group device =
    let get key = last_of group ("twitter:app:" ^ key ^ ":" ^ device)
                  |> Option.value ~default:"" in
    { name = get "name";
      id = get "id";
      url = get "url"}

  let build {twitter} = {
    site = get_site twitter |> Option.value ~default:"";
    iphone = get_app_info twitter "iphone";
    ipad = get_app_info twitter "ipad";
    google_play = get_app_info twitter "googleplay"
  }
end

module Player = struct
  type t = {
    site : string option;
    site_id : string option;
    description : string;
    title : string;
    image : string option;
    image_alt : string;
    player : string option;
    width : int;
    height: int;
    stream: string option;
  }

  let build {twitter; opengraph} = 
    let get_int k = match last_of twitter ("twitter:player:" ^ k) with
      | Some(v) -> (try Int.of_string v with _ -> 0)
      | None -> 0 
    in
    {
      site = get_site twitter;
      site_id = get_site_id twitter;
      description = get_description twitter opengraph;
      title = get_title twitter opengraph;
      image = get_image twitter opengraph;
      image_alt = get_image_alt twitter;
      player = last_of twitter "twitter:player";
      height = get_int "height";
      width = get_int "width";
      stream = last_of twitter "twitter:player:stream"
    }
end

type twitter_card = Summary of Summary.t
                  | Summary_large_image of Summary_large_image.t
                  | App of App.t
                  | Player of Player.t

let get_card groups =
  let kind = match last_of groups.twitter "twitter:card" with
    | Some(value) -> value
    | None -> last_of groups.opengraph "og:type" |> Option.value ~default:"summary" in
  match kind with
  | "summary_large_image" -> Summary_large_image (Summary_large_image.build groups)
  | "app" -> App (App.build groups)
  | "player" -> Player (Player.build groups)
  | _ -> Summary (Summary.build groups)