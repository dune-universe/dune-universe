module Summary : sig
  type t = {
    site : string option;
    site_id : string option;
    creator_id : string option;
    description : string;
    title : string;
    image : string option;
    image_alt : string;
  }
end

module Summary_large_image : sig
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
end

module App : sig
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
end

module Player : sig
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
end

type twitter_card = Summary of Summary.t
                  | Summary_large_image of Summary_large_image.t
                  | App of App.t
                  | Player of Player.t

(** Return the twitter card of the webpage given the grouped metatags. *)
val get_card : Metatags.grouped_metatags -> twitter_card