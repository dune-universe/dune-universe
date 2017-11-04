open Core

type t = Re2.Regex.t
type regex = t

module Template = struct
  type t = regex * string
end

module type Creators_intf = sig
  val create : string -> t
  val create_i : string -> t
  val create_im : string -> t
  val create_m : string -> t

  module Template : sig
    type t = Template.t
    type regex
    val create : regex:regex -> template:string -> t
  end with type regex := t
end

module type Accessors_intf = sig
  module Infix : sig
    val (=~) : string -> t -> bool
  end

  module Match : sig
    type t
    val by_index : t -> int -> string
    val by_name : t -> string -> string
  end

  module Template : sig
    type t
    val apply : t -> string -> string;;
  end with type t := Template.t

  val matches : t -> string -> bool;;
  val apply : t -> string -> Match.t option
  val split_at : t -> string -> string list;;
end

module Accessors : Accessors_intf = struct
  let matches t str = Re2.Regex.matches t str;;
  let apply t str = List.hd (Re2.Regex.get_matches_exn ~max:1 t str)
  let split_at t str = Re2.Regex.split t str

  module Infix = struct
    let (=~) = Re2.Regex.Infix.(=~);;
  end

  module Match = struct
    type t = Re2.Regex.Match.t
    let by_index m i = Re2.Regex.Match.get_exn ~sub:(`Index i) m
    let by_name m n = Re2.Regex.Match.get_exn ~sub:(`Name n) m
  end

  module Template = struct
    let apply (regex, template) str =
      Or_error.ok_exn (Re2.Regex.rewrite regex ~template str)
    ;;
  end
end

module Creators = struct
  let create = Re2.Regex.create_exn ~options:[];;
  let create_i =
    Re2.Regex.create_exn ~options:[`Case_sensitive false]
  ;;
  let create_im =
    Re2.Regex.create_exn
      ~options:[
        `Case_sensitive false;
        `Posix_syntax true;
        `One_line false;
      ]
  ;;
  let create_m =
    Re2.Regex.create_exn ~options:[
      `Posix_syntax true;
      `One_line false;
    ]
  ;;

  module Template = struct
    type t = Template.t;;

    let create ~regex ~(template : string) : t =
      if Re2.Regex.valid_rewrite_template regex ~template then
        (regex, template)
      else
        invalid_arg "Template.create"
  end
end
