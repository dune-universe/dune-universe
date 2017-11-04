open Async
open Core
open Core_extended.Std

module Regexp : sig
  include Email_regex.Accessors_intf

  (* All regexen and templates must be created inside this module *)
  val postmark : Email_regex.t;;
  val postmark' : Email_regex.t;;
  val escaped_line : Email_regex.t;;
  val blank : Email_regex.t;;

  val escape_line : Email_regex.Template.t;;
  (* val unix_line_terminator : Email_regex.Template.t;; *)

end
= struct
  open Email_regex.Creators;;

  (* Tokens *)
  let several str = "((" ^ str ^ ")+)";;
  let maybe str = "((" ^ str ^ ")?)";;
  let wsp = "[\t ]";;
  let wsp' = several wsp;;
  let no_wsp = "[^\t ]";;
  let no_wsp' = several no_wsp;;
  let __UNUSED_VALUE__any = ".*";;
  let alpha = "[A-Za-z]";;
  let alpha' = several alpha;;
  let digit = "[0-9]";;
  let digit' = several digit;;

  let cg name pattern = (sprintf "(?P<%s>%s)" name pattern);;
  let of_list = String.concat;;

  (* Regexen for parsing postmarks *)
  let datetime = of_list
                   [
                     cg "w" alpha'; wsp';
                     cg "m" alpha'; wsp';
                     cg "d" digit'; wsp';
                     cg "h" digit'; ":"; cg "i" digit'; ":"; cg "s" digit'; wsp';
                     cg "y" digit'
                   ]
  ;;

  let postmark = create ("^From" ^ wsp ^ "+.*$");;
  let postmark' = create (of_list
                            [ "^From"; wsp';
                              cg "from" no_wsp'; wsp';
                              cg "time" datetime; maybe wsp';
                              "$"
                            ]);;


  let escaped_line = create ("^>From" ^ wsp ^ ".*$");;
  let blank = create ("^" ^ wsp ^ "*$");;

  (* Templates *)
  let escape_line =
    Template.create
      ~regex:(create_m ("^From(" ^ wsp ^ ")"))
      ~template:">From\\1"

  let __UNUSED_VALUE__unix_line_terminator =
    Template.create
      ~regex:(create_m "\r\n")
      ~template:"\n"

  include Email_regex.Accessors;;
end

module Postmark = struct
  type t = {
    from : string;
    time : Time.t;
  } [@@deriving sexp]
  ;;

  let to_string t =
    let date, ofday = Time.to_date_ofday t.time ~zone:Time.Zone.utc in
    let parts = Time.Ofday.to_parts ofday in

    sprintf "From %s %s %s %2d %02d:%02d:%02d %4d"
      t.from
      (String.capitalize
         (String.lowercase (Day_of_week.to_string (Date.day_of_week date))))
      (Month.to_string (Date.month date))
      (Date.day date)
      parts.Time.Span.Parts.hr
      parts.Time.Span.Parts.min
      parts.Time.Span.Parts.sec
      (Date.year date)
  ;;

  (*    (sprintf "%a %b %e %T %Y");; *)
  let parse str =
    let open Result.Monad_infix in
    Result.of_option Regexp.(apply postmark' str)
      ~error:"Maybe missing >"
    >>= (fun m ->
      try
        let module Match = Regexp.Match in
        let from = Match.by_name m "from" in

        let _weekday = Day_of_week.of_string (Match.by_name m "w") in
        let y = Int.of_string (Match.by_name m "y") in
        let month = Month.of_string (Match.by_name m "m") in
        let d = Int.of_string (Match.by_name m "d") in
        let date = Date.create_exn ~y ~m:month ~d in

        let hr = Int.of_string (Match.by_name m "h") in
        let min = Int.of_string (Match.by_name m "i") in
        let sec = Int.of_string (Match.by_name m "s") in
        let ofday = Time.Ofday.create ~hr ~min ~sec () in

        let time = Time.of_date_ofday ~zone:Time.Zone.utc date ofday  in
        Result.Ok { from = from; time = time }
      with
        e -> Result.Error (Exn.to_string e))
    |> Result.map_error ~f:(sprintf "Unable to parse postmark %s: %s." str)
  ;;

  let of_string str = Result.ok_or_failwith (parse str);;
end

module Message = struct
  type t = {
    postmark : Postmark.t;
    email : Email.t;
  } [@@deriving sexp]

  let to_string message =
    let text = Email.to_string message.email in
    let text = Regexp.(Template.apply escape_line text) in
    String.concat ~sep:"\n"
      [
        Postmark.to_string message.postmark;
        text;
        ""
      ]
  ;;
end


open Regexp.Infix;;

let unescape line =
  if line =~ Regexp.escaped_line then
    String.chop_prefix_exn line ~prefix:">"
  else
    line
;;

let __UNUSED_VALUE__counting_fold fold ?(from=0) ~init ~f =
  fold
    ~init:(from, init)
    ~f:(fun (n, acc) data -> (n + 1, f n acc data))
;;

module Parser' = struct
  type t = string list * (Postmark.t option)

  type a = string
  type b = Message.t

  let create () = ([], None);;

  let parse ((current, postmark) as t) token =
    let module C = Parser_intf.Comm in

    (* Tries to get a message from the lines read so far *)
    let message_of_t ~next_t = function
      | (current, Some postmark) ->
        let text = String.concat ~sep:"\n" (List.rev current) in
        begin match Email.of_bigstring (Bigstring.of_string text) with
        | Ok email ->
          C.put next_t { Message. postmark = postmark; email = email; }
        | Error e ->
          C.warning next_t
            ~msg:(sprintf "ERROR %s in message %s."
                    (Error.to_string_hum e) (Postmark.to_string postmark))
        end
      | _ -> C.continue next_t
    in
    (* Parses a token (line or EOF) from the parser driver *)
    match token with
    | `Token line ->
      if current = [] && line =~ Regexp.blank then
        C.continue t
      else if Regexp.(line =~ postmark) then
        (* Check if beginning of message *)
        match Postmark.parse line with
        | Result.Error msg    -> C.warning t ~msg
        | Result.Ok postmark  -> message_of_t t ~next_t:([], Some postmark)
      else if Option.is_some postmark then
        (* All non-blank lines must fall inside a message *)
        C.continue ((unescape line) :: current, postmark)
      else
        C.warning t ~msg:(sprintf "No message context for: %s" line)
    | `Eof       -> message_of_t ~next_t:(create ()) t
  ;;

end

module Parser : Parser_intf.S with type a = string and type b = Message.t
  = Parser_intf.Make (Parser')

module type With_container = sig
  type t

  val t_of_fd : In_channel.t -> t
  val t_of_file : string -> t
  val of_string : string -> t
  val iter_string : t -> f:(string -> unit) -> unit
end

module With_lazy_list =
struct
  type t = Message.t Lazy_list.t

  let t_of_fd fd =
    Parser.parse_lazy_list
      (Lazy_list.of_iterator
         ~init:(fd, In_channel.input_line fd)
         ~next:(fun (fd, _) -> (fd, In_channel.input_line fd))
         ~curr:snd)
  ;;

  let t_of_file fname = In_channel.with_file fname ~f:t_of_fd;;

  let of_string str =
    let line_list = String.split str ~on:'\n' in
    Parser.parse_lazy_list (Lazy_list.of_list line_list)
  ;;

  let iter_string t ~f = Lazy_list.iter t ~f:(Fn.compose f Message.to_string);;
end


(* When creating a Lazy_sequence from an iterator, it can only be traversed once *)
module Lazy_sequence_extra = struct
  let of_fd fd =
    Lazy_sequence.protect (fun () ->
      let rec loop () =
        match In_channel.input_line fd with
        | Some line -> Lazy_sequence.(line ==> loop)
        | None      -> Lazy_sequence.empty
      in
      loop ())
      ~finally:(fun () -> In_channel.close fd)
  ;;
end

module With_seq = struct
  type t = Message.t Lazy_sequence.t

  let t_of_fd fd = Parser.parse_seq (Lazy_sequence_extra.of_fd fd);;
  let t_of_file fname = Parser.parse_seq (Lazy_sequence.read_lines fname);;

  let of_string str =
    let line_list = String.split str ~on:'\n' in
    Parser.parse_seq (Lazy_sequence.of_list line_list)
  ;;

  let iter_string t ~f =
    Lazy_sequence.iter t ~f:(Fn.compose f Message.to_string);;
end

module With_pipe = struct
  type t = Message.t Pipe.Reader.t

  (** Uses a deferred reader, returns a deferred Pipe *)
  let t_of_reader reader =
    (* Merge deferred into Pipe *)
    reader >>| fun reader ->
    let lines = Reader.lines reader in
    Parser.parse_pipe lines
  ;;

  (** Gets a pipe of messages from a file descriptor *)
  let t_of_fd fd =
    let reader =
      return (Reader.create fd)
    in
    t_of_reader reader
  ;;

  let t_of_file fname =
    let reader = Reader.open_file fname in
    t_of_reader reader
  ;;

  let of_string str =
    let line_list = String.split str ~on:'\n' in
    return (Parser.parse_pipe (Pipe.of_list line_list))
  ;;

  let iter_string t ~f =
    Pipe.iter t ~f:(Fn.compose f Message.to_string);
  ;;
end
