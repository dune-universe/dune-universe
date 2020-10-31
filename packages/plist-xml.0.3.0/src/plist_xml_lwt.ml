module IO : Plist_xml.IO
  with type s = Markup.async and type 'a io = 'a Lwt.t = struct
  type s = Markup.async
  type 'a io = 'a Lwt.t
  let next = Markup_lwt.next
  let peek = Markup_lwt.peek
  let parse_xml ?report ?encoding ?namespace ?entity ?context =
    Markup_lwt.parse_xml ?report ?encoding ?namespace ?entity ?context
  let bind = Lwt.bind
  let return = Lwt.return
end

include Plist_xml.Make(IO)
