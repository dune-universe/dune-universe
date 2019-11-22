module Domain = Mimestring.Case_insensitive

include (
  Email_address :
    module type of struct
    include Email_address
  end
  with module Domain := Email_address.Domain)

let local_address () = create (Core.Unix.getlogin ())
