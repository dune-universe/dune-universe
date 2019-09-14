open Lwt
module D = Dropbox_lwt_unix

let main t _args =
  D.info t >>= fun info ->
  let open Lwt_io in
  printlf "Name: %s (uid: %i)\n\
           referral_link: %s\n\
           Country: %s  (locale: %s)\n\
           Is paired: %b\n"
          info.D.display_name info.D.uid (Uri.to_string info.D.referral_link)
          info.D.country info.D.locale info.D.is_paired

let () =
  Common.run main
