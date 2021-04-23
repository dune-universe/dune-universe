module Make (C : Mirage_clock.PCLOCK) = struct
  let authenticator =
    let tas =
      List.fold_left
        (fun acc data ->
          let open Rresult.R.Infix in
          acc >>= fun acc ->
          X509.Certificate.decode_der (Cstruct.of_string data) >>| fun cert ->
          cert :: acc)
        (Ok []) Trust_anchor.certificates
    and time () = Some (Ptime.v (C.now_d_ps ())) in
    fun ?crls ?allowed_hashes () ->
      match tas with
      | Ok t ->
          Ok (X509.Authenticator.chain_of_trust ~time ?crls ?allowed_hashes t)
      | Error e -> Error e
end
