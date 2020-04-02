module Uuid = struct
  type t = { uuid:Uuidm.t; alias:string option }

  let state = Random.State.make_self_init ()

  let make () = { uuid=Uuidm.v4_gen state (); alias=None}

  (* Note: ns_apero namespace is inspired by those specified in https://tools.ietf.org/html/rfc4122#appendix-C *)
  let ns_apero = Uuidm.of_string "6ba7b842-9dad-11d1-80b4-00c04fd430c8" |> Acommon.Option.get


  let make_from_alias alias = { uuid=Uuidm.v5 ns_apero alias; alias=Some alias}

  let alias t = t.alias

  let compare t t' = Uuidm.compare t.uuid t'.uuid
  let equal t t' = Uuidm.equal t.uuid t'.uuid

  let of_bytes ?pos s =
    let open Acommon.Option.Infix in
    Uuidm.of_bytes ?pos s >== fun uuid -> { uuid; alias=None }
  let to_bytes t = Uuidm.to_bytes t.uuid

  let of_string ?pos s =
    let open Acommon.Option.Infix in
    Uuidm.of_string ?pos s >== fun uuid -> { uuid; alias=None }
  let to_string ?upper t = Uuidm.to_string ?upper t.uuid

  let nb_bytes = Uuidm.to_bytes ns_apero |> Lwt_bytes.of_string |> Lwt_bytes.length

  let encode t buf =
    let bytes = t |> to_bytes |> Bytes.of_string  in
    Abuf.write_bytes bytes buf

  let decode buf =
    Abuf.read_bytes nb_bytes buf |> Bytes.to_string |> of_bytes
    |> function
      | Some uuid -> uuid
      | None -> raise @@ Atypes.Exception(`InvalidFormat (`Msg ("Failed to decode Uuid")))

end 
