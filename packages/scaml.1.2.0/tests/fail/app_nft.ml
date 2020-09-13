open SCaml

type nfts = (nat, address) map

type storage =
  { nfts : nfts
  ; admin : address
  }

(** Create a new token with the initial owner *)
let [@entry] mint (id, address) { nfts; admin } =
  if Global.get_source () <> admin then failwith "permission denied";
  if Map.mem id nfts then failwith "id already exists";
  [], { nfts= Map.update id (Some address) nfts; admin }

(** Change the ownership.  Only the current owner can do it. *)
let [@entry] transfer (id, address) { nfts; admin } =
  match Map.get id nfts with
  | None -> failwith "no such id"
  | Some owner ->
      if Global.get_source () <> owner then failwith "permission denied";
      [], { nfts= Map.update id (Some address) nfts; admin }

(** Burn the token.  Only the curren owner can do it. *)
let [@entry] burn id { nfts; admin } =
  match Map.get id nfts with
  | None -> failwith "no such id"
  | Some owner ->
      if Global.get_source () <> owner then failwith "permission denied";
      [], { nfts= Map.update id None nfts; admin }
