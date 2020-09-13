open SCaml

type nfts = (nat, address) big_map

type storage =
  { nfts : nfts
  ; admin : address
  }

(** Create a new token with the initial owner *)
let [@entry] mint (id, address) { nfts; admin } =
  if Global.get_source () <> admin then failwith "permission denied";
  if BigMap.mem id nfts then failwith "id already exists";
  [], { nfts= BigMap.update id (Some address) nfts; admin }

(** Change the ownership.  Only the current owner can do it. *)
let [@entry] transfer (id, address) { nfts; admin } =
  match BigMap.get id nfts with
  | None -> failwith "no such id"
  | Some owner ->
      if Global.get_source () <> owner then failwith "permission denied";
      [], { nfts= BigMap.update id (Some address) nfts; admin }

(** Burn the token.  Only the curren owner can do it. *)
let [@entry] burn id { nfts; admin } =
  match BigMap.get id nfts with
  | None -> failwith "no such id"
  | Some owner ->
      if Global.get_source () <> owner then failwith "permission denied";
      [], { nfts= BigMap.update id None nfts; admin }
