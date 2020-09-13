open SCaml

type allowance_key =
  { owner: address
  ; spender: address
  } [@@deriving conv{ocaml}]

type storage = 
  { accounts : (address, nat) big_map (** Ownership of FA1.2 tokens *)
  ; allowance : (allowance_key, nat) big_map (** Allowances *)
  ; totalSupply : nat  (** Total supply *)
  }

type ('a, 'r) view = ('a * 'r contract)

type transfer = 
  { from  : address 
  ; to_   : address
  ; value : nat
  }

exception NotEnoughBalance of nat * nat

let do_transfer (from : address) to_ value accounts =
  let v_from = match BigMap.get from accounts with
    | None -> Nat 0
    | Some n -> n
  in
  let v_from'_int = v_from -^ value in
  let v_from' = match isnat v_from'_int with
    | None -> raise (NotEnoughBalance (value, v_from))
    | Some (Nat 0) -> None
    | Some v_from' -> Some v_from'
  in
  let v_to' = 
    match BigMap.get to_ accounts with
    | None -> Some value
    | Some n -> Some (n +^ value)
  in
  BigMap.update from v_from' (BigMap.update to_ v_to' accounts)

exception NotEnoughAllowance of nat * nat

let [@entry] transfer { from; to_; value } s =
  let sender = Global.get_sender () in
  if from = sender then 
    [],
    { s with accounts= do_transfer from to_ value s.accounts }
  else
    let k = { owner= from; spender= sender } in
    let allowed = 
      match BigMap.get k s.allowance with
      | None -> Nat 0
      | Some n -> n
    in
    if value > allowed then 
      raise (NotEnoughAllowance (value, allowed));
    let allowance = 
      if value = allowed then 
        BigMap.update k None s.allowance
      else
        (* allowed -^ value > Int 0 therefore abs always returns the same value in nat *)
        BigMap.update k (Some (abs (allowed -^ value))) s.allowance
    in
    [],
    { s with 
      accounts= do_transfer from to_ value s.accounts 
    ; allowance
    }

type approve =
  { spender : address
  ; value : nat
  }
    
exception UnsafeAllowanceChange of nat

let get_allowance k s = match BigMap.get k s.allowance with
  | None -> Nat 0
  | Some n -> n

let [@entry] approve { spender; value } s =
  let sender = Global.get_sender () in
  let k = { owner= sender; spender } in
  [],
  match get_allowance k s, value with
  | Nat 0, Nat 0 -> s
  | Nat 0, _ -> { s with allowance= BigMap.update k (Some value) s.allowance }
  | _, Nat 0 -> { s with allowance= BigMap.update k None s.allowance }
  | _, _ -> raise (UnsafeAllowanceChange value)

type getAllowance = (allowance_key, nat) view

let [@entry] getAllowance (a, c : getAllowance) s =
  [Operation.transfer_tokens (get_allowance a s) (Tz 0.) c], s

type getBalance = (address, nat) view

let [@entry] getBalance (a, c : getBalance) s =
  let n = match BigMap.get a s.accounts with
    | None -> Nat 0
    | Some n -> n
  in
  [Operation.transfer_tokens n (Tz 0.) c], s

type getTotalSupply = (unit, nat) view

let [@entry] getTotalSupply ((), c : getTotalSupply) s =
  [Operation.transfer_tokens s.totalSupply (Tz 0.) c], s
           

