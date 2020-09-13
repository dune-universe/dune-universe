module FA12 = struct
  [@@@SCaml] (* smart contract *)
  open SCaml

  type allowance_key =
    { owner: address
    ; spender: address
    }

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

  exception NotEnoughBalance of { required : nat ; present : nat }

  let do_transfer (from : address) to_ value accounts =
    let v_from = match BigMap.get from accounts with
      | None -> Nat 0
      | Some n -> n
    in
    let v_from'_int = v_from -^ value in
    let v_from' = match isnat v_from'_int with
      | None -> raise (NotEnoughBalance { required= value; present= v_from })
      | Some (Nat 0) -> None
      | Some v_from' -> Some v_from'
    in
    let v_to' =
      match BigMap.get to_ accounts with
      | None -> Some value
      | Some n -> Some (n +^ value)
    in
    BigMap.update from v_from' @@ BigMap.update to_ v_to' accounts

  exception NotEnoughAllowance of { required : nat ; present : nat }

  let [@entry] transfer { from; to_; value } s =
    let sender = Global.get_sender () in
    if from = sender then
      (* When called with "from" account equal to the transaction sender,
         we assume that the user transfers their own money and this does not
         require approval. *)
      [],
      { s with accounts= do_transfer from to_ value s.accounts }
    else
      (* Otherwise, the transaction sender must be previously authorized
         to transfer at least the requested number of tokens from the "from"
         account using the approve entrypoint.  In this case current number
         of tokens that sender is allowed to withdraw from the "from" address
         is decreased by the number of transferred tokens. *)
      let k = { owner= from; spender= sender } in
      let allowed =
        match BigMap.get k s.allowance with
        | None -> Nat 0
        | Some n -> n
      in
      if value > allowed then
        raise (NotEnoughAllowance { required= value ; present= allowed });
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

  exception UnsafeAllowanceChange of { previous : nat }

  let get_allowance k s = match BigMap.get k s.allowance with
    | None -> Nat 0
    | Some n -> n

   (* This entrypoint called with (address :spender, nat :value) parameters
      allows spender account to withdraw from the sender, multiple times,
      up to the value amount.  Each call of transfer entrypoint decreases
      the allowance amount on the transferred amount of tokens unless transfer
      is called with from account equal to sender. *)
  let [@entry] approve { spender; value } s =
    let sender = Global.get_sender () in
    let k = { owner= sender; spender } in
    [],
    match get_allowance k s, value with
    | Nat 0, Nat 0 -> s
    | Nat 0, _ -> { s with allowance= BigMap.update k (Some value) s.allowance }
    | _, Nat 0 -> { s with allowance= BigMap.update k None s.allowance }
    | _, _ ->
        (* Changing allowance value from non-zero value to a non-zero value is
           forbidden to prevent the corresponding attack vector.
           See https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM/edit
        *)
        raise (UnsafeAllowanceChange { previous= value })

  (* XXX We want Operation.call, which sends Tz 0 *)

  let [@entry] getAllowance (a, c : (allowance_key, nat) view) s =
    [Operation.transfer_tokens (get_allowance a s) (Tz 0.) c], s

  (* This view returns balance of the given address,
     or zero if no such address is registered. *)
  let [@entry] getBalance (a, c : (address, nat) view) s =
    let n = Option.value (BigMap.get a s.accounts) (Nat 0) in
    [Operation.transfer_tokens n (Tz 0.) c], s

  (* This view returns the sum of all participants' balances. *)
  let [@entry] getTotalSupply ((), c : (unit, nat) view) s =
    [Operation.transfer_tokens s.totalSupply (Tz 0.) c], s
end

(* Emit the compiled Michelson.  This call must be executed after all
   the smart contract code are declared. *)
let () = SCamlc.Ppx.emit ~outputprefix:"fa12"
