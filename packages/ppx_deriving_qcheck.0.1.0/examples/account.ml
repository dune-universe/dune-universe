let arb_uint = QCheck.(map abs small_int)

type balance = (int[@arb arb_uint]) [@@deriving arb]

type name = string [@@deriving arb]

type account = Account of name * balance [@@deriving arb]

type context = account list [@@deriving arb]

(** [get_amount acc] returns account amount, must be `Account *)
let get_amount (Account (_, x)) = x

(** [get_opt ctxt name] get account with [name] in [ctxt] if it exists *)
let get_opt ctxt name =
  List.find_opt (fun (Account (name', _)) -> name = name') ctxt

(** [get_exn ctxt name] get account with [name] in [ctxt], raises exception when
    not found *)
let get_exn ctxt name = Option.get @@ get_opt ctxt name

(** [update_account ctxt name amount] sets [amount] for account with [name] in [ctxt] *)
let update_account ctxt name amount =
  let amount = if amount < 0 then 0 else amount in

  List.map
    (function
      | Account (name', _) when name = name' -> Account (name, amount) | x -> x)
    ctxt

(** [valid_transfer ctxt src dst amount] returns whether the transfer is valid or not *)
let valid_transfer ctxt src dst amount =
  match (src, dst) with
  | (Some (Account _), None) -> true
  | (None, Some (Account _)) -> true
  | (Some (Account (x, amount')), Some (Account (y, _))) ->
      Option.is_some (get_opt ctxt x)
      && Option.is_some (get_opt ctxt y)
      && amount' >= amount
  | _ -> false

(** [transfer ctxt src dst amount] transfers [amount] from [src] to [dst] in [ctxt] *)
let transfer ctxt src dst amount =
  match (src, dst) with
  | (Some (Account (name, x)), None) -> update_account ctxt name (x - amount)
  | (None, Some (Account (name, x))) -> update_account ctxt name (x + amount)
  | (Some (Account (src, x)), Some (Account (dst, y))) ->
      if x >= amount then
        let ctxt = update_account ctxt src (x - amount) in
        update_account ctxt dst (y + amount)
      else ctxt
  | _ -> ctxt

let test_transfer_sink =
  let open QCheck in
  Test.make
    ~name:"[transfer ctxt (None => account) x] gives [x] to account"
    QCheck.(pair arb_context arb_uint)
    (fun (ctxt, n) ->
      assume (List.length ctxt > 0) ;

      let (Account (name, amount) as src) = List.hd ctxt in
      let new_ctxt = transfer ctxt None (Some src) n in

      let expected =
        let tmp = amount + n in
        if tmp < 0 then 0 else tmp
      in

      let actual = get_amount (get_exn new_ctxt name) in

      actual = expected)

let test_transfer_burn =
  let open QCheck in
  Test.make
    ~name:"[transfer ctxt (account => None) x] burns [x] from account"
    QCheck.(pair arb_context arb_uint)
    (fun (ctxt, n) ->
      assume (List.length ctxt > 0) ;

      let (Account (name, amount) as src) = List.hd ctxt in
      let new_ctxt = transfer ctxt (Some src) None n in

      let expected =
        let tmp = amount - n in
        if tmp < 0 then 0 else tmp
      in

      let actual = get_amount (get_exn new_ctxt name) in

      actual = expected)

let test_transfer_accounts =
  let open QCheck in
  Test.make
    ~name:
      "[transfer ctxt (account => account') x] takes [x] from account to \
       account'"
    QCheck.(pair arb_context arb_uint)
    (fun (ctxt, n) ->
      assume (List.length ctxt > 2) ;
      match ctxt with
      | (Account (name, amount) as src)
        :: (Account (name', amount') as dst) :: _ ->
          assume (valid_transfer ctxt (Some src) (Some dst) n) ;

          let new_ctxt = transfer ctxt (Some src) (Some dst) n in

          let src_expected =
            let tmp = amount - n in
            if tmp < 0 then 0 else tmp
          in
          let dst_expected = amount' + n in

          let src_actual = get_amount (get_exn new_ctxt name) in
          let dst_actual = get_amount (get_exn new_ctxt name') in

          src_actual = src_expected && dst_actual = dst_expected
      | _ ->
          assume false ;
          false)

let _ =
  QCheck_runner.run_tests
    ~verbose:true
    [ test_transfer_burn; test_transfer_sink; test_transfer_accounts ]
