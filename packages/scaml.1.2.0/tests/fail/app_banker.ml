(* banker.ml: a signature managed deposit account;
 * written to be compiled with SCaml and deployed to carthagenet
 * by github:haochenx on 2020-Feb-28 *)
open SCaml

type param = {
    pctr   : nat;
    action : action
  }

and action =
  | Deposit
  | Withdraw of {
      amount : tz;
      sg : signature            (* manager's signature for [pack envelope] *)
    }

and store = {
    ctr     : nat;
    bal     : tz;
    manager : key;
    rnonce  : nat;         (* nonce to prevent signature reply attack;
                            * should be unique per deployed contract *)
  }

type envelope = nat * nat * tz  (* rnonce, pctr, amonut *)
type checksig_result = SigOK | SigBad

(* some utility fuctions *)
let nat1 = Nat 1
let natinc x = x +^ nat1
let incctr ({ctr} as st) = { st with ctr = natinc ctr }

let transfer2source (k : tz) =
  let src = Global.get_source () in
  let srck = match Contract.contract src with
    | None -> failwith "arienai"
    | Some x -> x
  in
  Operation.transfer_tokens () k srck

let checksig key (evlp : envelope) sg =
  let packed = Obj.pack evlp in
  if Crypto.check_signature key sg packed
  then SigOK else SigBad
  

(* the entry point *)
let entry {pctr; action} ({ctr;bal;manager;rnonce} as st) =
  if pctr <> ctr then failwith ("counter inconsistency",ctr,pctr) else
    match action with
    | Deposit -> ([], { (incctr st) with
                           bal = bal +$ (Global.get_amount ()) })
    | Withdraw {amount;sg} ->
       match checksig manager (rnonce,pctr,amount) sg with
       | SigOK -> 
          ([transfer2source amount],
           { (incctr st) with
             bal = bal -$ amount } )
       | SigBad -> failwith "bad signature"
