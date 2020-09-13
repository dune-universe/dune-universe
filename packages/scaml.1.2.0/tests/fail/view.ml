open SCaml

let [@view] view (a, contract_b) storage =
  [Operation.transfer_tokens storage (Tz 0.) contract_b],
  storage
