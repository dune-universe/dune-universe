open SCaml

let main () () =
  List.fold_left (fun ops kh -> 
      Operation.transfer_tokens () (Tz 1.0) (Contract.implicit_account kh) :: ops)
    [] [Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"],
  ()

