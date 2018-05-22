let () =
  let res =
    [| Some "3"; Some "Paul"; Some "7" |]
    |> [%madcast: string option array -> (int * string * string option)]
  in
  assert (res = (3, "Paul", Some "7"))


let () =
  let actual =
    [| Some [| Some "1" ; Some "Pierre" ; Some "7" |] ;
       Some [| Some "3" ; Some "Paul" ; None |] ;
       None |]
    |> [%madcast: string option array option array -> (int * string * string option) option list]
  in
  let expected = [ Some (1, "Pierre", Some "7") ;
                   Some (3, "Paul", None) ;
                   None ] in
  assert (actual = expected)
