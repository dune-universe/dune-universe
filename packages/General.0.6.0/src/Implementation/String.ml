include Foundations.String

let split' s ~seps =
  let seps = SortedSet.Poly.of_list seps in
  let (parts, last_part) =
    s
    |> fold ~init:([], []) ~f:(fun (parts, current_part) c ->
      if SortedSet.Poly.contains seps ~v:c then
        (current_part::parts, [])
      else
        (parts, c::current_part)
    )
  in
  last_part::parts
  |> List.map ~f:List.reverse
  |> List.map ~f:of_list
  |> List.reverse

module Tests = struct
  open Testing

  module Examples = struct
    let repr = [
      ("foo", "\"foo\"");
      ("bar\"baz", "\"bar\\\"baz\"");
    ]

    let equal = [
      ["foo"];
    ]

    let different = [
      ("foo", "bar");
    ]

    let ordered = [
      ["aaaa"; "aaaaa"; "aaaab"; "ab"; "b"];
    ]
  end

  let test = "String" >:: [
    (let module T = Concepts.Able.Tests.Make0(Foundations.String)(Examples) in T.test);
    "split'" >:: (
      let make s seps expected =
        ~: "%S %S" s (of_list seps) (lazy (
          check_string_list ~expected (split' s ~seps)
        ))
      in
      [
        make "abcdefghfj" ['c'; 'f'] ["ab"; "de"; "gh"; "j"];
        make "xabxxcdx" ['x'] [""; "ab"; ""; "cd"; ""];
      ]
    );
  ]
end
