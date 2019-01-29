let rec searchi =
        (
          ~get: int => 'a,
          ~compare: ('a, 'a) => int,
          ~testCompare: 'a => int,
          ~first: int,
          ~last: int,
          _: unit,
        )
        : option(int) =>
  if (first <= last) {
    let getMiddle = (first, last) => first + (last - first) / 2;
    let continue = (first, last) => abs(first - last) > 1;
    GeneralBinarySearch.search(
      ~get,
      ~getMiddle,
      ~continue,
      ~compare,
      ~testCompare,
      ~first,
      ~last,
      (),
    );
  } else {
    raise(
      Exceptions.InvalidArguments(
        "BinarySearch.searchi",
        "~last must be greater than or equal to ~first, but ~first was '"
        ++ string_of_int(first)
        ++ "' and ~last was '"
        ++ string_of_int(last)
        ++ "'",
      ),
    );
  };

let search =
    (
      ~get: int => 'a,
      ~compare: ('a, 'a) => int,
      ~testCompare: 'a => int,
      ~first: int,
      ~last: int,
      _: unit,
    )
    : option('a) => {
  let i = searchi(~get, ~compare, ~testCompare, ~first, ~last, ());
  switch (i) {
  | Some(i) => Some(get(i))
  | None => None
  };
};
