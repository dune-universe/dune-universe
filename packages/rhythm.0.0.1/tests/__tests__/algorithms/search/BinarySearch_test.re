open TestFramework;

open Rhythm;

describe("BinarySearch", ({test}) => {
  ();
  test("simple array", ({expect}) => {
    let arr = [|5, 10, 15, 20, 25, 35, 1000|];
    let get = i => OCamlArray.SyntaxExn.(arr[i]);
    let compare = (a: int, b: int) => a - b;
    let testCompare = value => compare(value, 35);
    let first = 0;
    let last = OCamlArray.length(arr) - 1;
    let i =
      BinarySearch.searchi(~get, ~compare, ~testCompare, ~first, ~last, ());
    let i = Option.getExn(i);
    expect.int(i).toBe(5);
  });

  test("square root", ({expect}) => {
    /* What is the square root of 17? */
    let query = 17.0;
    /* This is the answer. */
    let answer = 4.1231056;

    /* Keys and values are the same so use identity function. */
    let get = x => x;

    /* Get the mid point between the two floats. */
    let getMiddle = (a, b) => a +. (b -. a) /. 2.0;

    /* We continue while the difference is greater than 1e-7. */
    let continue = (a, b) => abs_float(a -. b) > 1e-7;

    /* Generalized float comparison. */
    let compare = compare;

    /*
     * We have the answer if the difference is less than 1e-6, otherwise we
     * compare the result to our query to determine which half to search.
     */
    let testCompare = value => {
      let result = value *. value;
      let diff = abs_float(result -. query);
      if (diff < 1e-6) {
        0;
      } else if (result < query) {
        (-1);
      } else {
        1;
      };
    };

    let first = 0.0;
    let last = query;

    let result =
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
    let result = Option.getExn(result);
    /* Helper to round floats. */
    let roundFloat = f => int_of_float(floor(f +. 0.5));
    expect.bool(abs_float(answer -. result) <= 1e-6).toBeTrue();
    expect.int(roundFloat(result *. result)).toBe(17);
  });
});
