module StringSet = OcamlDiff.Set.Make(String);

let oldSet = StringSet.(empty |> add("A") |> add("B") |> add("C"));
let newSet =
  StringSet.(oldSet |> remove("A") |> remove("C") |> add("D") |> add("C"));
let addedItems =
  StringSet.symmetric_diff(oldSet, newSet, ~acc=[], ~f=(either, acc) =>
    switch (either) {
    | Left(a) =>
      Js.log("Element " ++ a ++ " is only in the old set.");
      acc;
    | Right(a) =>
      Js.log("Element " ++ a ++ " is only in the new set.");
      [a, ...acc];
    }
  );
/*
 > Element A is only in the old set.
   Element D is only in the new set.
 */

Js.log(Array.of_list(addedItems));
/*
 > [ 'D' ]
 */