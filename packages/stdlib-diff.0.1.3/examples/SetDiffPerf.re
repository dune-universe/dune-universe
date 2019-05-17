module IntSet =
  OcamlDiff.Set.Make({
    type t = int;
    let compare = Pervasives.compare;
  });

Js.log("Generating...");
let gen = count => {
  let rec loop = (acc, i, count) =>
    if (i < count) {
      loop(acc |> IntSet.add(Random.int(count * 5)), i + 1, count);
    } else {
      acc;
    };
  loop(IntSet.empty, 0, count);
};

let a = gen(1000000);
let b = IntSet.(a |> add(800421) |> add(420420));

Js.log("generated.");

let res =
  IntSet.symmetric_diff(a, b, ~acc=[], ~f=(either, acc) =>
    switch (either) {
    | Left(a) =>
      Js.log("Left " ++ string_of_int(a));
      [a, ...acc];
    | Right(a) =>
      Js.log("Right " ++ string_of_int(a));
      [a, ...acc];
    }
  );
Js.log(Array.of_list(res));