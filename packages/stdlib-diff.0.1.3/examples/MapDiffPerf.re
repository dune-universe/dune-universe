module IntMap =
  OcamlDiff.Map.Make({
    type t = int;
    let compare = Pervasives.compare;
  });

Js.log("Generating...");

let gen = count => {
  let rec loop = (acc, i, count) =>
    if (i < count) {
      loop(
        acc |> IntMap.add(Random.int(count * 5), Random.int(count * 5)),
        i + 1,
        count,
      );
    } else {
      acc;
    };
  loop(IntMap.empty, 0, count);
};

let a = gen(500000);
let b = IntMap.(a |> add(420420, 54) |> add(42, 51));
let a = IntMap.(a |> add(800421, 42) |> add(42, 81));

Js.log("generated.");

let res =
  IntMap.symmetric_diff(a, b, ~veq=(==), ~acc=[], ~f=(either, acc) =>
    switch (either) {
    | (k, Left(a)) =>
      Js.log("Key " ++ string_of_int(k) ++ ", Left " ++ string_of_int(a));
      [a, ...acc];
    | (k, Right(a)) =>
      Js.log("Key " ++ string_of_int(k) ++ ", Right " ++ string_of_int(a));
      [a, ...acc];
    | (k, Unequal(a, b)) =>
      Js.log(
        "Key "
        ++ string_of_int(k)
        ++ ", Unequal "
        ++ string_of_int(a)
        ++ ","
        ++ string_of_int(b),
      );
      [a, b, ...acc];
    }
  );
Js.log(Array.of_list(res));