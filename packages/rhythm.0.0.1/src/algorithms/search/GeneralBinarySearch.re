type directionality =
  | Increasing
  | Decreasing
  | Equal;

let comparisonToDirectionality = (value: int) =>
  switch (value) {
  | _ when value < 0 => Increasing
  | _ when value > 0 => Decreasing
  | _ => Equal
  };

let rec searchInternal =
        (
          ~directionality: directionality,
          ~get: 'key => 'value,
          ~getMiddle: ('key, 'key) => 'key,
          ~continue: ('key, 'key) => bool,
          ~compare: ('value, 'value) => int,
          ~testCompare: 'value => int,
          ~first: 'key,
          ~last: 'key,
          _: unit,
        )
        : option('key) => {
  ();
  if (continue(first, last)) {
    let mid = getMiddle(first, last);
    /* print_int(mid); */
    let midEl = get(mid);
    let midTest = testCompare(midEl);
    let midDir = comparisonToDirectionality(midTest);
    switch (directionality, midDir) {
    | (_, Equal) => Some(mid)
    | (Equal, _) => None
    | (Increasing, Increasing)
    | (Decreasing, Decreasing) =>
      searchInternal(
        ~directionality,
        ~get,
        ~getMiddle,
        ~continue,
        ~compare,
        ~testCompare,
        ~first=mid,
        ~last,
        (),
      )
    | (Increasing, Decreasing)
    | (Decreasing, Increasing) =>
      searchInternal(
        ~directionality,
        ~get,
        ~getMiddle,
        ~continue,
        ~compare,
        ~testCompare,
        ~first,
        ~last=mid,
        (),
      )
    };
  } else {
    let firstEl = get(first);
    let firstTest = testCompare(firstEl);
    let lastEl = get(last);
    let lastTest = testCompare(lastEl);
    switch (firstTest, lastTest) {
    | (0, _) => Some(first)
    | (_, 0) => Some(last)
    | _ => None
    };
  };
};

let search =
    (
      ~get: 'key => 'value,
      ~getMiddle: ('key, 'key) => 'key,
      ~continue: ('key, 'key) => bool,
      ~compare: ('value, 'value) => int,
      ~testCompare: 'value => int,
      ~first: 'key,
      ~last: 'key,
      _: unit,
    )
    : option('key) => {
  let firstEl = get(first);
  let lastEl = get(last);
  let firstTest = testCompare(firstEl);
  let lastTest = testCompare(lastEl);
  let d = compare(firstEl, lastEl);
  let d = comparisonToDirectionality(d);
  let shouldContinue = continue(first, last);
  switch (d, firstTest, lastTest, shouldContinue) {
  | (_, 0, _, _) => Some(first)
  | (_, _, 0, _) => Some(last)
  | (_, _, _, false) => None

  | (Increasing, _, _, _) when firstTest > 0 => None
  | (Increasing, _, _, _) when lastTest < 0 => None
  | (Decreasing, _, _, _) when firstTest < 0 => None
  | (Decreasing, _, _, _) when lastTest > 0 => None
  | (Equal, _, _, _) => None
  | _ =>
    searchInternal(
      ~directionality=d,
      ~get,
      ~getMiddle,
      ~continue,
      ~compare,
      ~testCompare,
      ~first,
      ~last,
      (),
    )
  };
};
