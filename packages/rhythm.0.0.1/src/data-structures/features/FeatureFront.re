type fastGetFirst('ds, 'el) =
  | SlowGetFirst
  | GetFirstExn('ds => 'el);

type fastAddFirst('ds, 'el) =
  | SlowAddFirst
  | AddFirst(('el, 'ds) => 'ds);

type fastRemoveFirst('ds, 'el) =
  | SlowRemoveFirst
  | RemoveFirstExn('ds => 'ds);

module type Config = {
  type t('el);

  let toList: t('el) => list('el);
  let fromList: list('el) => t('el);

  let isEmpty: t('el) => bool;
  let fastGetFirst: fastGetFirst(t('el), 'el);
  let fastAddFirst: fastAddFirst(t('el), 'el);
  let fastRemoveFirst: fastRemoveFirst(t('el), 'el);
};

module Default = {
  let fastGetFirst = SlowGetFirst;
  let fastAddFirst = SlowAddFirst;
  let fastRemoveFirst = SlowRemoveFirst;
};

module type Interface = {
  type tFront('el);

  let getFirst: tFront('el) => option('el);
  let getFirstExn: tFront('el) => 'el;

  let getFirstN: (int, tFront('el)) => option(tFront('el));
  let getFirstNExn: (int, tFront('el)) => tFront('el);

  let addFirst: ('el, tFront('el)) => tFront('el);

  let removeFirst: tFront('el) => option(tFront('el));
  let removeFirstExn: tFront('el) => tFront('el);

  let removeFirstN: (int, tFront('el)) => option(tFront('el));
  let removeFirstNExn: (int, tFront('el)) => tFront('el);

  let updateFirst: ('el => 'el, tFront('el)) => option(tFront('el));
  let updateFirstExn: ('el => 'el, tFront('el)) => tFront('el);

  let match1Exn: tFront('el) => ('el, tFront('el));
  let match2Exn: tFront('el) => ('el, 'el, tFront('el));
  let match3Exn: tFront('el) => ('el, 'el, 'el, tFront('el));
  let match4Exn: tFront('el) => ('el, 'el, 'el, 'el, tFront('el));
  let match5Exn: tFront('el) => ('el, 'el, 'el, 'el, 'el, tFront('el));
  let match6Exn: tFront('el) => ('el, 'el, 'el, 'el, 'el, 'el, tFront('el));
  let match7Exn:
    tFront('el) => ('el, 'el, 'el, 'el, 'el, 'el, 'el, tFront('el));

  let match1: tFront('el) => option(('el, tFront('el)));
  let match2: tFront('el) => option(('el, 'el, tFront('el)));
  let match3: tFront('el) => option(('el, 'el, 'el, tFront('el)));
  let match4: tFront('el) => option(('el, 'el, 'el, 'el, tFront('el)));
  let match5:
    tFront('el) => option(('el, 'el, 'el, 'el, 'el, tFront('el)));
  let match6:
    tFront('el) => option(('el, 'el, 'el, 'el, 'el, 'el, tFront('el)));
  let match7:
    tFront('el) => option(('el, 'el, 'el, 'el, 'el, 'el, 'el, tFront('el)));
};

module Add =
       (Config: Config)
       : (Interface with type tFront('el) = Config.t('el)) => {
  type tFront('el) = Config.t('el);

  let getFirstExn =
    switch (Config.fastGetFirst) {
    | GetFirstExn(getFirstExn) => getFirstExn
    | _ => (
        ds => {
          let list = Config.toList(ds);
          OCamlListCore.getFirstExn(list);
        }
      )
    };

  let getFirst = ds =>
    try (Some(getFirstExn(ds))) {
    | _ => None
    };

  let getFirstNExn =
    switch (Config.fastGetFirst, Config.fastRemoveFirst) {
    | (GetFirstExn(getFirstExn), RemoveFirstExn(removeFirstExn)) => (
        (count, ds) => {
          let ds = ref(ds);
          let result = ref([]);
          for (_i in 0 to count - 1) {
            result := [getFirstExn(ds^), ...result^];
            ds := removeFirstExn(ds^);
          };
          result^ |> Caml.List.rev |> Config.fromList;
        }
      )
    | _ => (
        (count, ds) => {
          let list = Config.toList(ds);
          let list = ref(list);
          let result = ref([]);
          for (_i in 0 to count - 1) {
            result := [OCamlListCore.getFirstExn(list^), ...result^];
            list := OCamlListCore.removeFirstExn(list^);
          };
          result^ |> Caml.List.rev |> Config.fromList;
        }
      )
    };

  let getFirstN = (count, ds) =>
    try (Some(getFirstNExn(count, ds))) {
    | _ => None
    };

  let addFirst =
    switch (Config.fastAddFirst) {
    | AddFirst(addFirst) => addFirst
    | _ => (
        (el, ds) => {
          let list = Config.toList(ds);
          let list = [el, ...list];
          Config.fromList(list);
        }
      )
    };

  let removeFirstExn =
    switch (Config.fastRemoveFirst) {
    | RemoveFirstExn(removeFirstExn) => removeFirstExn
    | _ => (
        ds => {
          let list = Config.toList(ds);
          switch (list) {
          | [] => raise(Exceptions.Empty("FeatureFront.removeFirstExn"))
          | [_, ...rest] => Config.fromList(rest)
          };
        }
      )
    };

  let removeFirstNExn =
    switch (Config.fastRemoveFirst) {
    | RemoveFirstExn(removeFirstExn) => (
        (count, ds) => {
          let ds = ref(ds);
          for (_i in 0 to count - 1) {
            ds := removeFirstExn(ds^);
          };
          ds^;
        }
      )
    | _ => (
        (count, ds) => {
          let list = Config.toList(ds);
          let list = ref(list);
          for (_i in 0 to count - 1) {
            list := OCamlListCore.removeFirstExn(list^);
          };
          list^ |> Config.fromList;
        }
      )
    };

  let removeFirstN = (count, ds) =>
    try (Some(removeFirstNExn(count, ds))) {
    | _ => None
    };

  let removeFirst = ds =>
    try (Some(removeFirstExn(ds))) {
    | _ => None
    };

  let updateFirstExn =
    switch (Config.fastGetFirst, Config.fastAddFirst, Config.fastRemoveFirst) {
    | (
        GetFirstExn(getFirstExn),
        AddFirst(addFirst),
        RemoveFirstExn(removeFirstExn),
      ) => (
        (fn, ds) =>
          if (Config.isEmpty(ds)) {
            raise(Exceptions.Empty("FeatureFront.updateFirstExn"));
          } else {
            let first = getFirstExn(ds);
            let rest = removeFirstExn(ds);
            let newFirst = fn(first);
            addFirst(newFirst, rest);
          }
      )
    | _ => (
        (fn, ds) => {
          let list = Config.toList(ds);
          switch (list) {
          | [] => raise(Exceptions.Empty("FeatureFront.updateFirstExn"))
          | [hd, ...rest] => Config.fromList([fn(hd), ...rest])
          };
        }
      )
    };

  let updateFirst = (fn, ds) =>
    try (Some(updateFirstExn(fn, ds))) {
    | _ => None
    };

  let match1Exn = ds => {
    let front = ds |> getFirstNExn(1) |> Config.toList;
    let rest = ds |> removeFirstNExn(1);
    switch (front) {
    | [el1] => (el1, rest)
    | _ => raise(Exceptions.InternalError("FeatureFront.matchNExn"))
    };
  };

  let match2Exn = ds => {
    let front = ds |> getFirstNExn(2) |> Config.toList;
    let rest = ds |> removeFirstNExn(2);
    switch (front) {
    | [el1, el2] => (el1, el2, rest)
    | _ => raise(Exceptions.InternalError("FeatureFront.matchNExn"))
    };
  };

  let match3Exn = ds => {
    let front = ds |> getFirstNExn(3) |> Config.toList;
    let rest = ds |> removeFirstNExn(3);
    switch (front) {
    | [el1, el2, el3] => (el1, el2, el3, rest)
    | _ => raise(Exceptions.InternalError("FeatureFront.matchNExn"))
    };
  };

  let match4Exn = ds => {
    let front = ds |> getFirstNExn(4) |> Config.toList;
    let rest = ds |> removeFirstNExn(4);
    switch (front) {
    | [el1, el2, el3, el4] => (el1, el2, el3, el4, rest)
    | _ => raise(Exceptions.InternalError("FeatureFront.matchNExn"))
    };
  };

  let match5Exn = ds => {
    let front = ds |> getFirstNExn(5) |> Config.toList;
    let rest = ds |> removeFirstNExn(5);
    switch (front) {
    | [el1, el2, el3, el4, el5] => (el1, el2, el3, el4, el5, rest)
    | _ => raise(Exceptions.InternalError("FeatureFront.matchNExn"))
    };
  };

  let match6Exn = ds => {
    let front = ds |> getFirstNExn(6) |> Config.toList;
    let rest = ds |> removeFirstNExn(6);
    switch (front) {
    | [el1, el2, el3, el4, el5, el6] => (el1, el2, el3, el4, el5, el6, rest)
    | _ => raise(Exceptions.InternalError("FeatureFront.matchNExn"))
    };
  };

  let match7Exn = ds => {
    let front = ds |> getFirstNExn(7) |> Config.toList;
    let rest = ds |> removeFirstNExn(7);
    switch (front) {
    | [el1, el2, el3, el4, el5, el6, el7] => (
        el1,
        el2,
        el3,
        el4,
        el5,
        el6,
        el7,
        rest,
      )
    | _ => raise(Exceptions.InternalError("FeatureFront.matchNExn"))
    };
  };

  let match1 = ds =>
    try (Some(match1Exn(ds))) {
    | _ => None
    };

  let match2 = ds =>
    try (Some(match2Exn(ds))) {
    | _ => None
    };

  let match3 = ds =>
    try (Some(match3Exn(ds))) {
    | _ => None
    };

  let match4 = ds =>
    try (Some(match4Exn(ds))) {
    | _ => None
    };

  let match5 = ds =>
    try (Some(match5Exn(ds))) {
    | _ => None
    };

  let match6 = ds =>
    try (Some(match6Exn(ds))) {
    | _ => None
    };

  let match7 = ds =>
    try (Some(match7Exn(ds))) {
    | _ => None
    };
};
