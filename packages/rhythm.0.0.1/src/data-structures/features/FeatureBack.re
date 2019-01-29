type fastGetLast('ds, 'el) =
  | SlowGetLast
  | GetLastExn('ds => 'el);

type fastAddLast('ds, 'el) =
  | SlowAddLast
  | AddLast(('el, 'ds) => 'ds);

type fastRemoveLast('ds, 'el) =
  | SlowRemoveLast
  | RemoveLastExn('ds => 'ds);

module type Config = {
  type t('el);

  let toList: t('el) => list('el);
  let fromList: list('el) => t('el);

  let isEmpty: t('el) => bool;
  let fastGetLast: fastGetLast(t('el), 'el);
  let fastAddLast: fastAddLast(t('el), 'el);
  let fastRemoveLast: fastRemoveLast(t('el), 'el);
};

module Default = {
  let fastGetLast = SlowGetLast;
  let fastAddLast = SlowAddLast;
  let fastRemoveLast = SlowRemoveLast;
};

module type Interface = {
  type tBack('el);

  let getLast: tBack('el) => option('el);
  let getLastExn: tBack('el) => 'el;

  let getLastN: (int, tBack('el)) => option(tBack('el));
  let getLastNExn: (int, tBack('el)) => tBack('el);

  let addLast: ('el, tBack('el)) => tBack('el);

  let removeLast: tBack('el) => option(tBack('el));
  let removeLastExn: tBack('el) => tBack('el);

  let removeLastN: (int, tBack('el)) => option(tBack('el));
  let removeLastNExn: (int, tBack('el)) => tBack('el);

  let updateLast: ('el => 'el, tBack('el)) => option(tBack('el));
  let updateLastExn: ('el => 'el, tBack('el)) => tBack('el);
};

module Add =
       (Config: Config)
       : (Interface with type tBack('el) = Config.t('el)) => {
  type tBack('el) = Config.t('el);

  let getLastExn =
    switch (Config.fastGetLast) {
    | GetLastExn(getLastExn) => getLastExn
    | _ => (
        ds =>
          ds |> Config.toList |> Caml.List.rev |> OCamlListCore.getFirstExn
      )
    };

  let getLast = ds =>
    try (Some(getLastExn(ds))) {
    | _ => None
    };

  let getLastNExn =
    switch (Config.fastGetLast, Config.fastRemoveLast) {
    | (GetLastExn(getLastExn), RemoveLastExn(removeLastExn)) => (
        (count, ds) => {
          let ds = ref(ds);
          let result = ref([]);
          for (i in 0 to count - 1) {
            result := [getLastExn(ds^), ...result^];
            ds := removeLastExn(ds^);
          };
          result^ |> Config.fromList;
        }
      )
    | _ => (
        (count, ds) => {
          let list = ds |> Config.toList |> Caml.List.rev;
          let list = ref(list);
          let result = ref([]);
          for (i in 0 to count - 1) {
            result := [OCamlListCore.getFirstExn(list^), ...result^];
            list := OCamlListCore.removeFirstExn(list^);
          };
          result^ |> Config.fromList;
        }
      )
    };

  let getLastN = (count, ds) =>
    try (Some(getLastNExn(count, ds))) {
    | _ => None
    };

  let addLast =
    switch (Config.fastAddLast) {
    | AddLast(addLast) => addLast
    | _ => (
        (el, ds) => {
          let list = Config.toList(ds);
          let list = list @ [el];
          Config.fromList(list);
        }
      )
    };

  let removeLastExn =
    switch (Config.fastRemoveLast) {
    | RemoveLastExn(removeLastExn) => removeLastExn
    | _ => (
        ds => {
          let list = Config.toList(ds);
          let list = Caml.List.rev(list);
          let list =
            switch (list) {
            | [] => raise(Exceptions.Empty("FeatureBack.removeLastExn"))
            | [_, ...rest] => rest
            };
          list |> Caml.List.rev |> Config.fromList;
        }
      )
    };

  let removeLastNExn =
    switch (Config.fastRemoveLast) {
    | RemoveLastExn(removeLastExn) => (
        (count, ds) => {
          let ds = ref(ds);
          for (i in 0 to count - 1) {
            ds := removeLastExn(ds^);
          };
          ds^;
        }
      )
    | _ => (
        (count, ds) => {
          let list = Config.toList(ds);
          let list = Caml.List.rev(list);
          let list = ref(list);
          for (i in 0 to count - 1) {
            list := OCamlListCore.removeFirstExn(list^);
          };
          list^ |> Caml.List.rev |> Config.fromList;
        }
      )
    };

  let removeLastN = (count, ds) =>
    try (Some(removeLastNExn(count, ds))) {
    | _ => None
    };

  let removeLast = ds =>
    try (Some(removeLastExn(ds))) {
    | _ => None
    };

  let updateLastExn =
    switch (Config.fastGetLast, Config.fastAddLast, Config.fastRemoveLast) {
    | (
        GetLastExn(getLastExn),
        AddLast(addLast),
        RemoveLastExn(removeLastExn),
      ) => (
        (fn, ds) =>
          if (Config.isEmpty(ds)) {
            raise(Exceptions.Empty("FeatureBack.updateLastExn"));
          } else {
            let first = getLastExn(ds);
            let rest = removeLastExn(ds);
            let newLast = fn(first);
            addLast(newLast, rest);
          }
      )
    | _ => (
        (fn, ds) => {
          let list = Config.toList(ds);
          let list = Caml.List.rev(list);
          let list =
            switch (list) {
            | [] => raise(Exceptions.Empty("FeatureBack.updateLastExn"))
            | [hd, ...rest] => [fn(hd), ...rest]
            };
          list |> Caml.List.rev |> Config.fromList;
        }
      )
    };

  let updateLast = (fn, ds) =>
    try (Some(updateLastExn(fn, ds))) {
    | _ => None
    };
};
