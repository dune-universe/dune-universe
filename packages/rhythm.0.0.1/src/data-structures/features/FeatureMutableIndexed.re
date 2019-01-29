module type Config = {
  type t('el);

  let length: t('el) => int;
  let getIndexExn: (int, t('el)) => 'el;
  let setIndexExn: (int, 'el, t('el)) => unit;
};

module type Interface = {
  type tMutableIndexed('el);

  let setIndex: (int, 'el, tMutableIndexed('el)) => result(unit, exn);
  let setIndexExn: (int, 'el, tMutableIndexed('el)) => unit;

  let updateIndex:
    (int, 'el => 'el, tMutableIndexed('el)) => result(unit, exn);
  let updateIndexExn: (int, 'el => 'el, tMutableIndexed('el)) => unit;

  let swap: (int, int, tMutableIndexed('el)) => result(unit, exn);
  let swapExn: (int, int, tMutableIndexed('el)) => unit;
};

module Add =
       (Config: Config)
       : (Interface with type tMutableIndexed('el) = Config.t('el)) => {
  type tMutableIndexed('el) = Config.t('el);

  let setIndexExn = Config.setIndexExn;

  let setIndex = (index, el, ds) =>
    try (
      {
        let () = setIndexExn(index, el, ds);
        Ok();
      }
    ) {
    | e => Error(e)
    };

  let updateIndexExn = (index, fn, ds) => {
    let newValue = fn(Config.getIndexExn(index, ds));
    setIndexExn(index, newValue, ds);
  };

  let updateIndex = (index, fn, ds) =>
    try (
      {
        let () = updateIndexExn(index, fn, ds);
        Ok();
      }
    ) {
    | e => Error(e)
    };

  let swapExn = (index1, index2, ds) => {
    let newIndex2 = Config.getIndexExn(index1, ds);
    let newIndex1 = Config.getIndexExn(index2, ds);
    let () = setIndexExn(index2, newIndex2, ds);
    let () = setIndexExn(index1, newIndex1, ds);
    ();
  };

  let swap = (index1, index2, ds) =>
    try (
      {
        let () = swapExn(index1, index2, ds);
        Ok();
      }
    ) {
    | e => Error(e)
    };
};
