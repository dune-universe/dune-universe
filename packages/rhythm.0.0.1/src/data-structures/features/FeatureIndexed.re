module type Config = {
  type t('el);
  let getIndexExn: (int, t('el)) => 'el;
};

module type Interface = {
  type tIndexed('el);

  let getIndex: (int, tIndexed('el)) => option('el);
  let getIndexExn: (int, tIndexed('el)) => 'el;
};

module Add =
       (Config: Config)
       : (Interface with type tIndexed('el) = Config.t('el)) => {
  type tIndexed('el) = Config.t('el);

  let getIndexExn = Config.getIndexExn;
  let getIndex = (index, ds) =>
    try (Some(getIndexExn(index, ds))) {
    | _ => None
    };
};
