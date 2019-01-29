module type Config = {
  type t('el);
  let length: t('el) => int;
  let get: (t('el), int) => 'el;
  let set: (t('el), int, 'el) => unit;
};

module type Interface = {
  type tMutableSyntax('el);

  module Syntax: {
    module Array: {
      let get: (tMutableSyntax('el), int) => option('el);
      let set: (tMutableSyntax('el), int, 'el) => result(unit, exn);
    };
  };

  module SyntaxNested: {
    module Array: {
      let get: (option(tMutableSyntax('el)), int) => option('el);
      let set: (option(tMutableSyntax('el)), int, 'el) => result(unit, exn);
    };
  };

  module SyntaxExn: {
    module Array: {
      let get: (tMutableSyntax('el), int) => 'el;
      let set: (tMutableSyntax('el), int, 'el) => unit;
    };
  };

  module SyntaxReset: {module Array: {};};
};

module Add =
       (Config: Config)
       : (Interface with type tMutableSyntax('el) = Config.t('el)) => {
  type tMutableSyntax('el) = Config.t('el);
  module Syntax = {
    module Array = {
      let get = (ds, i) => {
        let n = Config.length(ds);
        if (i < 0 || i >= n) {
          None;
        } else {
          Some(Config.get(ds, i));
        };
      };

      let set = (ds, i, value) => {
        let n = Config.length(ds);
        if (i < 0 || i >= n) {
          Error(Exceptions.IndexOutOfBounds("Syntax.set", i, 0, n));
        } else {
          Config.set(ds, i, value);
          Ok();
        };
      };
    };
  };

  module SyntaxNested = {
    module Array = {
      let get = (optDs, i) =>
        switch (optDs) {
        | Some(ds) =>
          let n = Config.length(ds);
          if (i < 0 || i >= n) {
            None;
          } else {
            Some(Config.get(ds, i));
          };
        | None => None
        };

      let set = (optDs, i, value) =>
        switch (optDs) {
        | Some(ds) =>
          let n = Config.length(ds);
          if (i < 0 || i >= n) {
            Error(Exceptions.IndexOutOfBounds("NestedSyntax.set", i, 0, n));
          } else {
            Config.set(ds, i, value);
            Ok();
          };
        | None => Error(Exceptions.UnexpectedNone("NestedSyntax.set"))
        };
    };
  };

  module SyntaxExn = {
    module Array = {
      let get = Config.get;
      let set = Config.set;
    };
  };

  module SyntaxReset = {
    module Array = {};
  };
};
