module type Config = {
  type t('el);
  let length: t('el) => int;
  let get: (t('el), int) => 'el;
};

module type Interface = {
  type tSyntax('el);

  module Syntax: {
    module Array: {let get: (tSyntax('el), int) => option('el);};
  };

  module SyntaxNested: {
    module Array: {let get: (option(tSyntax('el)), int) => option('el);};
  };

  module SyntaxExn: {module Array: {let get: (tSyntax('el), int) => 'el;};};

  module SyntaxReset: {module Array: {};};
};

module Add =
       (Config: Config)
       : (Interface with type tSyntax('el) = Config.t('el)) => {
  type tSyntax('el) = Config.t('el);
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
    };
  };

  module SyntaxExn = {
    module Array = {
      let get = Config.get;
    };
  };

  module SyntaxReset = {
    module Array = {};
  };
};
