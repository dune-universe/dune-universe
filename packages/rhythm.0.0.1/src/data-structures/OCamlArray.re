include OCamlArrayCore;

include FeatureMutableSyntax.Add({
  include OCamlArrayCore;
  let get = Caml.Array.get;
  let set = Caml.Array.set;
});

include FeatureSequence.Add({
  open FeatureSequence;
  include Default;
  include OCamlArrayCore;
});

include FeatureFront.Add({
  open FeatureFront;
  include Default;
  include OCamlArrayCore;
  let fastGetFirst = GetFirstExn(arr => SyntaxExn.(arr[0]));
});

include FeatureBack.Add({
  open FeatureBack;
  include Default;
  include OCamlArrayCore;
  let fastGetLast = GetLastExn(arr => SyntaxExn.(arr[length(arr) - 1]));
});

include FeatureIndexed.Add({
  include OCamlArrayCore;
  let getIndexExn = (i, arr) => SyntaxExn.(arr[i]);
});

include FeatureMutableIndexed.Add({
  include OCamlArrayCore;
  let getIndexExn = (i, arr) => SyntaxExn.(arr[i]);
  let setIndexExn = (i, value, arr) => SyntaxExn.(arr[i] = value);
});
