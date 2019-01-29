include OCamlListCore;

include FeatureSyntax.Add({
  include OCamlListCore;
  let get = Caml.List.nth;
});

include FeatureSequence.Add({
  open FeatureSequence;
  include Default;
  include OCamlListCore;
});

include FeatureFront.Add({
  open FeatureFront;
  include Default;
  include OCamlListCore;
  let fastGetFirst = GetFirstExn(OCamlListCore.getFirstExn);
  let fastAddFirst = AddFirst(OCamlListCore.addFirst);
  let fastRemoveFirst = RemoveFirstExn(OCamlListCore.removeFirstExn);
});

include FeatureBack.Add({
  open FeatureBack;
  include Default;
  include OCamlListCore;
});

include FeatureIndexed.Add({
  include OCamlListCore;
  let getIndexExn = (i, ds) => SyntaxExn.(ds[i]);
});
