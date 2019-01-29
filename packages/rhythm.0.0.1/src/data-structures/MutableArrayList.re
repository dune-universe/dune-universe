include MutableArrayListCore;

include FeatureSequence.Add({
  open FeatureSequence;
  include Default;
  include MutableArrayListCore;
});

include FeatureMutableSyntax.Add({
  include MutableArrayListCore;
  let get = (ds, index) => MutableArrayListCore.getIndexExn(index, ds);
  let set = (ds, index, el) =>
    MutableArrayListCore.setIndexExn(index, el, ds);
});

include FeatureFront.Add({
  open FeatureFront;
  include Default;
  include MutableArrayListCore;
  let fastGetFirst = GetFirstExn(ds => getIndexExn(0, ds));
  let fastAddFirst = AddFirst(addFirst);
  let fastRemoveFirst = RemoveFirstExn(removeFirstExn);
});

include FeatureBack.Add({
  open FeatureBack;
  include Default;
  include MutableArrayListCore;
  let fastGetLast = GetLastExn(ds => getIndexExn(length(ds) - 1, ds));
  let fastAddLast = AddLast(addLast);
  let fastRemoveLast = RemoveLastExn(removeLastExn);
});

include FeatureIndexed.Add({
  include MutableArrayListCore;
});

include FeatureMutableIndexed.Add({
  include MutableArrayListCore;
});
