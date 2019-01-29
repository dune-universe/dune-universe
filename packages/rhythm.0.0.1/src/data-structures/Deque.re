include DequeCore;

include FeatureSequence.Add({
  open FeatureSequence;
  include Default;
  include DequeCore;
  let fastReverse = Reverse(DequeCore.reverse);
});

include FeatureFront.Add({
  open FeatureFront;
  include Default;
  include DequeCore;
  let fastGetFirst = GetFirstExn(DequeCore.getFirstExn);
  let fastAddFirst = AddFirst(DequeCore.addFirst);
  let fastRemoveFirst = RemoveFirstExn(DequeCore.removeFirstExn);
});

include FeatureBack.Add({
  open FeatureBack;
  include Default;
  include DequeCore;
  let fastGetLast = GetLastExn(DequeCore.getLastExn);
  let fastAddLast = AddLast(DequeCore.addLast);
  let fastRemoveLast = RemoveLastExn(DequeCore.removeLastExn);
});
