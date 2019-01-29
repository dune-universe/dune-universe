type t('el);

let make: unit => t('el);
let isEmpty: t('el) => bool;
let length: t('el) => int;

include FeatureSequence.Interface with type tSequence('el) = t('el);

include FeatureFront.Interface with type tFront('el) = t('el);

include FeatureBack.Interface with type tBack('el) = t('el);

include FeatureIndexed.Interface with type tIndexed('el) = t('el);

include
  FeatureMutableIndexed.Interface with type tMutableIndexed('el) = t('el);

include
  FeatureMutableSyntax.Interface with type tMutableSyntax('el) = t('el);
