type t('el) = list('el);

let make: unit => t('el);
let isEmpty: t('el) => bool;
let length: t('el) => int;

include FeatureSyntax.Interface with type tSyntax('el) = t('el);

include FeatureSequence.Interface with type tSequence('el) = t('el);

include FeatureFront.Interface with type tFront('el) = t('el);

include FeatureBack.Interface with type tBack('el) = t('el);

include FeatureIndexed.Interface with type tIndexed('el) = t('el);
