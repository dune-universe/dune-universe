type t('value) = option('value);

let flatMap: ('a => t('b), t('a)) => t('b);
let getExn: t('value) => 'value;
let getWithDefault: ('value, t('value)) => 'value;
let isNone: t('value) => bool;
let isSome: t('value) => bool;
let makeNone: _ => t('value);
let makeSome: 'value => t('value);
let map: ('a => 'b, t('a)) => t('b);
let mapExn: ('a => 'b, t('a)) => 'b;
let mapWithDefault: ('b, 'a => 'b, t('a)) => 'b;

module Infix: {
  let (|?:): (option('value), 'value) => 'value;
  let (>>|): (option('value), 'value => 'result) => option('result);
  let (>>=): (option('value), 'value => option('result)) => option('result);
};
