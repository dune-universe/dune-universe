type t('el) = array('el);
let make = () => [||];
let length = Caml.Array.length;
let isEmpty = arr => length(arr) === 0;
let toList: t('el) => list('el) = Caml.Array.to_list;
let fromList: list('el) => t('el) = Caml.Array.of_list;
