type t('el) = {
  mutable first: int,
  mutable length: int,
  mutable capacity: int,
  mutable data: array(option('el)),
};

let make = () => {
  first: 0,
  length: 0,
  capacity: 64,
  data: Caml.Array.init(64, _ => None),
};

let length = ds => ds.length;
let isEmpty = ds => length(ds) === 0;

let rawi = (i, ds) => {
  let i = ds.first + i;
  let i = i + ds.capacity;
  let i = i mod ds.capacity;
  i;
};

let ensureCapacity = ds =>
  if (ds.length >= ds.capacity) {
    let arr =
      Caml.Array.init(ds.capacity * 2, i =>
        if (i < ds.length) {
          ds.data[rawi(i, ds)];
        } else {
          None;
        }
      );
    ds.first = 0;
    ds.capacity = Caml.Array.length(arr);
    ds.data = arr;
    ();
  };

let toList: t('el) => list('el) =
  ds => {
    let arr =
      Caml.Array.init(ds.length, i => Option.getExn(ds.data[rawi(i, ds)]));
    Caml.Array.to_list(arr);
  };

let fromList: list('el) => t('el) =
  list => {
    let arr = list |> Caml.Array.of_list |> Caml.Array.map(el => Some(el));
    let ds = {
      first: 0,
      length: Caml.Array.length(arr),
      capacity: Caml.Array.length(arr),
      data: arr,
    };
    ds;
  };

let getIndexExn = (index, ds) => {
  ();
  if (index < 0 || index >= ds.length) {
    raise(
      Exceptions.IndexOutOfBounds(
        "MutableArrayListCore.getIndexExn",
        index,
        0,
        ds.length,
      ),
    );
  } else {
    Option.getExn(ds.data[rawi(index, ds)]);
  };
};

let setIndexExn = (index, el, ds) => {
  ();
  if (index < 0 || index >= ds.length) {
    raise(
      Exceptions.IndexOutOfBounds(
        "MutableArrayListCore.setIndexExn",
        index,
        0,
        ds.length,
      ),
    );
  } else {
    ds.data[rawi(index, ds)] = Some(el);
  };
};

let addFirst = (el, ds) => {
  ensureCapacity(ds);
  let i = rawi(ds.first - 1, ds);
  ds.data[i] = Some(el);
  ds.first = ds.first - 1;
  if (ds.first < 0) {
    ds.first = ds.first + ds.capacity;
  };
  ds.length = ds.length + 1;
  ds;
};

let removeFirstExn = ds =>
  if (ds.length === 0) {
    raise(Exceptions.Empty("MutableArrayListCore.removeFirstExn"));
  } else {
    let i = rawi(0, ds);
    ds.data[i] = None;
    ds.first = ds.first + 1;
    ds.length = ds.length - 1;
    ds;
  };

let addLast = (el, ds) => {
  ensureCapacity(ds);
  let i = rawi(ds.first + ds.length, ds);
  ds.data[i] = Some(el);
  ds.length = ds.length + 1;
  ds;
};

let removeLastExn = ds =>
  if (ds.length === 0) {
    raise(Exceptions.Empty("MutableArrayListCore.removeLastExn"));
  } else {
    let i = rawi(length(ds) - 1, ds);
    ds.data[i] = None;
    ds.length = ds.length - 1;
    ds;
  };
