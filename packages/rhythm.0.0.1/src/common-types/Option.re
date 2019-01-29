type t('value) = option('value);

let isSome = o =>
  switch (o) {
  | Some(_) => true
  | None => false
  };

let isNone = o =>
  switch (o) {
  | Some(_) => false
  | None => true
  };

let getWithDefault = (default, o) =>
  switch (o) {
  | Some(value) => value
  | None => default
  };

let getExn = o =>
  switch (o) {
  | Some(value) => value
  | None => raise(Exceptions.UnexpectedNone("Option.getValueExn"))
  };

let makeNone = _ => None;

let makeSome = value => Some(value);

let map = (f, o) =>
  switch (o) {
  | Some(value) => Some(f(value))
  | None => None
  };

let mapExn = (f, o) =>
  switch (o) {
  | Some(value) => f(value)
  | None => raise(Exceptions.UnexpectedNone("Option.getValueExn"))
  };

let mapWithDefault = (default, f, o) =>
  switch (o) {
  | Some(value) => f(value)
  | None => default
  };

let flatMap = (f, o) =>
  switch (o) {
  | Some(value) => f(value)
  | None => None
  };

module Infix = {
  let (|?:) = (o, default) =>
    switch (o) {
    | Some(value) => value
    | None => default
    };

  let (>>|) = (opt, fn) =>
    switch (opt) {
    | Some(value) => Some(fn(value))
    | None => None
    };

  let (>>=) = (opt, fn) =>
    switch (opt) {
    | Some(value) => fn(value)
    | None => None
    };
};
