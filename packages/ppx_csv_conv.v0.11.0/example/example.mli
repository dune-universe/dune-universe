open! Core

type t = {
  a : float;
  b : string;
  c : int;
  d : Date.t;
} [@@deriving fields, csv]
