type b = int * int [@@deriving make]

type o = int option * int option [@@deriving make]

type l = int list * int list [@@deriving make]

type s = string * string [@@deriving make]

type d = (int[@default 42]) * (int[@default 420]) [@@deriving make]

type r = int option * (string[@required]) * (int option[@required])
[@@deriving make]

type complex =
  int
  * int option
  * int list
  * string
  * (int[@default 1024])
  * (string[@required])
[@@deriving make]
