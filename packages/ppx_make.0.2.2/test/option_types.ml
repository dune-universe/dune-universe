type a = int option [@@deriving make]

type d = (int option[@default 7]) [@@deriving make]
