module%override Lexing : sig
  type position = _ [@@rewrite] [@@deriving show]
end

module%override Longident : sig
  [%%types] [@@deriving show]
end

module%override Location : sig
  [%%types] [@@deriving show]
end

module%override Asttypes : sig
  [%%types] [@@deriving show]
end

module%override Parsetree : sig
  [%%types] [@@deriving show]
end
