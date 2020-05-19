module%override Lexing = struct
  type position = _ [@@deriving show]
end

module%override Longident = struct
  [%%types] [@@deriving show]
end

module%override Location = struct
  [%%types] [@@deriving show]
end

module%override Asttypes = struct
  [%%types] [@@deriving show]
end

module%override Parsetree = struct
  [%%types] [@@deriving show]
end
