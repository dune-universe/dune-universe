class ['self] map_from_fold : object ('self)
  method private visit_option : 'a .
    ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
  method private build_None : 'env -> 'b option
  method private build_Some : 'env -> 'b -> 'b option
end
