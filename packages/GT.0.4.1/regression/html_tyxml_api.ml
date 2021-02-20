module H = Tyxml.Html

let int =
  { GT.gcata = GT.gcata_int;
    GT.plugins = object
      method show = GT.int.GT.plugins#show
      method gmap = GT.int.GT.plugins#gmap
      method compare = GT.int.GT.plugins#compare
      method eq      = GT.int.GT.plugins#eq
      method foldl = GT.int.GT.plugins#foldr
      method foldr = GT.int.GT.plugins#foldl
      method stateful = GT.int.GT.plugins#stateful
      method eval = GT.int.GT.plugins#eval
      method html n = Tyxml_html.pcdata (string_of_int n)
    end
  }
let float =
  { GT.gcata = GT.gcata_float;
    GT.plugins = object
      method show = GT.float.GT.plugins#show
      method gmap = GT.float.GT.plugins#gmap
      method compare = GT.float.GT.plugins#compare
      method eq      = GT.float.GT.plugins#eq
      method foldl = GT.float.GT.plugins#foldr
      method foldr = GT.float.GT.plugins#foldl
      method stateful = GT.float.GT.plugins#stateful
      method eval = GT.float.GT.plugins#eval
      method html n = Tyxml_html.pcdata (string_of_float n)
    end
  }

let string =
  { GT.gcata = GT.gcata_string;
    GT.plugins = object
      method show = GT.string.GT.plugins#show
      method gmap = GT.string.GT.plugins#gmap
      method compare = GT.string.GT.plugins#compare
      method eq      = GT.string.GT.plugins#eq
      method foldl = GT.string.GT.plugins#foldr
      method foldr = GT.string.GT.plugins#foldl
      method stateful = GT.string.GT.plugins#stateful
      method eval     = GT.string.GT.plugins#eval
      method show_typed x = GT.string.GT.plugins#show x
      method html         = Tyxml_html.pcdata
    end
  }

class ['a, 'b, 'self, 'html] html_tuple2_t _ fa fb =
  object
    inherit [ unit, 'a, 'syn
            , unit, 'b, 'syn
            , unit, 'self, 'syn] GT.pair_t
    constraint 'syn = 'html
    method c_Pair () x y = Tyxml.Html.div [fa x; fb y]
  end

class ['a, 'self, 'html] html_list_t fself fa = object
  inherit ['inh, 'a, 'syn, 'inh, 'self, 'html H.elt] GT.list_t
  method c_Nil () = H.div [H.pcdata "[]"]
  method c_Cons () x xs = H.div [fa x; H.pcdata "::"; fself xs]
end

class ['a, 'self, 'html] html_option_t _fself fa = object
  inherit [unit, 'a, 'syn, unit, 'self, 'syn] GT.option_t
  constraint 'syn = 'html H.elt
  method c_None ()   = H.pcdata "None"
  method c_Some () x = H.div [H.pcdata "Some"; fa x]
end

let tuple2 =
  { GT.gcata = GT.gcata_pair;
    GT.plugins = object
      method show = GT.tuple2.GT.plugins#show
      method html fa fb t =
        GT.fix0 (fun fself -> GT.tuple2.GT.gcata (new html_tuple2_t fself fa fb)) t
      method gmap = GT.tuple2.GT.plugins#gmap
      method compare = GT.tuple2.GT.plugins#compare
      method eq      = GT.tuple2.GT.plugins#eq
      method foldl = GT.tuple2.GT.plugins#foldr
      method foldr = GT.tuple2.GT.plugins#foldl
      method stateful = GT.tuple2.GT.plugins#stateful
      method eval = GT.tuple2.GT.plugins#eval
    end
  }
