module A = struct module B = struct type t = char
                                    let default = 'c' end end
module Types :
  sig
    type simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    include
      sig
        val simple_record_factory :
          ?int_field:int ->
            ?string_field:string ->
              ?other_field:A.B.t -> unit -> simple_record
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type simple_variant =
      | A 
      | B of int 
      | C of int * string 
      | D of {
      int_field: int ;
      string_field: string } [@@deriving factory]
    include
      sig
        val simple_variant_a_factory : unit -> simple_variant
        val simple_variant_b_factory : ?tup0:int -> unit -> simple_variant
        val simple_variant_c_factory :
          ?tup0:int -> ?tup1:string -> unit -> simple_variant
        val simple_variant_d_factory :
          ?int_field:int -> ?string_field:string -> unit -> simple_variant
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type record_with_options =
      {
      non_optional: int ;
      optional: int option ;
      nested: int option option }[@@deriving factory]
    include
      sig
        val record_with_options_factory :
          ?non_optional:int ->
            ?optional:int ->
              ?nested:int option -> unit -> record_with_options
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type 'a parametrized = {
      param: 'a option ;
      non_paramed: string }[@@deriving factory]
    include
      sig
        val parametrized_factory :
          ?param:'a -> ?non_paramed:string -> unit -> 'a parametrized
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type copied = simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    include
      sig
        val copied_factory :
          ?int_field:int ->
            ?string_field:string -> ?other_field:A.B.t -> unit -> copied
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    include
      struct
        let simple_record_factory ?(int_field= 0)  ?(string_field= "") 
          ?(other_field= A.B.default)  () =
          { int_field; string_field; other_field }
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type simple_variant =
      | A 
      | B of int 
      | C of int * string 
      | D of {
      int_field: int ;
      string_field: string } [@@deriving factory]
    include
      struct
        let simple_variant_a_factory () = A
        let simple_variant_b_factory ?(tup0= 0)  () = B tup0
        let simple_variant_c_factory ?(tup0= 0)  ?(tup1= "")  () =
          C (tup0, tup1)
        let simple_variant_d_factory ?(int_field= 0)  ?(string_field= "")  ()
          = D { int_field; string_field }
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type record_with_options =
      {
      non_optional: int ;
      optional: int option ;
      nested: int option option }[@@deriving factory]
    include
      struct
        let record_with_options_factory ?(non_optional= 0)  ?optional 
          ?nested  () = { non_optional; optional; nested }
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type 'a parametrized = {
      param: 'a option ;
      non_paramed: string }[@@deriving factory]
    include
      struct
        let parametrized_factory ?param  ?(non_paramed= "")  () =
          { param; non_paramed }
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type copied = simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    include
      struct
        let copied_factory ?(int_field= 0)  ?(string_field= "") 
          ?(other_field= A.B.default)  () =
          { int_field; string_field; other_field }
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
