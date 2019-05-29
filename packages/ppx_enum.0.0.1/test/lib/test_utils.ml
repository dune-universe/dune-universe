open Ppxlib
open OUnit2

module Fixtures = struct
  let location =
    Location.{ loc_start = {pos_fname = "test.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5}
             ; loc_end = {pos_fname = "test.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 10}
             ; loc_ghost = false
             }
end

module Factories = struct
  let ast_name_node ?(loc=Fixtures.location) name =
    {txt = name; loc }

  let ast_type_node type_name =
    { ptyp_desc = Ptyp_constr ((ast_name_node (Lident type_name)),  [])
    ; ptyp_loc = Fixtures.location
    ; ptyp_attributes = []
    }

  let ast_constructor_node ~name ~pcd_args ~pcd_res =
    { pcd_name = ast_name_node name
    ; pcd_args
    ; pcd_res
    ; pcd_loc = Fixtures.location
    ; pcd_attributes = []
    }
end


let test_constructor_is_bare =
  let open Parsetree in
  let test ~constructor ~expected ctxt =
    let actual = Ppx_enum_lib.Utils.constructor_is_bare constructor in
    assert_equal ~ctxt ~cmp:[%eq: bool] ~printer:[%show: bool] expected actual
  in
  (* This list of possible formulations is based on the comments in parsetree.mli.
   * However, I was unable to get the last entry in that list (`| C of {...} as t`)
   * to compile.*)
  "constructor_is_bare" >:::
  [ "| C" >::
      test
        ~constructor: (
          Factories.ast_constructor_node
            ~name: "C"
            ~pcd_args: (Pcstr_tuple [])
            ~pcd_res: None
          )
        ~expected:true
  ; "| C of int * string" >::
      test
        ~constructor: (
          Factories.ast_constructor_node
            ~name: "C"
            ~pcd_args: (Pcstr_tuple [ Factories.ast_type_node "int"
                                    ; Factories.ast_type_node "string"
            ])
            ~pcd_res: None
        )
        ~expected:false
  ; "| C: int" >::
      test
        ~constructor: (
          Factories.ast_constructor_node
            ~name: "C"
            ~pcd_args: (Pcstr_tuple [])
            ~pcd_res: (Some (Factories.ast_type_node "int"))
          )
        ~expected:false
  ; "| C: int * string -> int" >::
      test
        ~constructor: (
          Factories.ast_constructor_node
            ~name: "C"
            ~pcd_args: (Pcstr_tuple [ Factories.ast_type_node "int"
                                    ; Factories.ast_type_node "string"
                                    ])
            ~pcd_res: (Some (Factories.ast_type_node "int"))
          )
        ~expected:false
  ; "| C of {test: int}" >::
      test
        ~constructor: (
          Factories.ast_constructor_node
            ~name: "C"
            ~pcd_args: (Pcstr_record [{ pld_name = Factories.ast_name_node "test"
                                      ; pld_mutable = Immutable
                                      ; pld_type = Factories.ast_type_node "int"
                                      ; pld_loc = Fixtures.location
                                      ; pld_attributes = []
                                      }])
            ~pcd_res: None
          )
        ~expected:false
  ; "| C of {test: int} -> int" >::
      test
        ~constructor: (
          Factories.ast_constructor_node
            ~name: "C"
            ~pcd_args: (Pcstr_record [{ pld_name = Factories.ast_name_node "test"
                                      ; pld_mutable = Immutable
                                      ; pld_type = Factories.ast_type_node "int"
                                      ; pld_loc = Fixtures.location
                                      ; pld_attributes = []
                                      }])
            ~pcd_res: (Some (Factories.ast_type_node "int"))
          )
        ~expected:false
  ]

let suite =
  "Factory" >:::
  [ test_constructor_is_bare
  ]
