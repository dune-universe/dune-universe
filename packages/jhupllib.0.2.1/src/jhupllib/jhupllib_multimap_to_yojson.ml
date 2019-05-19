open Batteries;;
open Jhupllib_multimap;;
open Jhupllib_yojson_utils;;

module Make
    (M : Multimap_sig)
    (K_yojson : To_yojson_type with type t = M.key)
    (V_yojson : To_yojson_type with type t = M.value)
=
struct
  let to_yojson m =
    `Assoc
      [ ( "type"
        , `String "multimap"
        )
      ; ( "mappings"
        , `List
            (M.enum_by_key m
             |> Enum.map
               (fun (k,vs) ->
                  `Assoc
                    [ ( "key", K_yojson.to_yojson k )
                    ; ( "values"
                      , `List ( M.S.enum vs
                                |> Enum.map V_yojson.to_yojson
                                |> List.of_enum
                              )
                      )
                    ]
               )
             |> List.of_enum
            )
        )
      ]
  ;;
end;;
