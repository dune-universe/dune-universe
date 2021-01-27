# Json_encoding Ppx deriver

ppx_deriving_encoding is a json-encoding ppx deriver.

    type x = {
        a : string;
        b : int list;
        c : Ezjsonm.value option;
    }
    [@@deriving json_encoding]

will produce the encoding:

    let x_enc : x encoding =
      conv
        (fun {a; b; c} -> (a, b, c))
        (fun (a, b, c) -> {a; b; c})
        (obj3
          (req "a" string)
          (req "b" (list int))
          (opt "c" any_ezjson_value))

Most of regular json types are handled.
GADT and variant inheritance are not handled yet.

## Field Options

- `[@req]`
    Nullable field instead of an undefined (`opt`)

        a : x option [@req]

    will produce a field :

        (req "a" (option x_enc))

- `[@dft expr]`
    Default field instead

        a : x [@dft expr]

    will produce a field :

        (dft "a" x_enc expr)

- `[@key "name"]`
    Specify the name of the field

         a : x [@key "name"]

     will produce a field :

        (req "name" x_enc)

- `[@title expr]`
    Specify the title of the field

         a : x [@title expr]

     will produce a field :

        (req ~title:expr "a" x_enc)

- `[@description expr]`
    Specify the description of the field

         a : x [@description expr]

     will produce a field :

        (req ~description:expr "a" x_enc)

- `[@exclude expr]`
    Exclude a field from an encoding

        {
          a : x;
          b : y; [@exclude y_default]
          c : z;
        }

     will produce :

        conv
            (fun {a; _; c} -> (a, c))
            (fun (a, c) -> {a; b = y_default; c})
            (obj2
                (req "a" x_enc)
                (req "c" z_enc))

- `[@merge]`
    Merge an field object instead of creating another field

        {
          a : x; [@merge]
          b : z;
        }

    will produce :

        conv
            (fun {a; _; c} -> (a, c))
            (fun (a, c) -> {a; b = y_default; c})
            (merge_objs x_enc (obj1 (req "c" z_enc)))

## General options

- `[@assoc]`

    Create an assoc encoding

        (string * x) list [@assoc]

    will produce :

        assoc x_enc

- `[@enum]`

    Create an string enum encoding

        [ `A | `B | `C ]

    will produce :

        string_enum [ "a", `A ; "b" `B; "c", `C ]

    For normal type constructor, you need to use the flag attribute:

        type t = A | B | C [@@deriving json_encoding {enum}]

- `[@encoding expr]`

    Assign a generic encoding

- `[@obj1 "name"]`

    Wrap an encoding inside a obj1 encoding

## Tuple options

- `[@object]`

    Create an object encoding from a tuple

        ( w, x [@exclude x_default], y [@key "name"], z ) [@object]

    will produce:

        conv
          (fun (w, _, y, z) -> (w, y, z))
          (fun (w, y, z) -> (w, x_default, y, z))
          (obj3
            (req "0" w_enc)
            (req "name" y_enc)
            (req "3" z_enc))

## Variant options

If it is not a string enumeration, any constructor or polymorphic variant will produce a union encoding.
Any case of the union can receive `[@title expr]`, `[@description expr]`, `[@kind "kind"]` attributes.

`[@kind "kind_name"]` will add the encoding

    (obj1 (req "kind" (constant "kind_name")))

to allow several constructor with the same type to be well desctructed.

    type t =
        | A of x [@kind "a"]
        | B of y
        | C of x [@kind "c"] [@@deriving json_encoding]

will produce :

    let enc =
      union [
        case
          (conv (fun x -> (), x) (fun ((), x) -> x)
            (merge_objs (obj1 (req "kind" (constant "a"))) x_enc))
          (function A x -> Some x | _ -> None)
          (fun x -> A x);
        case
          y_enc
          (function B x -> Some x | _ -> None)
          (fun x -> B x);
        case
          (conv (fun x -> (), x) (fun ((), x) -> x)
            (merge_objs (obj1 (req "kind" (constant "c"))) x_enc))
          (function C x -> Some x | _ -> None)
          (fun x -> C x);
      ]

## Top type options

- `ignore`

    wrap an object encoding to ignore other fields

        type t = {
            a : x;
            b : y;
        } [@@deriving json_encoding {ignore}]

    will produce :

        let enc =
          conv
            (fun x -> (), x)
            (fun ((), x) -> x)
            (merge_objs
              unit
              (conv
                (fun {a; b} -> (a, b))
                (fun (a, b) -> {a; b})
                (obj2
                  (req "a" x_enc)
                  (req "b" y_enc))))

    It can also be used as an attribute for records in constructor:

        type x =
            | A of { a : y } [@ignore]
            | B of t

- `remove_prefix`

    Remove prefixes of record

        type x = {
            x_a : a;
            b : b;
        } [@@deriving json_encoding {remove_prefix="true"}]

    will produce :

        let x_enc =
          conv
            (fun {x_a; b} -> (x_a, b))
            (fun (x_a, b) -> {x_a; b})
            (obj2
              (req "a" a_enc)
              (req "b" b_enc))

    When all the record fields have the same prefix they will be automatically removed. This automatic removing can be cancelled by `[@@deriving json_encoding {remove_prefix="false"}]`

- `recursive`

    Wrap an encoding in a recursive construction

        type x =
          | A of x
          | B [@@deriving json_encoding {recursive}]

    will produce :

        let x_enc x_enc =
          union [
            case x_enc (function A x -> Some x | _ -> None) (fun x -> A x);
            case empty (function B -> Some () | _ -> None) (fun () -> B)
          ]

- `title`, `description`

    Wrap en encoding to add some description

        type x = y [@@deriving json_encoding {title = "title"; description = "descr"}]

    will produce :

        let x_enc =
          def "x" ~title:"title" ~description:"descr" y_enc

- `schema`

    Wrap an encoding to add a schema

        type x = y [@@deriving json_encoding {schema = sch}]

    will produce :

        let x_enc =
          conv (fun x -> x) (fun x -> x)
            ~schema:sch y_enc

- `option`

    Choose the default option handling

        type x = {
          a : string option
        }[@@deriving json_encoding {option = "req"}]

        type y = {
          a : string option
        }[@@deriving json_encoding {option = "opt"}]

        type z = {
          a : string option
        }[@@deriving json_encoding {option = "dft"}]

    will produce :

        let x_enc = obj1 (req "a" (option string))
        let y_enc = obj1 (opt "a" string)
        let z_enc = obj1 (dft "a" (option string) None)

    The default one is dft which handles both `null` and `undefined` but will give an error at runtime if the following encoding is nullable.

- `debug`

    Force the printing of the produced encoding during compilation
