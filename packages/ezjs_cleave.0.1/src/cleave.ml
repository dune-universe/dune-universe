open Js_of_ocaml

type string_field = Js.js_string Js.t Js.prop
type int_field = int Js.prop
type bool_field = bool Js.prop
type string_array_field = Js.js_string Js.t Js.js_array Js.t Js.prop
type int_array_field = int Js.js_array Js.t Js.prop
type string_meth = Js.js_string Js.t Js.meth
type unit_meth = unit Js.meth

class type options = object
  method creditCard : bool_field
  method creditCardStrictMode : bool_field
  method onCreditCardTypeChanged : (Js.js_string Js.t -> unit) Js.t Js.prop
  method phone : bool_field
  method phoneRegionCode : string_field
  method date : bool_field
  method datePattern : string_array_field
  method time : bool_field
  method timePattern : string_array_field
  method timeFormat : string_field
  method numeral : bool_field
  method numeralThousandsGroupStyle : string_field (*thousand, lakh, wan, none*)
  method numeralIntegerScale : int_field
  method numeralDecimalScale : int_field
  method numeralDecimalMark : string_field
  method numeralPositiveOnly : bool_field
  method stripLeadingZeroes : bool_field
  method blocks : int_array_field
  method delimiter : string_field
  method delimiters : string_array_field
  method delimiterlazyshow : bool_field
  method prefix : string_field
  method noImmediatePrefix : bool_field
  method rawValueTrimPrefix : bool_field
  method numericOnly : bool_field
  method uppercase : bool_field
  method lowercase : bool_field
  method onValueChanged : (Dom_html.event Js.t -> unit) Js.prop
end

class type cleave = object
  method getRawValue : string_meth
  method setRawValue : Js.js_string Js.t -> unit_meth
  method getFormattedValue : string_meth
  method getISOFormatDate : string_meth
  method destroy : unit_meth
  method setPhoneRegionCode : string_meth
end

let cleave : (Js.js_string Js.t -> options Js.t -> cleave Js.t) Js.constr =
  Js.Unsafe.global##._Cleave

let raw_options () : options Js.t = Js.Unsafe.obj [||]

let general_options ?(blocks=[]) ?(delimiter=" ") ?(delimiters=[])
    ?(delimiter_lazy=false) ?(prefix=None) ?(no_immediate_prefix=false)
    ?(raw_value_trim_prefix=false) ?(numeric=false)
    ?(uppercase=false) ?(lowercase=false) ?(onchange=(fun _ -> ())) options =
  options##.blocks := Js.array (Array.of_list blocks);
  options##.delimiter := Js.string delimiter;
  options##.delimieters := Js.array (Array.of_list @@ List.map Js.string delimiters);
  options##.delimiterlazyshow := delimiter_lazy;
  options##.noImmediatePrefix := no_immediate_prefix;
  options##.rawValueTrimPrefix := raw_value_trim_prefix;
  options##.numericOnly := numeric;
  options##.uppercase := uppercase;
  options##.lowercase := lowercase;
  options##.onValueChanged := onchange;
  match prefix with
  | None -> ()
  | Some prefix -> options##.prefix := Js.string prefix

let make_card ?(strict_mode=false) ?onchange ?gen_opt selector =
  let options = raw_options () in
  options##.creditCard := true;
  options##.creditCardStrictMode := strict_mode;
  begin match onchange with
    | None -> ()
    | Some onchange -> options##.onCreditCardTypeChanged := onchange end;
  begin match gen_opt with
    | None -> ()
    | Some gen_opt -> gen_opt options end;
  new%js cleave (Js.string selector) options

let make_phone ?gen_opt region_code selector =
  let options = raw_options () in
  options##.phone := true;
  options##.phoneRegionCode := Js.string region_code;
  begin match gen_opt with
    | None -> ()
    | Some gen_opt -> gen_opt options end;
  new%js cleave (Js.string selector) options

let make_date ?(date_pattern=["d"; "m"; "Y"]) ?gen_opt selector =
  let options = raw_options () in
  options##.date := true;
  options##.datePattern := Js.array (Array.of_list @@ List.map Js.string date_pattern);
  begin match gen_opt with
    | None -> ()
    | Some gen_opt -> gen_opt options end;
  new%js cleave (Js.string selector) options

let make_time ?(time_pattern=["h"; "m"; "s"]) ?(time_format="24") ?gen_opt selector =
  let options = raw_options () in
  options##.time := true;
  options##.timePattern := Js.array (Array.of_list @@ List.map Js.string time_pattern);
  options##.timeFormat := Js.string time_format;
  begin match gen_opt with
    | None -> ()
    | Some gen_opt -> gen_opt options end;
  new%js cleave (Js.string selector) options

let make_numeral ?(group_style="thousand") ?integer_scale ?(decimal_scale=2)
    ?(decimal_mark=".") ?(positive=false) ?(strip_zeroes=true) ?gen_opt selector =
  let options = raw_options () in
  options##.numeral := true;
  options##.numeralThousandsGroupStyle := Js.string group_style;
  options##.numeralDecimalScale := decimal_scale;
  options##.numeralDecimalMark := Js.string decimal_mark;
  options##.numeralPositiveOnly := positive;
  options##.stripLeadingZeroes := strip_zeroes;
  begin match integer_scale with
    | None -> ()
    | Some integer_scale -> options##.numeralIntegerScale := integer_scale end;
  begin match gen_opt with
    | None -> ()
    | Some gen_opt -> gen_opt options end;
  new%js cleave (Js.string selector) options

let value cl = Js.to_string cl##getRawValue
let set_value cl s = cl##(setRawValue (Js.string s))
let fvalue cl = Js.to_string cl##getFormattedValue
let iso_date cl = Js.to_string cl##getISOFormatDate
let destroy cl = cl##destroy
let set_region_code cl code = cl##(setPhoneRegionCode (Js.string code))
