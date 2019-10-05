module File = struct
  type t = {
    file_name : string;
    contents : string;
  }
end

type t = File.t list

type generated_names = {
  runtime_module_name : string;
  module_alias : string;
  serialize : string;
  deserialize : string;
  byte_output_var_name : string;
  parsed_message_var_name : string;
}

let field_value_runtime_module_name = "Field_value"

let field_value_module_alias = "Field'"

type output_format =
  | Binary
  | Text

let names_of_output_format = function
  | Binary ->
      {
        runtime_module_name = "Binary_format";
        module_alias = "Bin'";
        serialize = "to_binary";
        deserialize = "of_binary";
        byte_output_var_name = "_o";
        parsed_message_var_name = "_m";
      }
  | Text ->
      {
        runtime_module_name = "Text_format";
        module_alias = "Text'";
        serialize = "to_text";
        deserialize = "of_text";
        byte_output_var_name = "_o";
        parsed_message_var_name = "_m";
      }
