module String = Stdcompat.String
module Template = Template
module Parser = Parser
module Expansion = Expansion
module Expansion_type = Expansion_type

type variable = Expansion.variable

let parse_and_expand ~f ~template =
  let parsed_template = Parser.template_of_string template in
  f ~template:parsed_template


let template_uri = parse_and_expand ~f:Expansion.expand_template

let template_uri_with_strings = parse_and_expand ~f:Expansion.expand_template_with_strings

let template_uri_with_lists = parse_and_expand ~f:Expansion.expand_template_with_lists

let template_uri_with_assoc_list = parse_and_expand ~f:Expansion.expand_template_with_assoc_list
