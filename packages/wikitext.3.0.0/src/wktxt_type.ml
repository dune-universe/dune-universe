type order = Ordered | Unordered

type def_type = Term | Description

type table_cell = TableCell | TableHeader

type document = block list

and block =
  | Header of int * inline list
  | Paragraph of inline list
  | List of block list list
  | NumList of block list list
  | DefList of def_block list
  | Table of inline list * table_block list list
  | Hrule
  | NoWikiBlock of string

and table_block =
  | TableHead of inline list
  | TableItem of inline list

and def_block = inline list * block list

and inline =
  | Bold of inline list
  | Italic of inline list
  | String of string
  | Link of string
  | NoWiki of string
