Delimited
=========

This library contains parsers & generators for CSVs (and CSV-like delimited
formats). It additionally contains some non-csv format parsers.

## Reading CSVs
The `Delimited.Read` module contains two mechanisms for reading CSVs:

* The applicative interface. This interface has a small learning curve but
  produces clean, composable and type-safe parsers for the `'a` of your choice.
* Using the pre-built `Row` object & requesting headers/indices directly. This
  is a more obvious interface but provides little protection against programmer
  error.

In new code you should favour the applicative interface (it's like
`Command.Param`). See the example under `example/` or ask a delimited dev for
help if you're not familiar.

## Writing CSVs
The `Delimited.Write` module contains two mechanisms for writing CSVs:

* The main interface. Like the applicative interface for reading CSVs, this
  protects fairly well against programmer error and provides clean, composable
  and type-safe CSV generators for the `'a` of your choice.
* The row-based interface takes string lists and formats them as CSVs. There
  are no checks that you have the correct number or order of columns.

New code should prefer the builder interface. There's an example under
`example/` or you can ask a delimited dev for help.

(Technical note: the main writer interface is _not_ an applicative functor.)

## Non-CSV parsers
There are two non-csv formats supported for parsing:

* `Non_csv.character_separated_without_quoting`: like CSVs but no field may
  contain newlines or the delimiter character as there is no quoting
* `Non_csv.Positional`: parses fixed-width fields
