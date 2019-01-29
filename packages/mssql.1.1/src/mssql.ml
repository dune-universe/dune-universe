module Error = Mssql_error
module Param = Db_field
module Row = Row
module Test = Test
include Client

exception Error = Error.Mssql_error
