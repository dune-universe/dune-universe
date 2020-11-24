open OUnit
open Mysql_protocol

let fields = Test_query_select.fields
let mysql_fields = Test_query_select.mysql_fields

let field_name_questionmark = ("?", 0)
let mysql_field_questionmark vendor = 
  match vendor with
  | Test_types.MySQL ->
    {
      Mp_field_packet.field_catalog = "def";
      Mp_field_packet.field_db = "";
      Mp_field_packet.field_table = "";
      Mp_field_packet.field_org_table = "";
      Mp_field_packet.field_name = "?";
      Mp_field_packet.field_org_name = "";
      (* always Binary charset and collation, no matter the character encoding is *)
      Mp_field_packet.field_charset_number = Mp_charset.charset_number (Mp_charset.Binary_charset, Mp_charset.Binary_collation);
      Mp_field_packet.field_length = Int64.zero;
      Mp_field_packet.field_type = Mp_field_packet.Field_type_var_string;
      Mp_field_packet.field_flags = [Mp_field_packet.Field_flag_binary;];
      Mp_field_packet.field_decimals = 0;
      Mp_field_packet.field_default = Int64.zero;
      Mp_field_packet.version = Mp_protocol.Protocol_version_41;
    }
  | Test_types.MariaDB ->
    {
      Mp_field_packet.field_catalog = "def";
      Mp_field_packet.field_db = "";
      Mp_field_packet.field_table = "";
      Mp_field_packet.field_org_table = "";
      Mp_field_packet.field_name = "?";
      Mp_field_packet.field_org_name = "";
      (* always Binary charset and collation, no matter the character encoding is *)
      Mp_field_packet.field_charset_number = Mp_charset.charset_number (Mp_charset.Binary_charset, Mp_charset.Binary_collation);
      Mp_field_packet.field_length = Int64.zero;
      Mp_field_packet.field_type = Mp_field_packet.Field_type_null;
      Mp_field_packet.field_flags = [Mp_field_packet.Field_flag_binary;];
      Mp_field_packet.field_decimals = 0;
      Mp_field_packet.field_default = Int64.zero;
      Mp_field_packet.version = Mp_protocol.Protocol_version_41;
    }

let build_ok_prepare vendor db_name charset version =
  (*
    In MariaDB, the handler is incremented by 1 each time.
    If needed, restart the MariaDB server to pass the test.
  *)
  { Mp_client.prepare_handler = Int64.of_int 1;
    Mp_client.prepare_nb_columns = 56;
    Mp_client.prepare_nb_parameters = 1;
    Mp_client.prepare_warning_count = 0;
    Mp_client.prepare_parameters_fields = [mysql_field_questionmark vendor];
    Mp_client.prepare_parameters_names = [field_name_questionmark];
    Mp_client.prepare_columns_fields = (mysql_fields vendor db_name charset version);
    Mp_client.prepare_columns_names = fields;
  }

let test1 vendor connection db_name version = 
  let () = 
    let sql = "SELECT * FROM test_ocmp WHERE f_autoinc_not_null_no_def = ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let f = 
      let p = Mp_client.prepare ~connection:connection ~statement:stmt in
      let (_, p) = Mp_client.get_prepared_statement p in
      p
    in
    assert_equal ~msg:sql 
      (build_ok_prepare vendor db_name connection.Mp_client.configuration.Mp_client.charset_number version)
      (Test_query.try_query ~f:f ~sql:sql)
  in
  ()

let test host connection encoding _ = 
  let (vendor, version, _, _, _) = host in
  let module F = (
    val (
      match encoding with
      | Mp_charset.Latin1 -> (
          let module E = struct
            include Fixture_latin1
          end
          in (module E : Fixture.FIXTURE)
        )
      | Mp_charset.Utf8 -> (
          let module E = struct
            include Fixture_utf8
          end
          in (module E : Fixture.FIXTURE)
        )
      | _ -> assert false
    ) : Fixture.FIXTURE
  )
  in
  try
    test1 vendor connection F.db_name version
  with
  | Mp_client.Error err as e -> (
      let () = prerr_endline (Mp_client.error_exception_to_string err) in
      raise e
    )
