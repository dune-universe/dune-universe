#include <stdalign.h>
#include <maxminddb.h>

size_t mmdb_ml_sizeof_mmdb_s(void);
size_t mmdb_ml_alignof_mmdb_s(void);
size_t mmdb_ml_sizeof_mmdb_entry_data_s(void);
size_t mmdb_ml_alignof_mmdb_entry_data_s(void);

size_t   mmdb_ml_language_count(MMDB_s* mmdb);
char**   mmdb_ml_language_names(MMDB_s* mmdb);
uint16_t mmdb_ml_binary_format_major_version(MMDB_s* mmdb);
uint16_t mmdb_ml_binary_format_minor_version(MMDB_s* mmdb);

bool           mmdb_ml_get_entry_data_has_data(MMDB_entry_data_s* data);
uint32_t       mmdb_ml_get_entry_data_type(MMDB_entry_data_s* data);
uint32_t       mmdb_ml_get_entry_data_size(MMDB_entry_data_s* data);

const char*    mmdb_ml_get_entry_data_utf8_string_value(MMDB_entry_data_s* data);
double         mmdb_ml_get_entry_data_double_value(MMDB_entry_data_s* data);
const uint8_t* mmdb_ml_get_entry_data_bytes_value(MMDB_entry_data_s* data);
uint16_t       mmdb_ml_get_entry_data_uint16_value(MMDB_entry_data_s* data);
uint32_t       mmdb_ml_get_entry_data_uint32_value(MMDB_entry_data_s* data);
int32_t        mmdb_ml_get_entry_data_int32_value(MMDB_entry_data_s* data);
uint64_t       mmdb_ml_get_entry_data_uint64_value(MMDB_entry_data_s* data);
bool           mmdb_ml_get_entry_data_boolean_value(MMDB_entry_data_s* data);
float          mmdb_ml_get_entry_data_float_value(MMDB_entry_data_s* data);
