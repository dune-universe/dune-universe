#include <stddef.h>
#include <maxminddb.h>

size_t mmdb_ml_sizeof_mmdb_s(void) {
    return sizeof(MMDB_s);
}

size_t mmdb_ml_alignof_mmdb_s(void) {
    return offsetof(struct { char c; struct MMDB_s x; }, x);
}

size_t mmdb_ml_sizeof_mmdb_entry_data_s(void) {
    return sizeof(MMDB_entry_data_s);
}

size_t mmdb_ml_alignof_mmdb_entry_data_s(void) {
    return offsetof(struct { char c; struct MMDB_entry_data_s x; }, x);
}

uint32_t mmdb_ml_get_entry_data_has_data(MMDB_entry_data_s* data) {
    return data->has_data;
}

uint32_t mmdb_ml_get_entry_data_type(MMDB_entry_data_s* data) {
    return data->type;
}

uint32_t mmdb_ml_get_entry_data_size(MMDB_entry_data_s* data) {
    return data->data_size;
}

const char* mmdb_ml_get_entry_data_utf8_string_value(MMDB_entry_data_s* data) {
    return data->utf8_string;
}

double mmdb_ml_get_entry_data_double_value(MMDB_entry_data_s* data) {
    return data->double_value;
}

const uint8_t* mmdb_ml_get_entry_data_bytes_value(MMDB_entry_data_s* data) {
    return data->bytes;
}

uint16_t mmdb_ml_get_entry_data_uint16_value(MMDB_entry_data_s* data) {
    return data->uint16;
}

uint32_t mmdb_ml_get_entry_data_uint32_value(MMDB_entry_data_s* data) {
    return data->uint32;
}

int32_t mmdb_ml_get_entry_data_int32_value(MMDB_entry_data_s* data) {
    return data->int32;
}

uint64_t mmdb_ml_get_entry_data_uint64_value(MMDB_entry_data_s* data) {
    return data->uint64;
}

bool mmdb_ml_get_entry_data_boolean_value(MMDB_entry_data_s* data) {
    return data->boolean;
}

float mmdb_ml_get_entry_data_float_value(MMDB_entry_data_s* data) {
    return data->float_value;
}
