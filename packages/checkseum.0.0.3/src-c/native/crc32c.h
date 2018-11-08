#ifndef CHECKSUM_CRC32C_H
#define CHECKSUM_CRC32C_H

#include "size_t.h"
#include <stdint.h>

uint32_t crc32c(uint32_t crc, const void *buf, size_t len);

#endif
