/*
 * Copyright (c) 2017 Tony Wuersch. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * sslconf 0.8.3 - commit 0806f5ae3774c30cc8ee18aaabe9d106d7816457
 */

/*
 * read an NCONF config file and dump out its contents
 */

#include <stdio.h>
#include <string.h>
#include <openssl/conf.h>

int main(int argc, char *argv[]) {
  long eline;
  char *cmd, *path;
  char *key;
  CONF *cnf;

  cnf = NCONF_new(NULL);
  if (cnf == NULL) {
    perror("NCONF_new");
    exit(1);
  }

  cmd = argv[0];

  argc--, argv++;

  if (argc != 1) {
    fprintf(stderr, "usage: %s path_to_config_file\n", cmd);
    exit(1);
  }

  path = *argv;

  /* add TESTENV for env tests */
  (void)putenv("TESTENV=testvalue");

  /* int res = NCONF_load(cnf, path, &eline); */
  /* for debugging, change "NCONF_load" to "def_load" */
  int res = NCONF_load(cnf, path, &eline);
  if (res <= 0) {
    fprintf(stderr, "error on line %ld\n", eline);
    exit(1);
  }

  NCONF_dump_fp(cnf, stdout);
}

/*
 * Copyright (c) 2017 Tony Wuersch
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
