/*
 * Copyright (C) 2015 Unikernel Systems
 *
 * Permission to use, copy, modify, and distribute this software for any
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

#include <launch.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include <stdio.h>

/* From the OCaml Unix package */
extern void unix_error(int errcode, char *cmdname, value cmdarg);

#define Nothing ((value)0)

struct state {
  char *name;
  int *listening_fds;
  size_t n_listening_fds;
  int pipe_writer;
};

static void really_write(int fd, uint8_t *buffer, int total){
  int n,remaining = total;
  while (remaining > 0){
    n = (int) write(fd, buffer, (size_t) remaining);
    if (n == 0){
      fprintf(stderr, "Internal error writing to launch_activate_socket_pipe\n");
      exit(1);
    }
    remaining -= (int) n;
    buffer = buffer + n;
  }
}

static void write_int32(int fd, uint32_t x){
  uint32_t tmp = htonl(x);
  really_write(fd, (uint8_t*) &tmp, sizeof(tmp));
}

static void *activate_thread(void *data){
  struct state *state = (struct state*) data;
  int err = launch_activate_socket(state->name, &(state->listening_fds), &(state->n_listening_fds));
  uint8_t x = 0;
  switch (err) {
    case ENOENT: x = 1; break;
    case ESRCH:  x = 2; break;
    case EALREADY: x = 3; break;
  }
  write_int32(state->pipe_writer, x);
  if (x == 0) {
    write_int32(state->pipe_writer, state->n_listening_fds);
    for (int i = 0; i < state->n_listening_fds; i++) {
      write_int32(state->pipe_writer, *(state->listening_fds + i));
    }
  }
  close(state->pipe_writer);
  free(state->name);
  /* These are only allocated in the successful path */
  if (state->listening_fds) free(state->listening_fds);
  return NULL;
}

value stub_launch_activate_socket(value name) {
  CAMLparam1(name);
  CAMLlocal1(result);
  pthread_t background_thread;

  int filedes[2];
  int err = pipe(filedes);
  if (err) unix_error(errno, "pipe", Nothing);

  struct state *state = (struct state*)malloc(sizeof(struct state));
  if (!state) unix_error(errno, "malloc", Nothing);
  bzero(state, sizeof(struct state));

  state->name = strdup(String_val(name));
  state->pipe_writer = filedes[1];
  err = pthread_create(&background_thread, NULL, activate_thread, (void*)state);
  if (err) unix_error(err, "pthread_create", Nothing);
  CAMLreturn(Val_int(filedes[0]));
}
