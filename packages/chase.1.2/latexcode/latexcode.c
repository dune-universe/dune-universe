/*
  Simple literate programming by commenting out all but code
  environments in a LaTeX source file.

  Author: John D Ramsdell

  Copyright (C) 2019 The MITRE Corporation

  This program is free software: you can redistribute it and/or
  modify it under the terms of the BSD License as published by the
  University of California.
*/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <regex.h>
/* Default comment string */
#define COMMENT "%"
#define LINESIZ 1024

const char version[] = "1.0";

static int
putcmt(const char *cmt, const char *line) {
  return fprintf(stdout, "%s %s", cmt, line);
}

static int
go(const char *cmt) {
  char line[LINESIZ];
  regex_t begin, end;
  int i;
  int commenting = 1;		/* Should comment string be added? */

  /* Compile regular expressions */
  i = regcomp(&begin, "\\begin\{code}", REG_EXTENDED | REG_NOSUB);
  if (i) {
    regerror(i, &begin, line, LINESIZ);
    fprintf(stderr, "%s\n", line);
    return 1;
  }

  i = regcomp(&end, "\\end\{code}", REG_EXTENDED | REG_NOSUB);
  if (i) {
    regerror(i, &end, line, LINESIZ);
    fprintf(stderr, "%s\n", line);
    return 1;
  }

  for (;;) {				/* Main loop */

    if (!fgets(line, LINESIZ, stdin)) { /* Read a line */
      if (ferror(stdin)) {
	fprintf(stderr, "Read error\n");
	return 1;
      }
      return 0;			/* EOF found -- we're done! */
    }

    i = regexec(&begin, line, 0, NULL, 0);
    switch (i) {
    case 0:			/* Begin code found */
      putcmt(cmt, line);
      commenting = 0;
      continue;
    case REG_NOMATCH:
      break;			/* No match */
    default:
      regerror(i, &begin, line, LINESIZ);
      fprintf(stderr, "%s\n", line);
      return 1;
    }

    i = regexec(&end, line, 0, NULL, 0);
    switch (i) {
    case 0:			/* End code found */
      putcmt(cmt, line);
      commenting = 1;
      continue;
    case REG_NOMATCH:		/* No match */
      if (commenting)
	putcmt(cmt, line);
      else
	fputs(line, stdout);
      break;
    default:
      regerror(i, &end, line, LINESIZ);
      fprintf(stderr, "%s\n", line);
      return 1;
    }
  }
}

/* Generic filtering main and usage routines altered to handle the
   comment string command line option. */

static void
print_version(const char *program)
{
  fprintf(stderr, "%s version %s\n", program, version);
}

static void
usage(const char *prog)
{
  fprintf(stderr,
	  "Usage: %s [options] [input]\n"
	  "Options:\n"
	  "  -o file -- output to file (default is standard output)\n"
	  "  -c cmt  -- set comment string (default is \"%s\")\n"
	  "  -v      -- print version information\n"
	  "  -h      -- print this message\n",
	  prog, COMMENT);
  print_version(prog);
}

int
main(int argc, char **argv)
{
  extern char *optarg;
  extern int optind;

  char *input = NULL;
  char *output = NULL;
  char *comment = COMMENT;

  for (;;) {
    int c = getopt(argc, argv, "o:c:vh");
    if (c == -1)
      break;
    switch (c) {
    case 'o':
      output = optarg;
      break;
    case 'c':
      comment = optarg;
      break;
    case 'v':
      print_version(argv[0]);
      return 0;
    case 'h':
      usage(argv[0]);
      return 0;
    default:
      usage(argv[0]);
      return 1;
    }
  }

  switch (argc - optind) {
  case 0:			/* Use stdin */
    break;
  case 1:
    input = argv[optind];
    if (!freopen(input, "r", stdin)) {
      perror(input);
      return 1;
    }
    break;
  default:
    fprintf(stderr, "Bad arg count\n");
    usage(argv[0]);
    return 1;
  }

  if (output && !freopen(output, "w", stdout)) {
    perror(output);
    return 1;
  }

  return go(comment);
}
