# Dune_watch

A tool to relaunch `jbuilder build` when a file modification is detected via `fswatch` or `inotifywait`.

# Requirements

Of course, we need `jbuilder` (or `dune`).  In addition,

* Mac OS X: `fswatch` command is required.
* Linux: `inodewait` command is required.

In Linux, there is a limit of number of file monitorings per user,
and `inotifywait` command fails if there are too many files under
the directory of `dune_watch` is running.  The issue may be solved
by increasing the limit may solve the issue:
https://github.com/guard/listen/wiki/Increasing-the-amount-of-inotify-watchers

# Limitation

Currently it only tested with Mac OS X with `fswatch` and Linux with `inotifywait`.

# How to use

At the top of your project,

```shell
$ dune_watch
```

then it executes `jbuilder build` build command.
Once the command finishes, `dune_watch` monitors file modification
under the directory where it is launched and relaunches the build command
when necessary.
