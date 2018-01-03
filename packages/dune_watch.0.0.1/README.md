# Dune_watch

A tool to relaunch `jbuilder build` when a file modification is detected via `fswatch`.

# Requirements

`dune_watch` requires `jbuilder` and `fswatch` command.

# How to use

At the top of your project,

```shell
$ dune_watch
```

then it executes `jbuilder build` build command.
Once the command finishes, `dune_watch` monitors file modification
under the directory where it is launched and relaunches the build command
when necessary.
