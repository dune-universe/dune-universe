# dirsift

Search for directories by type

## Installation

Statically linked binaries are available via [GitHub releases](https://github.com/darrenldl/dirsift/releases)

Alternatively you can install dirsift via opam

## Usage

```
dirsift -t TYPE [PATH]
```

Subdirectories of PATH which satisfy specified TYPE requirements are listed

PATH defaults to `.` if unspecified

TYPE can be one of
- `git`
  - Directory contains `.git` subdirectory
- `not-git`
- `hidden`
  - Name of directory begins with `.`
- `not-hidden`
- `hot`
  - Directory contains >=1 file last modified within past 7 days (7 x 24 hours)
  - User configurable
- `warm`
  - Directory contains >=1 file last modified within past 30 days (30 x 24 hours), but not `hot`
  - User configurable
- `cold`
  - Directory is neither `hot` nor `warm`

If multiple `-t TYPE` are specified, they are connected by `and` (conjunction),
i.e. directory must satisfy all TYPE requirements to be listed
