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
  - Directory identified as git repository
- `not-git`
- `borg`
  - Directory identified as BorgBackup backup program repository
- `not-borg`
- `restic`
  - Directory identified as Restic backup program repository
- `not-restic`
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

## Configuration

Create a file at `~/.config/dirsift/config`, add any number of the options
from the example config as follows

```
hot_upper_bound = "7d"
warm_upper_bound = "30d"
```
