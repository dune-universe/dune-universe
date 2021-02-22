## 1.0.6 2021-02-22

### Added

- Added Variable#__vscodeVariableMenuContext property.

## 1.0.5 2021-01-28

### Added

- Added Debug_rpc.set_progressive_command_handler

### Fixed

- Delay send event to after executing command to make vscode happy

## 1.0.4 (2021-01-02)

### Added

- Added Debug_rpc.log_src to allow to control log level externally

## 1.0.3 (2021-01-02)

### Fixed

- Fix command failure will send response with `success=true`

## 1.0.2 (2020-12-07)

### Fixed

- Fix `breakpoints : Breakpoint.t` should be `breakpoints : Breakpoint.t list`
- Fix `Env.t = Empty_dict.t` shuld be `Env.t = String_opt_dict.t`

## 1.0.1 (2020-12-04)

Initial release.

- Specification version is 1.43
