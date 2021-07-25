## v3.0.1 2021-07-24

- Improve documentation, fix some typos in code. Rename `read_result` to `read` and `read_part` to `read 

## v3.0.0 2021-07-23

This release contains breaking change, therefore the version is incremented to
v3.0.0

- Flatten module `Part_header` to `part_header`
- Implement reader/pull based parser to retrieve multipart parts
- Implement push-based incremental input model
- Remove dependency on `lwt`, `reparse-lwt` and `reparse`

## v2.0.1 2021-06-27

- Relax CRFL token on the first boundary value line

## v2.0.0 2021-06-26

- Upgrade to `reparse v3.0.0`
- Change API to streaming api 

## v1.1.0 2021-04-07

- Ensure compatibility with reparse v2.1.0

## v1.0.1 2020-04-12

- Fixes error prone equal - #10.

## v1.0.0 2020-30-11

- First release.
