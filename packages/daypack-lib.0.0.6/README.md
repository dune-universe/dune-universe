# Daypack-lib

Daypack-lib is a schedule, time and time slots handling library

## Note: Daypack is still WIP

The core scheduling and progress tracking functionalities are largely finished,
but facilities for usage of library in frontend, and the frontend itself
are still underway

## Demos

#### Daypc (cli frontend)

__TODO__

#### Daypack\_lib (core library)

See here __TODO__

## Features

#### Overview

<details><summary>Details</summary>
<p>

- Automatic scheduling

  - See [below](#constraints-or-scheduling-strategies-supported) for strategies supported in scheduling requests

- Manual scheduling

- Recurrence

  - All automatic scheduling strategies are available for recurring tasks as well

- Time pattern (more for __devs__)

  - Functionally very similar to cron time expression, but strictly less general than cron expression

  - This is mainly used as a query for the time slot searching functions in `Time_pattern`

- Time expression

  - A natural to use language with formal grammar for specifying time point and time slots

  - Can be seen as a more expressive layer over `Time_pattern`

- Duration expression

  - A natural to use language with formal grammar for specifying duration of time

- Time profiles

  - Specification of scheduling requests is supplemented by time profiles, which are aliases
    for time periods (a pair of time patterns indicating start and end time of time slots)

  - Some downloadable prebuilt profiles are

    - `work_hours`: Monday to Friday 9am to 5pm

    - `sleep_hours`: Everyday 11pm to 12am, 12am to 6am

  - Time profiles are JSON files designed to be easily created/customised/extended by users, and Daypack processes all profiles
    provided in the profile directory (see user manual)

  - Time profile builder sites are being planned right now (similar to keyboard or mouse macro/profile building sites)

  - See [user manual](#TODO) for details

- Backup plan

  - You can specify multiple scheduling strategies for a given scheduling request,
    and Daypack will try them sequentially until one works

- Progress tracking

  - You can mark task items as "completed" (or "uncompleted")

  - You can record time periods spent for task items

- Schedule versioning and rollback

  - "Snapshots" are made before certain major operations such as scheduling,
    user can also initiate a snapshot manually

  - This allows rollbacks/undos should the user find the schedule resulted from an operation
    unsatisfactory

- (WIP) Multiple users (supported by library, but frontend adoption is WIP)

- (WIP) Taking transit time into account during scheduling

  - This feature is unlikely to land any time soon

- Daypack\_lib is offline (more for __devs__)

  - Daypack\_lib contains implementation of all functionalities, and has zero dependency on any online service

  - This is not novel/unexpected or necessarily desirable, and is listed more for clarity's sake, as some similar software make use of online services

</p>
</details>

#### Constraints (or scheduling strategies) supported

<details><summary>Details</summary>
<p>

__Note:__ The following lists all the constraints supported by the core library,
but frontends may not expose them completely

- `Fixed`

  - Manual scheduling, specifies a task segment starts at a fixed time point

  - E.g. "Meeting starts at 2pm and last for 1.5 hours"

- `Shift`

  - Daypack shifts the task segment(s) around and tries to find a spot
  
  - E.g. "Homework takes 2 hours, schedule it for me between 9am-5pm of next 3 days"

- `Split_and_shift`

  - Daypack splits task segment into smaller segments then shifts them around and tries to find a spot, takes following parameters

    - minimum size

    - maximum size (optional)

    - increment

    - split count (either maximum or exact)

  - E.g. "This work takes 5 hours, I need it done by the end of this week,
    split and shift for me across 5pm-10pm of said days, but all split segments must be at least 1 hour long"

- `Split_even`

  - Daypack splits a task segment into evenly sized smaller segments across some specified
    buckets/boundaries with shifting

    - If some buckets are not usable, then Daypack tries to split across remaining
      buckets with larger even splits

  - E.g. "I want to exercise 5 hours, split it evenly across next 7 days, boundaries
    being 1pm-5pm of each day"

    - If one day ends up being too full to be used, then Daypack splits across 6 days instead,
      and so on

- `Time_share`

  - Interleave multiple task segments with some specified interval size

  - E.g. "Interleave task A, B, C across 1pm-4:30pm with interval size of 30 mins" produces
    the following agenda

    - | Time slots    | Task   |
      | ---           | ---    |
      | 1:00pm-1:30pm | Task A |
      | 1:30pm-2:00pm | Task B |
      | 2:00pm-2:30pm | Task C |
      | 2:30pm-3:00pm | Task A |
      | 3:00pm-3:30pm | Task B |
      | 3:30pm-4:00pm | Task C |
      | 4:00pm-4:30pm | Task A |

- `Push_toward`

  - Similar to shifting, but tries positions closest to a specified time first

  - E.g. "I need this done, which takes 15mins, it needs to be done between 4pm-10pm,
    but I want it as close to 6pm as possible"

</p>
</details>

## Architecture and limitations

<details><summary>Details</summary>
<p>

Daypack does not aim to be a general solver, and only supports a limited set of constraints
(which are listed above)

Furthermore, Daypack only uses a backtracking search procedure with pruning (implemented using lazy sequences)
for solving the constraints,
and does not use any advanced or potentially more efficient constraint solving techniques

It is subsequently inferior to a lot of other automatic task scheduling software,
and cannot accomodate very complex scheduling scenarios

Nevertheless, we hope that the supported constraints are powerful enough for a
simple and standalone personal task scheduler

More detailed docs on the way

Some of the features that Daypack does __NOT__ support

- Resource allocation

  - Doesn't seem to be a useful item for personal TODO list

</p>
</details>

## Getting started

#### Installation

__TODO__

#### User guide

See here __TODO__ for `daypc` user guide

See here __TODO__ for `daypack_lib` library documentation

## Contributions

#### Ideas

Got a feature request? Feel free to open an issue to start a discussion.

Please note that since Daypack was never designed to be a general solver, there
are things prohibitively expensive to properly implement as a result (short of
adding a general solver into Daypack),
which we may cite as a reason should we reject your feature request

We ask for your understanding should that be the case

#### Code

Code contributions are welcome. Please note that by submitting your original work, you agree to
license your work under the MIT license.

## Acknowledgements

- Cli frontend is heavily inspired by [Taskwarrior](https://taskwarrior.org/), which one of the authors heavily used

- We became aware of [Eva](https://github.com/Procrat/eva) later on as well, and took inspiration from its UI/UX design choices and feature set

  - The underlying architecture was independently designed and developed however

- Many thanks to [Gabriel 'Drup' Radanne](https://github.com/Drup) for his review and suggestions on the design and implementation of time expression,
  and also other parts of the library

## LICENSE

MIT
