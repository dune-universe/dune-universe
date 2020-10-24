# telltime
Cli tool for interacting with Daypack-lib components

## Warning
Both telltime and Daypack-lib are WIP. Do not use either of them for anything serious.

## Examples

#### Search for time slots matching Daypack time expression

"Hm, I wonder what years have Febuary 29th?"

```
$ telltime search --time-slots 5 --years 100 "feb 29 00:00"
Searching in time zone offset (seconds)            : 36000
Search by default starts from (in above time zone) : 2020 Sep 03 19:24:15

Matching time slots (in above time zone):
[2024 Feb 29 00:00:00, 2024 Feb 29 00:00:01)
[2028 Feb 29 00:00:00, 2028 Feb 29 00:00:01)
[2032 Feb 29 00:00:00, 2032 Feb 29 00:00:01)
[2036 Feb 29 00:00:00, 2036 Feb 29 00:00:01)
[2040 Feb 29 00:00:00, 2040 Feb 29 00:00:01)
```

"When exactly is Thursday 9am to Friday 10am?"

```
$ telltime search --time-slots 1 "thu 9am to friday 10am"
Searching in time zone offset (seconds)            : 36000
Search by default starts from (in above time zone) : 2020 Sep 06 17:33:22

Matching time slots (in above time zone):
[2020 Sep 10 09:00:00, 2020 Sep 11 10:00:00)
```

"Would be handy to know what this cron expression refers to"

```
$ telltime search --time-slots 5 "0 4 8-14 * *"
Searching in time zone offset (seconds)            : 36000
Search by default starts from (in above time zone) : 2020 Sep 06 17:39:56

Matching time slots (in above time zone):
[2020 Sep 08 04:00:00, 2020 Sep 08 04:01:00)
[2020 Sep 09 04:00:00, 2020 Sep 09 04:01:00)
[2020 Sep 10 04:00:00, 2020 Sep 10 04:01:00)
[2020 Sep 11 04:00:00, 2020 Sep 11 04:01:00)
[2020 Sep 12 04:00:00, 2020 Sep 12 04:01:00)
```

"The (insert generic tool name) supports querying based on time, but only in standard date formats/unix time stamps. If only there is a way of retrieving these easily."

```
$ telltime search --time-slots 100 "2019 dec 1 00:00 to 2019 dec 15 15:00 \
  || 2020 . jan . 17 to 20, 23 . 13:00 to 22:00"
Searching in time zone offset (seconds)            : 36000
Search by default starts from (in above time zone) : 2020 Sep 06 17:53:22

Matching time slots (in above time zone):
[2019 Dec 01 00:00:00, 2019 Dec 15 15:00:00)
[2020 Jan 17 13:00:00, 2020 Jan 17 22:00:00)
[2020 Jan 18 13:00:00, 2020 Jan 18 22:00:00)
[2020 Jan 19 13:00:00, 2020 Jan 19 22:00:00)
[2020 Jan 20 13:00:00, 2020 Jan 20 22:00:00)
[2020 Jan 23 13:00:00, 2020 Jan 23 22:00:00)
$ telltime search --time-slots 100 --format unix "2019 dec 1 00:00 to 2019 dec 15 15:00 \
  || 2020 . jan . 17 to 20, 23 . 13:00 to 22:00"
Searching in time zone offset (seconds)            : 36000
Search by default starts from (in above time zone) : 2020 Sep 06 17:49:01

Matching time slots:
[1575122400, 1576386000)
[1579230000, 1579262400)
[1579316400, 1579348800)
[1579402800, 1579435200)
[1579489200, 1579521600)
[1579748400, 1579780800)
```

"I have a bunch of time ranges, but some of them overlap, and they are not in the right order. If only there is a way to combine and sort them easily."

```
$ telltime search --time-slots 1000 "2020 . jan . 1, 10, 20 . 13:00 to 14:00 \
  || 2019 dec 25 13:00 \
  || 2019 dec 25 10am to 17:00 \
  || 2020 jan 5 10am to 1:30pm \
  || 2020 . jan . 7 to 12 . 9:15am to 2:45pm"
Searching in time zone offset (seconds)            : 36000
Search by default starts from (in above time zone) : 2020 Sep 06 18:01:12

Matching time slots (in above time zone):
[2019 Dec 25 10:00:00, 2019 Dec 25 17:00:00)
[2020 Jan 01 13:00:00, 2020 Jan 01 14:00:00)
[2020 Jan 05 10:00:00, 2020 Jan 05 13:30:00)
[2020 Jan 07 09:15:00, 2020 Jan 07 14:45:00)
[2020 Jan 08 09:15:00, 2020 Jan 08 14:45:00)
[2020 Jan 09 09:15:00, 2020 Jan 09 14:45:00)
[2020 Jan 10 09:15:00, 2020 Jan 10 14:45:00)
[2020 Jan 11 09:15:00, 2020 Jan 11 14:45:00)
[2020 Jan 12 09:15:00, 2020 Jan 12 14:45:00)
[2020 Jan 20 13:00:00, 2020 Jan 20 14:00:00)
```

Search for all Australia ACT 2020 public holidays that fall on weekends

```
$ telltime search "( \
  (2020 . jan . 1, 27 . 00:00 to 23:59) \
  || (2020 . mar . 9 . 00:00 to 23:59) \
  || (2020 . apr . 10 to 13, 25, 27 . 00:00 to 23:59) \
  || (2020 . jun . 1, 8 . 00:00 to 23:59) \
  || (2020 . oct . 5 . 00:00 to 23:59) \
  || (2020 . dec . 25, 26 . 00:00 to 23:59) \
) \
&& w[sat,sun]hm"
Searching in time zone offset (seconds)            : 36000
Search by default starts from (in above time zone) : 2020 Sep 04 00:07:27

Matching time slots (in above time zone):
[2020 Dec 26 00:00:00, 2020 Dec 26 23:59:00)
```

"I want to generate time filters for Wireshark"

```
$ telltime search \
>   --format "(frame.time >= {smon:Xxx} {smday:0X}, {syear} {shour:0X}:{smin:0X}:{ssec:0X} && frame.time < {emon:Xxx} {emday:0X}, {eyear} {ehour:0X}:{emin:0X}:{esec:0X})" \
>   --sep " || " \
>   "2020 . jun . 1 to 15 . 5pm to 6pm"
Searching in time zone offset (seconds)            : 39600
Search by default starts from (in above time zone) : 2020 Oct 21 13:18:46

(frame.time >= Jun 01, 2020 17:00:00 && frame.time < Jun 01, 2020 18:00:00) || (frame.time >= Jun 02, 2020 17:00:00 && frame.time < Jun 02, 2020 18:00:00) || (frame.time >= Jun 03, 2020 17:00:00 && frame.time < Jun 03, 2020 18:00:00) || (frame.time >= Jun 04, 2020 17:00:00 && frame.time < Jun 04, 2020 18:00:00) || (frame.time >= Jun 05, 2020 17:00:00 && frame.time < Jun 05, 2020 18:00:00) || (frame.time >= Jun 06, 2020 17:00:00 && frame.time < Jun 06, 2020 18:00:00) || (frame.time >= Jun 07, 2020 17:00:00 && frame.time < Jun 07, 2020 18:00:00) || (frame.time >= Jun 08, 2020 17:00:00 && frame.time < Jun 08, 2020 18:00:00) || (frame.time >= Jun 09, 2020 17:00:00 && frame.time < Jun 09, 2020 18:00:00) || (frame.time >= Jun 10, 2020 17:00:00 && frame.time < Jun 10, 2020 18:00:00)
```

#### Get exact time after some duration from now

```
$ telltime from-now "1 hour"
Now                   : 2020-09-03 15:53:29
Duration (original)   : 1 hour
Duration (normalized) : 1 hours 0 mins 0 secs
Now + duration        : 2020-09-03 16:53:29
```

```
$ telltime from-now "1.5 hour"
Now                   : 2020-09-03 15:54:24
Duration (original)   : 1.5 hour
Duration (normalized) : 1 hours 30 mins 0 secs
Now + duration        : 2020-09-03 17:24:24
```

```
$ telltime from-now "1.5 days 2.7 hours 0.5 minutes"
Now                   : 2020-09-03 15:55:43
Duration (original)   : 1.5 days 2.7 hours 0.5 minutes
Duration (normalized) : 1 days 14 hours 42 mins 30 secs
Now + duration        : 2020-09-05 06:38:13
```

#### Get time right now

```
$ telltime now
2020-09-03 15:57:39
```

## Possible uses of telltime

- For scripts to check if time is within time slots specified by time expression
- Looking up the exact date for sentences we often use, e.g. "thu 9am to 12pm"
- Do some operations over time slots, set operators such as `&&` (intersect), `||` (union) are available

## Online demo

- The engine that evaluates the time expression can be accessed via this online [demo](https://daypack-dev.github.io/time-expr-demo/)
- Note that the online demo may be more limiting than `telltime` itself, due to memory space restriction of JS being run in a browser
  - In other words, `telltime` might still return something useful even if the demo failed for a particular input
