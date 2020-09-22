# Time expression specification

## Syntax

```
<hms_mode> ::=
  | "am"
  | "pm"
  | ... (all other variants with different casing of above)

<hms> ::=
  | <hour>                       [<hms_mode>]
  | <hour> : <minute>            [<hms_mode>]
  | <hour> : <minute> : <second> [<hms_mode>]

<weekday> ::=
  | "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday"
  | ... (all prefixes that allow unique match of any of above)
  | ... (all other variants with different casing of above)

<month_day> ::=
  | "1" | ... | "31"

<day> ::=
  | <weekday>
  | <month_day>

<human_int_month> ::=
  | "1" | ... | "12"

<direct_pick_month> ::=
  | "january"
  | "february"
  | "march"
  | "april"
  | "may"
  | "june"
  | "july"
  | "august"
  | "september"
  | "october"
  | "november"
  | "december"
  | ... (all prefixes that allow unique match of any of above)
  | ... (all other variants with different casing of above)

<month> ::=
  | <human_int_month>
  | <direct_pick_month>

<year> ::=
  | "0" | ... | "9999" (or whatever the exact numbers that are representable in Ptime, TODO)

<hms_range> ::=
  | <hms> "to" <hms>

<hms_ranges> ::=
  | <hms_range> [, <hms_ranges>]

<hmss> ::=
  | <hms> [',' <hmss>]

<time_point_expr> ::=
  |                                                <hms>
  |                                    <day>       <hms>
  |            <month>                 <month_day> <hms>
  | <year>     <direct_pick_month>     <month_day> <hms>
  | <year> '-' <human_int_month>   '-' <month_day> <hms>

<branch_op> ::=
  | "next-batch"
  | "next-" <nat> "-batch"
  | "every-batch"

<branching_time_point_expr_atom> ::=
  |                          <weekdays>   '.' <hmss>
  |                          <month_days> '.' <hmss>
  |             <months> '.' <month_days> '.' <hmss>
  | <years> '.' <months> '.' <month_days> '.' <hmss>
  | <hmss> "of" <weekdays>
  | <hmss> "of" <month_days>
  | <hmss> "of" <month_days> "of" <months>
  | <hmss> "of" <month_days> "of" <months> "of" <years>

<branching_time_point_expr> ::=
  |             <branching_time_point_expr_atom>
  | <branch_op> <branching_time_point_expr_atom>

<month_day_range> ::=
  | <month_day> "to" <month_day>

<month_days> ::=
  | <month_day> [',' <month_days>]
  | <month_day_range> [',' <month_days>]

<weekday_range> ::=
  | <weekday> "to" <weekday>

<weekdays> ::=
  | <weekday> [',' <weekdays>]
  | <weekday_range> [',' <weekdays>]

<month_range> ::=
  | <month> "to" <month>

<months> ::=
  | <month> [',' <months>]
  | <month_range> [',' <months>]

<year_range> ::=
  | <year> "to" <year>

<years> ::=
  | <year> [',' <years>]
  | <year_range> [',' <years>]

<time_slot_expr> ::=
  | <time_point_expr> "to" <time_point_expr>

<branching_time_slot_expr_atom>
  |                          <weekdays>   '.' <hms_ranges>
  |                          <month_days> '.' <hms_ranges>
  |             <months> '.' <month_days> '.' <hms_ranges>
  | <years> '.' <months> '.' <month_days> '.' <hms_ranges>
  | <hms_ranges> "of" <weekdays>
  | <hms_ranges> "of" <month_days>
  | <hms_ranges> "of" <month_days> "of" <months>
  | <hms_ranges> "of" <month_days> "of" <months> "of" <years>

<branching_time_slot_expr> ::=
  |             <branching_time_slot_expr>
  | <branch_op> <branching_time_slot_expr>

<cron_expr> ::=
  TODO

<time_pattern> ::=
  | <cron_expr>
  | [ 'y' [ '[' <years>      ']' ] ]
    [ 'm' [ '[' <months>     ']' ] ]
    [ 'w' [ '[' <weekdays>   ']' ] ]
    [ 'd' [ '[' <month_days> ']' ] ]
      'h' [ '[' <hours>      ']' ]
      'm' [ '[' <minutes>    ']' ]
    [ 's' [ '[' <seconds>    ']' ] ]

<time_expr> ::=
  | <time_expr_union_part>
  | <time_expr> "||" <time_expr_union_part>

<time_expr_union_part> ::=
  | <time_expr_ordered_select_part>
  | <time_expr_union_part> "&&" <time_expr_ordered_select_part>

<time_expr_ordered_select_part> ::=
  | <time_expr_inter_part>
  | <time_expr_ordered_select_part> "||" <time_expr_inter_part>

<unary_op> ::=
  | "not"
  | "next-slot"
  | "next-point"
  | "next-" <nat> "-slot"
  | "next-" <nat> "-point"
  | "every"

<time_expr_inter_part> ::=
  | <time_expr_group>
  | <unary_op> <time_expr_inter_part>

<time_expr_group> ::=
  | <time_expr_atom>
  | '(' <time_expr> ')'

<time_expr_atom> ::=
  | <time_point_expr>
  | <branching_time_point_expr>
  | <time_slot_expr>
  | <branching_time_slot_expr>
  | <time_pattern>
```

## Semantics

Semantic functions:

```
eval_time_point_expr : time_point_expr -> (int64 * int64) Seq.t
eval_time_slot_expr : time_slot_expr -> (int64 * int64) Seq.t
eval_time_pattern : time_pattern -> (int64 * int64) Seq.t
eval_time_expr : time_expr -> (int64 * int64) Seq.t
```

Semantic equations:

```
TODO
```
