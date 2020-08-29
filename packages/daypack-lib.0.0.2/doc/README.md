# Supplementary documentation

The documentation here contains tutorials and other info that does not fit into library documentation

Refer to the library documentation for information on individual types and functions

## Tutorial

- [Time](tutorial/time.md)

  - Basic time handling

- [Time slots](tutorial/time_slots.md)

  - Time slots/intervals manipulations for general use and also for performing set operations

- Time finding

  - [Time pattern](tutorial/time_pattern.md)

    - Finding time or time segments using a pattern similar to cron expression, but slightly
      more powerful
    
  - [Time expression](tutorial/time_expr.md)

    - A small language for specifying complex time segments
    
    - Aims to be a more expressive and user-friendly layer over time pattern

- [Schedule](tutorial/sched.md)

  - Schedule implementation which provides:
  
    - Bookkeeping of task, task instances, task segments

    - Progress tracking
    
    - Scheduling requests (essentially time constraints)
    
    - Time based indexing of scheduled items, allowing efficient navigation of the agenda based on time
