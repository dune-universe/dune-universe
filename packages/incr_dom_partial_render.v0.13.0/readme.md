# Incr_dom_partial_render

This library contains Incr_dom components and helper functions for constructing 
partially-rendered UI's.  Generally speaking, a partially-rendered component is 
one which understands which parts of it are visible on the page, and can decide 
how to show or hide parts of UI in order to achieve better DOM performance with 
fewer elements on the page.  Similarly, hidden elements do not need incremental
updates, so incremental graph recomputation can be avoided for large subsets of 
your model.

# Table

`Table` provides tabular views capable of:

- Partial rendering (only the visible rows are actually rendered)
- Headers that float as you scroll
- Sorting the table on one or more columns

It's designed to be used for large tables (at least thousands of
rows). There is no need to use Table for a 3x3 table.

See [[./doc/table.md][=Table='s README]] for more details on usage and a minimal working
example app, and see the [[../incr_dom/example/ts_gui/README.md][Ts_gui
example app]] for a more sophisticated (and more complicated) use of
the widget.
