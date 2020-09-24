#order-xfce-i3

This is a small utility that allow you to keep a synchronized order between i3 tabs and the xfce pannel window buttons plugin.
In order to do so, you will need to have your xfce pannel window buttons sorting order setting set to "Group title and window title", 
and then you can run the order_xfce_i3 executable.
While it is running, every time you do an action that may change the order of the tabs (create a new window for instance) it will reorder the i3 tabs to be sorted 
lexicographically according to group name and then window name.
The order may not be synchronised if there are multiple windows with same group and window name, however, as soon as this situation changes, it will resynchronize. 
A common case is multiple terminals with the same current directory.

It is written in OCaml using ocaml-i3ipc.

This is not a perfect tool, it would be better in my opinion to move the order of the window button than the i3 tabs, 
as this would allow one to still use i3 shortcuts to place tabs windows where he would see fit.
One possible way to do this would be to edit the timestamp that is offered as a sorting method in xfce pannel window buttons plugin,
however I could not find trace of any kind of timestamp by reading about X11, and I am afraid this is a property stored by xfce. 

Information on this topic is very much welcome, as any feature request or suggestion.
