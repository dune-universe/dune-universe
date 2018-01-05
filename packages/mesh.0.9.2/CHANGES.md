0.9.2 2017-12-25
----------------

- Compile `EasyMesh.exe` and update the parsing of its output.
- Make checks in `Mesh_triangle` more uniform.
- Fix bug in the Triangle bindings.
- Remove all occurrences of `Obj.magic` to ensure polymorphism
  w.r.t. the layout.  Does not change the interface.

0.9.1 2017-12-18
----------------

- Add `Mesh.create` and `Mesh_triangle.create`.
- Slightly improve the documentation.
- `Mesh.scilab`: improve suggested command to export to PDF.

0.9.0 2017-12-14
----------------

- Rename `Mesh_display` as `Mesh_graphics` and `Easymesh` as
  `Mesh_easymesh`.
- Mesh.scilab: check the vector (representing the function) size and
  make the files more portable.
- Install EasyMesh program as part of the library.
- Fix some typos.

0.8.9 2017-10-30
----------------

- Split the package into `mesh`, `mesh-display`, `mesh-easymesh` and
  `mesh-triangle`.
- Port to `jbuilder` and `topkg`.
