/* The notations of this file are to be thought for the Bigarray
   fortran_layout.  Macros are used to transform it for the C layout. */

CAMLexport value NAME(value switches,
                      value mesh_in,
                      value triangle_area)
{
  CAMLparam3(switches, mesh_in, triangle_area);
  CAMLlocal2(vba, tuple);
  struct caml_ba_array *ba;
  struct triangulateio in, out, vor;
  long int dims[2];

  /* IN structure */
  ba = BA_METHOD(mesh_in, meth_point);
  in.pointlist               = MAT_OF_BA(ba);
  in.numberofpoints          = ba->dim[DIM_2ND];
  ba = BA_METHOD(mesh_in, meth_point_attribute);
  in.pointattributelist      = MAT_OF_BA(ba);
  in.numberofpointattributes = ba->dim[DIM_1ST];
  ba = BA_METHOD(mesh_in, meth_point_marker);
  if (ba->dim[0] == 0)
    in.pointmarkerlist       = NULL;
  else {
    COPY_INT_BA(in.pointmarkerlist, ba, ba->dim[0]);
  }

  ba = BA_METHOD(mesh_in, meth_triangle);
  if ((in.numberoftriangles = ba->dim[DIM_2ND]) == 0) {
    in.numberofcorners = 0;
    in.trianglelist = NULL;
    in.numberoftriangleattributes = 0;
    in.triangleattributelist = NULL;
  }
  else {
    /* For mesh refinement (with `r' switch) */
    in.numberofcorners         = ba->dim[DIM_1ST];
    COPY_INT_BA(in.trianglelist, ba, in.numberoftriangles * in.numberofcorners);
    ba = BA_METHOD(mesh_in, meth_triangle_attribute);
    in.numberoftriangleattributes = ba->dim[DIM_1ST];
    in.triangleattributelist   = MAT_OF_BA(ba);
    in.trianglearealist = VEC_OF_BA(Bigarray_val(triangle_area));
  }
  in.neighborlist = NULL;
  
  ba = BA_METHOD(mesh_in, mesh_segment);
  in.numberofsegments        = ba->dim[DIM_2ND];
  COPY_INT_BA(in.segmentlist, ba, 2 * in.numberofsegments);
  ba = BA_METHOD(mesh_in, meth_segment_marker);
  if (ba->dim[0] == 0)
    in.segmentmarkerlist     = NULL;
  else {
    COPY_INT_BA(in.segmentmarkerlist, ba, in.numberofsegments);
  }
  
  ba = BA_METHOD(mesh_in, meth_hole);
  in.numberofholes           = ba->dim[DIM_2ND];
  in.holelist                = MAT_OF_BA(ba);
  ba = BA_METHOD(mesh_in, meth_region);
  in.numberofregions         = ba->dim[DIM_2ND];
  in.regionlist              = MAT_OF_BA(ba);
  /* in.neighborlist in.edgelist in.edgemarkerlist in.normlist
   * in.numberofedges ignored for input. */

  /* OUT structure ("all scalars may be ignored") */
  out.pointlist = NULL;
  out.pointattributelist = NULL;
  out.pointmarkerlist = NULL;
  out.trianglelist = NULL;
  out.triangleattributelist = NULL;
  out.neighborlist = NULL;
  out.segmentlist = NULL;
  out.segmentmarkerlist = NULL;
  out.edgelist = NULL;
  out.edgemarkerlist = NULL;
  
  /* Voronoi structure */
  vor.pointlist = NULL;
  vor.pointattributelist = NULL;
  vor.numberofpoints = 0;
  vor.edgelist = NULL;
  vor.normlist = NULL;
  vor.numberofedges = 0;

  /* Call [triangle] */
  triangulate(String_val(switches), &in, &out, &vor);

  /* free the copied integer matrices for input */
  if (in.pointmarkerlist != NULL) free(in.pointmarkerlist);  
  if (in.trianglelist != NULL) free(in.trianglelist);
  if (in.segmentlist != NULL) free(in.segmentlist);
  if (in.segmentmarkerlist != NULL) free(in.segmentmarkerlist);
  
  /* Create an OCaml structure from [out] */
  tuple = alloc(14, 0);

  /* Point.  The array has been allocated by triangle but must be
     freed by OCaml => MANAGED. */
  dims[DIM_1ST] = 2;
  dims[DIM_2ND] = out.numberofpoints;
  vba = alloc_bigarray(PREC | LAYOUT | BIGARRAY_MANAGED,
                       2, out.pointlist, dims);
  Store_field(tuple, 0, vba);
  /* point_attribute */
  dims[DIM_1ST] = out.numberofpointattributes;
  vba = alloc_bigarray(PREC | LAYOUT | BIGARRAY_MANAGED,
                       2, out.pointattributelist, dims);
  Store_field(tuple, 1, vba);
  /* point_marker */
  dims[0] = out.numberofpoints;
  COPY_BA_INT(vba, 1, out.pointmarkerlist, dims);
  Store_field(tuple, 2, vba);
  /* triangle */
  dims[DIM_1ST] = out.numberofcorners;
  dims[DIM_2ND] = out.numberoftriangles;
  COPY_BA_INT(vba, 2, out.trianglelist, dims);
  Store_field(tuple, 3, vba);
  /* triangle_attribute */
  dims[DIM_1ST] = out.numberoftriangleattributes;
  vba = alloc_bigarray(PREC | LAYOUT | BIGARRAY_MANAGED,
                       2, out.triangleattributelist, dims);
  Store_field(tuple, 4, vba);
  /* neighbor */
  dims[DIM_1ST] = 3;
  COPY_BA_INT(vba, 2, out.neighborlist, dims);
  Store_field(tuple, 5, vba);
  /* segment */
  dims[DIM_1ST] = 2;
  dims[DIM_2ND] = out.numberofsegments;
  COPY_BA_INT(vba, 2, out.segmentlist, dims);
  Store_field(tuple, 6, vba);
  /* segment_marker */
  dims[0] = out.numberofsegments;
  COPY_BA_INT(vba, 1, out.segmentmarkerlist, dims);
  Store_field(tuple, 7, vba);
  /* edge */  
  dims[DIM_1ST] = 2;
  dims[DIM_2ND] = out.numberofedges;
  COPY_BA_INT(vba, 2, out.edgelist, dims);
  Store_field(tuple, 8, vba);
  /* edge_marker */
  dims[0] = out.numberofedges;
  COPY_BA_INT(vba, 1, out.edgemarkerlist, dims);
  Store_field(tuple, 9, vba);
  
  /* Add the fields for [vor] */
  /* point */
  dims[DIM_1ST] = 2;
  dims[DIM_2ND] = vor.numberofpoints;
  vba = alloc_bigarray(PREC | LAYOUT | BIGARRAY_MANAGED,
                       2, vor.pointlist, dims);
  Store_field(tuple, 10, vba);
  /* point_attribute */
  dims[DIM_1ST] = vor.numberofpointattributes;
  vba = alloc_bigarray(PREC | LAYOUT | BIGARRAY_MANAGED,
                       2, vor.pointattributelist, dims);
  Store_field(tuple, 11, vba);
  /* edge */
  dims[DIM_1ST] = 2;
  dims[DIM_2ND] = vor.numberofedges;
  COPY_BA_INT(vba, 2, vor.edgelist, dims);
  Store_field(tuple, 12, vba);
  /* normal */
  vba = alloc_bigarray(PREC | LAYOUT | BIGARRAY_MANAGED,
                       2, vor.normlist, dims);
  Store_field(tuple, 13, vba);

  CAMLreturn(tuple);
}

#undef NAME
#undef DIM_1ST
#undef DIM_2ND
#undef LAYOUT
