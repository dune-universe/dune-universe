/* File generated from plplot_core.idl */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include "camlidlruntime.h"


#include "plplot_core.h"

int camlidl_transl_table_plplot_core_enum_1[11] = {
  PL_DIFFUSE,
  PL_DRAW_LINEX,
  PL_DRAW_LINEY,
  PL_DRAW_LINEXY,
  PL_MAG_COLOR,
  PL_BASE_CONT,
  PL_TOP_CONT,
  PL_SURF_CONT,
  PL_DRAW_SIDES,
  PL_FACETED,
  PL_MESH,
};

int camlidl_ml2c_plplot_core_enum_plplot3d_style_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_1[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot3d_style_enum(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_plplot_core_enum_1, 11, "enum plplot3d_style_enum: bad enum plplot3d_style_enum value");
  return _v2;
}

void camlidl_ml2c_plplot_core_plplot3d_style(value _v1, plplot3d_style * _c2, camlidl_ctx _ctx)
{
  (*_c2) = convert_flag_list(_v1, camlidl_transl_table_plplot_core_enum_1);
}

value camlidl_c2ml_plplot_core_plplot3d_style(plplot3d_style * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc_flag_list((*_c2), camlidl_transl_table_plplot_core_enum_1, 11);
  return _v1;
}

int camlidl_transl_table_plplot_core_enum_2[4] = {
  PL_BIN_DEFAULT,
  PL_BIN_CENTRED,
  PL_BIN_NOEXPAND,
  PL_BIN_NOEMPTY,
};

int camlidl_ml2c_plplot_core_enum_plplot_bin_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_2[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_bin_enum(int _c1)
{
  value _v2;
  switch(_c1) {
  case PL_BIN_DEFAULT: _v2 = Val_int(0); break;
  case PL_BIN_CENTRED: _v2 = Val_int(1); break;
  case PL_BIN_NOEXPAND: _v2 = Val_int(2); break;
  case PL_BIN_NOEMPTY: _v2 = Val_int(3); break;
  default: invalid_argument("enum plplot_bin_enum: bad enum plplot_bin_enum value");
  }
  return _v2;
}

void camlidl_ml2c_plplot_core_plplot_bin_style(value _v1, plplot_bin_style * _c2, camlidl_ctx _ctx)
{
  (*_c2) = convert_flag_list(_v1, camlidl_transl_table_plplot_core_enum_2);
}

value camlidl_c2ml_plplot_core_plplot_bin_style(plplot_bin_style * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc_flag_list((*_c2), camlidl_transl_table_plplot_core_enum_2, 4);
  return _v1;
}

int camlidl_transl_table_plplot_core_enum_3[5] = {
  PL_HIST_DEFAULT,
  PL_HIST_NOSCALING,
  PL_HIST_IGNORE_OUTLIERS,
  PL_HIST_NOEXPAND,
  PL_HIST_NOEMPTY,
};

int camlidl_ml2c_plplot_core_enum_plplot_hist_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_3[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_hist_enum(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_plplot_core_enum_3, 5, "enum plplot_hist_enum: bad enum plplot_hist_enum value");
  return _v2;
}

void camlidl_ml2c_plplot_core_plplot_hist_style(value _v1, plplot_hist_style * _c2, camlidl_ctx _ctx)
{
  (*_c2) = convert_flag_list(_v1, camlidl_transl_table_plplot_core_enum_3);
}

value camlidl_c2ml_plplot_core_plplot_hist_style(plplot_hist_style * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc_flag_list((*_c2), camlidl_transl_table_plplot_core_enum_3, 5);
  return _v1;
}

int camlidl_transl_table_plplot_core_enum_4[4] = {
  PL_UNINITIALIZED,
  PL_INITIALIZED,
  PL_VIEWPORT_DEFINED,
  PL_WORLD_COORDINATES_DEFINED,
};

int camlidl_ml2c_plplot_core_enum_plplot_run_level_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_4[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_run_level_enum(int _c1)
{
  value _v2;
  switch(_c1) {
  case PL_UNINITIALIZED: _v2 = Val_int(0); break;
  case PL_INITIALIZED: _v2 = Val_int(1); break;
  case PL_VIEWPORT_DEFINED: _v2 = Val_int(2); break;
  case PL_WORLD_COORDINATES_DEFINED: _v2 = Val_int(3); break;
  default: invalid_argument("enum plplot_run_level_enum: bad enum plplot_run_level_enum value");
  }
  return _v2;
}

void camlidl_ml2c_plplot_core_plplot_run_level(value _v1, plplot_run_level * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_ml2c_plplot_core_enum_plplot_run_level_enum(_v1);
}

value camlidl_c2ml_plplot_core_plplot_run_level(plplot_run_level * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_plplot_core_enum_plplot_run_level_enum((*_c2));
  return _v1;
}

int camlidl_transl_table_plplot_core_enum_5[8] = {
  PL_POSITION_LEFT,
  PL_POSITION_RIGHT,
  PL_POSITION_TOP,
  PL_POSITION_BOTTOM,
  PL_POSITION_INSIDE,
  PL_POSITION_OUTSIDE,
  PL_POSITION_VIEWPORT,
  PL_POSITION_SUBPAGE,
};

int camlidl_ml2c_plplot_core_enum_plplot_position_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_5[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_position_enum(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_plplot_core_enum_5, 8, "enum plplot_position_enum: bad enum plplot_position_enum value");
  return _v2;
}

void camlidl_ml2c_plplot_core_plplot_position_opt(value _v1, plplot_position_opt * _c2, camlidl_ctx _ctx)
{
  (*_c2) = convert_flag_list(_v1, camlidl_transl_table_plplot_core_enum_5);
}

value camlidl_c2ml_plplot_core_plplot_position_opt(plplot_position_opt * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc_flag_list((*_c2), camlidl_transl_table_plplot_core_enum_5, 8);
  return _v1;
}

int camlidl_transl_table_plplot_core_enum_6[8] = {
  PL_LEGEND_NONE,
  PL_LEGEND_COLOR_BOX,
  PL_LEGEND_LINE,
  PL_LEGEND_SYMBOL,
  PL_LEGEND_TEXT_LEFT,
  PL_LEGEND_BACKGROUND,
  PL_LEGEND_BOUNDING_BOX,
  PL_LEGEND_ROW_MAJOR,
};

int camlidl_ml2c_plplot_core_enum_plplot_legend_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_6[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_legend_enum(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_plplot_core_enum_6, 8, "enum plplot_legend_enum: bad enum plplot_legend_enum value");
  return _v2;
}

void camlidl_ml2c_plplot_core_plplot_legend_opt(value _v1, plplot_legend_opt * _c2, camlidl_ctx _ctx)
{
  (*_c2) = convert_flag_list(_v1, camlidl_transl_table_plplot_core_enum_6);
}

value camlidl_c2ml_plplot_core_plplot_legend_opt(plplot_legend_opt * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc_flag_list((*_c2), camlidl_transl_table_plplot_core_enum_6, 8);
  return _v1;
}

int camlidl_transl_table_plplot_core_enum_7[17] = {
  PL_COLORBAR_LABEL_LEFT,
  PL_COLORBAR_LABEL_RIGHT,
  PL_COLORBAR_LABEL_TOP,
  PL_COLORBAR_LABEL_BOTTOM,
  PL_COLORBAR_IMAGE,
  PL_COLORBAR_SHADE,
  PL_COLORBAR_GRADIENT,
  PL_COLORBAR_CAP_NONE,
  PL_COLORBAR_CAP_LOW,
  PL_COLORBAR_CAP_HIGH,
  PL_COLORBAR_SHADE_LABEL,
  PL_COLORBAR_ORIENT_RIGHT,
  PL_COLORBAR_ORIENT_TOP,
  PL_COLORBAR_ORIENT_LEFT,
  PL_COLORBAR_ORIENT_BOTTOM,
  PL_COLORBAR_BACKGROUND,
  PL_COLORBAR_BOUNDING_BOX,
};

int camlidl_ml2c_plplot_core_enum_plplot_colorbar_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_7[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_colorbar_enum(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_plplot_core_enum_7, 17, "enum plplot_colorbar_enum: bad enum plplot_colorbar_enum value");
  return _v2;
}

void camlidl_ml2c_plplot_core_plplot_colorbar_opt(value _v1, plplot_colorbar_opt * _c2, camlidl_ctx _ctx)
{
  (*_c2) = convert_flag_list(_v1, camlidl_transl_table_plplot_core_enum_7);
}

value camlidl_c2ml_plplot_core_plplot_colorbar_opt(plplot_colorbar_opt * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc_flag_list((*_c2), camlidl_transl_table_plplot_core_enum_7, 17);
  return _v1;
}

int camlidl_transl_table_plplot_core_enum_8[6] = {
  PL_FCI_FAMILY_UNCHANGED,
  PL_FCI_SANS,
  PL_FCI_SERIF,
  PL_FCI_MONO,
  PL_FCI_SCRIPT,
  PL_FCI_SYMBOL,
};

int camlidl_ml2c_plplot_core_enum_plplot_fci_family_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_8[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_fci_family_enum(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_plplot_core_enum_8, 6, "enum plplot_fci_family_enum: bad enum plplot_fci_family_enum value");
  return _v2;
}

int camlidl_transl_table_plplot_core_enum_9[4] = {
  PL_FCI_STYLE_UNCHANGED,
  PL_FCI_UPRIGHT,
  PL_FCI_ITALIC,
  PL_FCI_OBLIQUE,
};

int camlidl_ml2c_plplot_core_enum_plplot_fci_style_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_9[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_fci_style_enum(int _c1)
{
  value _v2;
  switch(_c1) {
  case PL_FCI_STYLE_UNCHANGED: _v2 = Val_int(0); break;
  case PL_FCI_UPRIGHT: _v2 = Val_int(1); break;
  case PL_FCI_ITALIC: _v2 = Val_int(2); break;
  case PL_FCI_OBLIQUE: _v2 = Val_int(3); break;
  default: invalid_argument("enum plplot_fci_style_enum: bad enum plplot_fci_style_enum value");
  }
  return _v2;
}

int camlidl_transl_table_plplot_core_enum_10[3] = {
  PL_FCI_WEIGHT_UNCHANGED,
  PL_FCI_MEDIUM,
  PL_FCI_BOLD,
};

int camlidl_ml2c_plplot_core_enum_plplot_fci_weight_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_10[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_fci_weight_enum(int _c1)
{
  value _v2;
  switch(_c1) {
  case PL_FCI_WEIGHT_UNCHANGED: _v2 = Val_int(0); break;
  case PL_FCI_MEDIUM: _v2 = Val_int(1); break;
  case PL_FCI_BOLD: _v2 = Val_int(2); break;
  default: invalid_argument("enum plplot_fci_weight_enum: bad enum plplot_fci_weight_enum value");
  }
  return _v2;
}

int camlidl_transl_table_plplot_core_enum_11[4] = {
  PL_DRAWMODE_UNKNOWN,
  PL_DRAWMODE_DEFAULT,
  PL_DRAWMODE_REPLACE,
  PL_DRAWMODE_XOR,
};

int camlidl_ml2c_plplot_core_enum_plplot_draw_mode_enum(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_plplot_core_enum_11[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_plplot_core_enum_plplot_draw_mode_enum(int _c1)
{
  value _v2;
  switch(_c1) {
  case PL_DRAWMODE_UNKNOWN: _v2 = Val_int(0); break;
  case PL_DRAWMODE_DEFAULT: _v2 = Val_int(1); break;
  case PL_DRAWMODE_REPLACE: _v2 = Val_int(2); break;
  case PL_DRAWMODE_XOR: _v2 = Val_int(3); break;
  default: invalid_argument("enum plplot_draw_mode_enum: bad enum plplot_draw_mode_enum value");
  }
  return _v2;
}

void camlidl_ml2c_plplot_core_nonzero_error_int(value _v1, nonzero_error_int * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_plplot_core_nonzero_error_int(nonzero_error_int * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((*_c2));
  return _v1;
}

value camlidl_plplot_core_c_pl_setcontlabelformat(
	value _v_lexp,
	value _v_sigdig)
{
  int lexp; /*in*/
  int sigdig; /*in*/
  lexp = Int_val(_v_lexp);
  sigdig = Int_val(_v_sigdig);
  c_pl_setcontlabelformat(lexp, sigdig);
  return Val_unit;
}

value camlidl_plplot_core_c_pl_setcontlabelparam(
	value _v_offset,
	value _v_size,
	value _v_spacing,
	value _v_active)
{
  double offset; /*in*/
  double size; /*in*/
  double spacing; /*in*/
  int active; /*in*/
  offset = Double_val(_v_offset);
  size = Double_val(_v_size);
  spacing = Double_val(_v_spacing);
  active = Int_val(_v_active);
  c_pl_setcontlabelparam(offset, size, spacing, active);
  return Val_unit;
}

value camlidl_plplot_core_c_pladv(
	value _v_page)
{
  int page; /*in*/
  page = Int_val(_v_page);
  c_pladv(page);
  return Val_unit;
}

value camlidl_plplot_core_c_plarc(
	value _v_x,
	value _v_y,
	value _v_a,
	value _v_b,
	value _v_angle1,
	value _v_angle2,
	value _v_rotate,
	value _v_fill)
{
  double x; /*in*/
  double y; /*in*/
  double a; /*in*/
  double b; /*in*/
  double angle1; /*in*/
  double angle2; /*in*/
  double rotate; /*in*/
  int fill; /*in*/
  x = Double_val(_v_x);
  y = Double_val(_v_y);
  a = Double_val(_v_a);
  b = Double_val(_v_b);
  angle1 = Double_val(_v_angle1);
  angle2 = Double_val(_v_angle2);
  rotate = Double_val(_v_rotate);
  fill = Int_val(_v_fill);
  c_plarc(x, y, a, b, angle1, angle2, rotate, fill);
  return Val_unit;
}

value camlidl_plplot_core_c_plarc_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plarc(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}

value camlidl_plplot_core_c_plaxes(
	value _v_x0,
	value _v_y0,
	value _v_xopt,
	value _v_xtick,
	value _v_nxsub,
	value _v_yopt,
	value _v_ytick,
	value _v_nysub)
{
  double x0; /*in*/
  double y0; /*in*/
  char const *xopt; /*in*/
  double xtick; /*in*/
  int nxsub; /*in*/
  char const *yopt; /*in*/
  double ytick; /*in*/
  int nysub; /*in*/
  x0 = Double_val(_v_x0);
  y0 = Double_val(_v_y0);
  xopt = String_val(_v_xopt);
  xtick = Double_val(_v_xtick);
  nxsub = Int_val(_v_nxsub);
  yopt = String_val(_v_yopt);
  ytick = Double_val(_v_ytick);
  nysub = Int_val(_v_nysub);
  c_plaxes(x0, y0, xopt, xtick, nxsub, yopt, ytick, nysub);
  return Val_unit;
}

value camlidl_plplot_core_c_plaxes_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plaxes(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}

value camlidl_plplot_core_c_plbin(
	value _v_x,
	value _v_y,
	value _v_opt)
{
  int nbin; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  plplot_bin_style opt; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  nbin = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  nbin = _c3;
  camlidl_ml2c_plplot_core_plplot_bin_style(_v_opt, &opt, _ctx);
  c_plbin(nbin, x, y, opt);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plbtime(
	value _v_ctime)
{
  int *year; /*out*/
  int *month; /*out*/
  int *day; /*out*/
  int *hour; /*out*/
  int *min; /*out*/
  double *sec; /*out*/
  double ctime; /*in*/
  int _c1;
  int _c2;
  int _c3;
  int _c4;
  int _c5;
  double _c6;
  value _vresult;
  value _vres[6] = { 0, 0, 0, 0, 0, 0, };

  ctime = Double_val(_v_ctime);
  year = &_c1;
  month = &_c2;
  day = &_c3;
  hour = &_c4;
  min = &_c5;
  sec = &_c6;
  c_plbtime(year, month, day, hour, min, sec, ctime);
  Begin_roots_block(_vres, 6)
    _vres[0] = Val_int(*year);
    _vres[1] = Val_int(*month);
    _vres[2] = Val_int(*day);
    _vres[3] = Val_int(*hour);
    _vres[4] = Val_int(*min);
    _vres[5] = copy_double(*sec);
    _vresult = camlidl_alloc_small(6, 0);
    { mlsize_t _c7;
      for (_c7 = 0; _c7 < 6; _c7++) Field(_vresult, _c7) = _vres[_c7];
    }
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plbop(value _unit)
{
  c_plbop();
  return Val_unit;
}

value camlidl_plplot_core_c_plbox(
	value _v_xopt,
	value _v_xtick,
	value _v_nxsub,
	value _v_yopt,
	value _v_ytick,
	value _v_nysub)
{
  char const *xopt; /*in*/
  double xtick; /*in*/
  int nxsub; /*in*/
  char const *yopt; /*in*/
  double ytick; /*in*/
  int nysub; /*in*/
  xopt = String_val(_v_xopt);
  xtick = Double_val(_v_xtick);
  nxsub = Int_val(_v_nxsub);
  yopt = String_val(_v_yopt);
  ytick = Double_val(_v_ytick);
  nysub = Int_val(_v_nysub);
  c_plbox(xopt, xtick, nxsub, yopt, ytick, nysub);
  return Val_unit;
}

value camlidl_plplot_core_c_plbox_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plbox(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_plbox3(
	value _v_xopt,
	value _v_xlabel,
	value _v_xtick,
	value _v_nsubx,
	value _v_yopt,
	value _v_ylabel,
	value _v_ytick,
	value _v_nsuby,
	value _v_zopt,
	value _v_zlabel,
	value _v_ztick,
	value _v_nsubz)
{
  char const *xopt; /*in*/
  char const *xlabel; /*in*/
  double xtick; /*in*/
  int nsubx; /*in*/
  char const *yopt; /*in*/
  char const *ylabel; /*in*/
  double ytick; /*in*/
  int nsuby; /*in*/
  char const *zopt; /*in*/
  char const *zlabel; /*in*/
  double ztick; /*in*/
  int nsubz; /*in*/
  xopt = String_val(_v_xopt);
  xlabel = String_val(_v_xlabel);
  xtick = Double_val(_v_xtick);
  nsubx = Int_val(_v_nsubx);
  yopt = String_val(_v_yopt);
  ylabel = String_val(_v_ylabel);
  ytick = Double_val(_v_ytick);
  nsuby = Int_val(_v_nsuby);
  zopt = String_val(_v_zopt);
  zlabel = String_val(_v_zlabel);
  ztick = Double_val(_v_ztick);
  nsubz = Int_val(_v_nsubz);
  c_plbox3(xopt, xlabel, xtick, nsubx, yopt, ylabel, ytick, nsuby, zopt, zlabel, ztick, nsubz);
  return Val_unit;
}

value camlidl_plplot_core_c_plbox3_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plbox3(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10], argv[11]);
}

value camlidl_plplot_core_c_plcalc_world(
	value _v_rx,
	value _v_ry)
{
  double rx; /*in*/
  double ry; /*in*/
  double *wx; /*out*/
  double *wy; /*out*/
  int *window; /*out*/
  double _c1;
  double _c2;
  int _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  rx = Double_val(_v_rx);
  ry = Double_val(_v_ry);
  wx = &_c1;
  wy = &_c2;
  window = &_c3;
  c_plcalc_world(rx, ry, wx, wy, window);
  Begin_roots_block(_vres, 3)
    _vres[0] = copy_double(*wx);
    _vres[1] = copy_double(*wy);
    _vres[2] = Val_int(*window);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plclear(value _unit)
{
  c_plclear();
  return Val_unit;
}

value camlidl_plplot_core_c_plcol0(
	value _v_icol0)
{
  int icol0; /*in*/
  icol0 = Int_val(_v_icol0);
  c_plcol0(icol0);
  return Val_unit;
}

value camlidl_plplot_core_c_plcol1(
	value _v_col1)
{
  double col1; /*in*/
  col1 = Double_val(_v_col1);
  c_plcol1(col1);
  return Val_unit;
}

value camlidl_plplot_core_c_plconfigtime(
	value _v_scale,
	value _v_offset1,
	value _v_offset2,
	value _v_ccontrol,
	value _v_ifbtime_offset,
	value _v_year,
	value _v_month,
	value _v_day,
	value _v_hour,
	value _v_min,
	value _v_sec)
{
  double scale; /*in*/
  double offset1; /*in*/
  double offset2; /*in*/
  int ccontrol; /*in*/
  int ifbtime_offset; /*in*/
  int year; /*in*/
  int month; /*in*/
  int day; /*in*/
  int hour; /*in*/
  int min; /*in*/
  double sec; /*in*/
  scale = Double_val(_v_scale);
  offset1 = Double_val(_v_offset1);
  offset2 = Double_val(_v_offset2);
  ccontrol = Int_val(_v_ccontrol);
  ifbtime_offset = Int_val(_v_ifbtime_offset);
  year = Int_val(_v_year);
  month = Int_val(_v_month);
  day = Int_val(_v_day);
  hour = Int_val(_v_hour);
  min = Int_val(_v_min);
  sec = Double_val(_v_sec);
  c_plconfigtime(scale, offset1, offset2, ccontrol, ifbtime_offset, year, month, day, hour, min, sec);
  return Val_unit;
}

value camlidl_plplot_core_c_plconfigtime_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plconfigtime(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}

value camlidl_plplot_core_c_plcpstrm(
	value _v_iplsr,
	value _v_flags)
{
  int iplsr; /*in*/
  int flags; /*in*/
  iplsr = Int_val(_v_iplsr);
  flags = Int_val(_v_flags);
  c_plcpstrm(iplsr, flags);
  return Val_unit;
}

value camlidl_plplot_core_c_plctime(
	value _v_year,
	value _v_month,
	value _v_day,
	value _v_hour,
	value _v_min,
	value _v_sec)
{
  int year; /*in*/
  int month; /*in*/
  int day; /*in*/
  int hour; /*in*/
  int min; /*in*/
  double sec; /*in*/
  double *ctime; /*out*/
  double _c1;
  value _vres;

  year = Int_val(_v_year);
  month = Int_val(_v_month);
  day = Int_val(_v_day);
  hour = Int_val(_v_hour);
  min = Int_val(_v_min);
  sec = Double_val(_v_sec);
  ctime = &_c1;
  c_plctime(year, month, day, hour, min, sec, ctime);
  _vres = copy_double(*ctime);
  return _vres;
}

value camlidl_plplot_core_c_plctime_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plctime(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_plend(value _unit)
{
  c_plend();
  return Val_unit;
}

value camlidl_plplot_core_c_plend1(value _unit)
{
  c_plend1();
  return Val_unit;
}

value camlidl_plplot_core_c_plenv(
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax,
	value _v_just,
	value _v_axis)
{
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  int just; /*in*/
  int axis; /*in*/
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  just = Int_val(_v_just);
  axis = Int_val(_v_axis);
  c_plenv(xmin, xmax, ymin, ymax, just, axis);
  return Val_unit;
}

value camlidl_plplot_core_c_plenv_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plenv(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_plenv0(
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax,
	value _v_just,
	value _v_axis)
{
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  int just; /*in*/
  int axis; /*in*/
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  just = Int_val(_v_just);
  axis = Int_val(_v_axis);
  c_plenv0(xmin, xmax, ymin, ymax, just, axis);
  return Val_unit;
}

value camlidl_plplot_core_c_plenv0_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plenv0(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_pleop(value _unit)
{
  c_pleop();
  return Val_unit;
}

value camlidl_plplot_core_c_plerrx(
	value _v_xmin,
	value _v_xmax,
	value _v_y)
{
  int n; /*in*/
  double *xmin; /*in*/
  double *xmax; /*in*/
  double *y; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_xmin) / Double_wosize;
  xmin = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    xmin[_c2] = Double_field(_v_xmin, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_xmax) / Double_wosize;
  xmax = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    xmax[_c4] = Double_field(_v_xmax, _c4);
  }
  n = _c3;
  _c5 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    y[_c6] = Double_field(_v_y, _c6);
  }
  n = _c5;
  c_plerrx(n, xmin, xmax, y);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plerry(
	value _v_x,
	value _v_ymin,
	value _v_ymax)
{
  int n; /*in*/
  double *x; /*in*/
  double *ymin; /*in*/
  double *ymax; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_ymin) / Double_wosize;
  ymin = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    ymin[_c4] = Double_field(_v_ymin, _c4);
  }
  n = _c3;
  _c5 = Wosize_val(_v_ymax) / Double_wosize;
  ymax = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    ymax[_c6] = Double_field(_v_ymax, _c6);
  }
  n = _c5;
  c_plerry(n, x, ymin, ymax);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plfamadv(value _unit)
{
  c_plfamadv();
  return Val_unit;
}

value camlidl_plplot_core_c_plfill(
	value _v_x,
	value _v_y)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  c_plfill(n, x, y);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plfill3(
	value _v_x,
	value _v_y,
	value _v_z)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  double *z; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  _c5 = Wosize_val(_v_z) / Double_wosize;
  z = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    z[_c6] = Double_field(_v_z, _c6);
  }
  n = _c5;
  c_plfill3(n, x, y, z);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plflush(value _unit)
{
  c_plflush();
  return Val_unit;
}

value camlidl_plplot_core_c_plfont(
	value _v_ifont)
{
  int ifont; /*in*/
  ifont = Int_val(_v_ifont);
  c_plfont(ifont);
  return Val_unit;
}

value camlidl_plplot_core_c_plfontld(
	value _v_fnt)
{
  int fnt; /*in*/
  fnt = Int_val(_v_fnt);
  c_plfontld(fnt);
  return Val_unit;
}

value camlidl_plplot_core_c_plgchr(value _unit)
{
  double *p_def; /*out*/
  double *p_ht; /*out*/
  double _c1;
  double _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  p_def = &_c1;
  p_ht = &_c2;
  c_plgchr(p_def, p_ht);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_double(*p_def);
    _vres[1] = copy_double(*p_ht);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgcmap1_range(value _unit)
{
  double *min_color; /*out*/
  double *max_color; /*out*/
  double _c1;
  double _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  min_color = &_c1;
  max_color = &_c2;
  c_plgcmap1_range(min_color, max_color);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_double(*min_color);
    _vres[1] = copy_double(*max_color);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgcol0(
	value _v_icol0)
{
  int icol0; /*in*/
  int *r; /*out*/
  int *g; /*out*/
  int *b; /*out*/
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  icol0 = Int_val(_v_icol0);
  r = &_c1;
  g = &_c2;
  b = &_c3;
  c_plgcol0(icol0, r, g, b);
  Begin_roots_block(_vres, 3)
    _vres[0] = Val_int(*r);
    _vres[1] = Val_int(*g);
    _vres[2] = Val_int(*b);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgcol0a(
	value _v_icol0)
{
  int icol0; /*in*/
  int *r; /*out*/
  int *g; /*out*/
  int *b; /*out*/
  double *a; /*out*/
  int _c1;
  int _c2;
  int _c3;
  double _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  icol0 = Int_val(_v_icol0);
  r = &_c1;
  g = &_c2;
  b = &_c3;
  a = &_c4;
  c_plgcol0a(icol0, r, g, b, a);
  Begin_roots_block(_vres, 4)
    _vres[0] = Val_int(*r);
    _vres[1] = Val_int(*g);
    _vres[2] = Val_int(*b);
    _vres[3] = copy_double(*a);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgcolbg(value _unit)
{
  int *r; /*out*/
  int *g; /*out*/
  int *b; /*out*/
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  r = &_c1;
  g = &_c2;
  b = &_c3;
  c_plgcolbg(r, g, b);
  Begin_roots_block(_vres, 3)
    _vres[0] = Val_int(*r);
    _vres[1] = Val_int(*g);
    _vres[2] = Val_int(*b);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgcolbga(value _unit)
{
  int *r; /*out*/
  int *g; /*out*/
  int *b; /*out*/
  double *a; /*out*/
  int _c1;
  int _c2;
  int _c3;
  double _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  r = &_c1;
  g = &_c2;
  b = &_c3;
  a = &_c4;
  c_plgcolbga(r, g, b, a);
  Begin_roots_block(_vres, 4)
    _vres[0] = Val_int(*r);
    _vres[1] = Val_int(*g);
    _vres[2] = Val_int(*b);
    _vres[3] = copy_double(*a);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgcompression(value _unit)
{
  int *compression; /*out*/
  int _c1;
  value _vres;

  compression = &_c1;
  c_plgcompression(compression);
  _vres = Val_int(*compression);
  return _vres;
}

value camlidl_plplot_core_c_plgdev(value _unit)
{
  char *p_dev; /*out*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  p_dev = camlidl_malloc(1024 * sizeof(char ), _ctx);
  c_plgdev(p_dev);
  _vres = copy_string(p_dev);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_plplot_core_c_plgdidev(value _unit)
{
  double *p_mar; /*out*/
  double *p_aspect; /*out*/
  double *p_jx; /*out*/
  double *p_jy; /*out*/
  double _c1;
  double _c2;
  double _c3;
  double _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  p_mar = &_c1;
  p_aspect = &_c2;
  p_jx = &_c3;
  p_jy = &_c4;
  c_plgdidev(p_mar, p_aspect, p_jx, p_jy);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_double(*p_mar);
    _vres[1] = copy_double(*p_aspect);
    _vres[2] = copy_double(*p_jx);
    _vres[3] = copy_double(*p_jy);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgdiori(value _unit)
{
  double *p_rot; /*out*/
  double _c1;
  value _vres;

  p_rot = &_c1;
  c_plgdiori(p_rot);
  _vres = copy_double(*p_rot);
  return _vres;
}

value camlidl_plplot_core_c_plgdiplt(value _unit)
{
  double *p_xmin; /*out*/
  double *p_ymin; /*out*/
  double *p_xmax; /*out*/
  double *p_ymax; /*out*/
  double _c1;
  double _c2;
  double _c3;
  double _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  p_xmin = &_c1;
  p_ymin = &_c2;
  p_xmax = &_c3;
  p_ymax = &_c4;
  c_plgdiplt(p_xmin, p_ymin, p_xmax, p_ymax);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_double(*p_xmin);
    _vres[1] = copy_double(*p_ymin);
    _vres[2] = copy_double(*p_xmax);
    _vres[3] = copy_double(*p_ymax);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgdrawmode(value _unit)
{
  int _res;
  value _vres;

  _res = c_plgdrawmode();
  _vres = camlidl_c2ml_plplot_core_enum_plplot_draw_mode_enum(_res);
  return _vres;
}

value camlidl_plplot_core_c_plgfci(value _unit)
{
  long long *pfci; /*out*/
  long long _c1;
  value _vres;

  pfci = &_c1;
  c_plgfci(pfci);
  _vres = copy_int64(*pfci);
  return _vres;
}

value camlidl_plplot_core_c_plgfam(value _unit)
{
  int *p_fam; /*out*/
  int *p_num; /*out*/
  int *p_bmax; /*out*/
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  p_fam = &_c1;
  p_num = &_c2;
  p_bmax = &_c3;
  c_plgfam(p_fam, p_num, p_bmax);
  Begin_roots_block(_vres, 3)
    _vres[0] = Val_int(*p_fam);
    _vres[1] = Val_int(*p_num);
    _vres[2] = Val_int(*p_bmax);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgfnam(value _unit)
{
  char *fnam; /*out*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  fnam = camlidl_malloc(1024 * sizeof(char ), _ctx);
  c_plgfnam(fnam);
  _vres = copy_string(fnam);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_plplot_core_c_plgfont(value _unit)
{
  int *p_family; /*out*/
  int *p_style; /*out*/
  int *p_weight; /*out*/
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  p_family = &_c1;
  p_style = &_c2;
  p_weight = &_c3;
  c_plgfont(p_family, p_style, p_weight);
  Begin_roots_block(_vres, 3)
    _vres[0] = Val_int(*p_family);
    _vres[1] = Val_int(*p_style);
    _vres[2] = Val_int(*p_weight);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plglevel(value _unit)
{
  plplot_run_level *p_level; /*out*/
  plplot_run_level _c1;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  p_level = &_c1;
  c_plglevel(p_level);
  _vres = camlidl_c2ml_plplot_core_plplot_run_level(&*p_level, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_plplot_core_c_plgpage(value _unit)
{
  double *p_xp; /*out*/
  double *p_yp; /*out*/
  int *p_xleng; /*out*/
  int *p_yleng; /*out*/
  int *p_xoff; /*out*/
  int *p_yoff; /*out*/
  double _c1;
  double _c2;
  int _c3;
  int _c4;
  int _c5;
  int _c6;
  value _vresult;
  value _vres[6] = { 0, 0, 0, 0, 0, 0, };

  p_xp = &_c1;
  p_yp = &_c2;
  p_xleng = &_c3;
  p_yleng = &_c4;
  p_xoff = &_c5;
  p_yoff = &_c6;
  c_plgpage(p_xp, p_yp, p_xleng, p_yleng, p_xoff, p_yoff);
  Begin_roots_block(_vres, 6)
    _vres[0] = copy_double(*p_xp);
    _vres[1] = copy_double(*p_yp);
    _vres[2] = Val_int(*p_xleng);
    _vres[3] = Val_int(*p_yleng);
    _vres[4] = Val_int(*p_xoff);
    _vres[5] = Val_int(*p_yoff);
    _vresult = camlidl_alloc_small(6, 0);
    { mlsize_t _c7;
      for (_c7 = 0; _c7 < 6; _c7++) Field(_vresult, _c7) = _vres[_c7];
    }
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgra(value _unit)
{
  c_plgra();
  return Val_unit;
}

value camlidl_plplot_core_c_plgradient(
	value _v_x,
	value _v_y,
	value _v_angle)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  double angle; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  angle = Double_val(_v_angle);
  c_plgradient(n, x, y, angle);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plgspa(value _unit)
{
  double *xmin; /*out*/
  double *xmax; /*out*/
  double *ymin; /*out*/
  double *ymax; /*out*/
  double _c1;
  double _c2;
  double _c3;
  double _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  xmin = &_c1;
  xmax = &_c2;
  ymin = &_c3;
  ymax = &_c4;
  c_plgspa(xmin, xmax, ymin, ymax);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_double(*xmin);
    _vres[1] = copy_double(*xmax);
    _vres[2] = copy_double(*ymin);
    _vres[3] = copy_double(*ymax);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgstrm(value _unit)
{
  int *p_strm; /*out*/
  int _c1;
  value _vres;

  p_strm = &_c1;
  c_plgstrm(p_strm);
  _vres = Val_int(*p_strm);
  return _vres;
}

value camlidl_plplot_core_c_plgver(value _unit)
{
  char *p_ver; /*out*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  p_ver = camlidl_malloc(1024 * sizeof(char ), _ctx);
  c_plgver(p_ver);
  _vres = copy_string(p_ver);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_plplot_core_c_plgvpd(value _unit)
{
  double *p_xmin; /*out*/
  double *p_xmax; /*out*/
  double *p_ymin; /*out*/
  double *p_ymax; /*out*/
  double _c1;
  double _c2;
  double _c3;
  double _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  p_xmin = &_c1;
  p_xmax = &_c2;
  p_ymin = &_c3;
  p_ymax = &_c4;
  c_plgvpd(p_xmin, p_xmax, p_ymin, p_ymax);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_double(*p_xmin);
    _vres[1] = copy_double(*p_xmax);
    _vres[2] = copy_double(*p_ymin);
    _vres[3] = copy_double(*p_ymax);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgvpw(value _unit)
{
  double *p_xmin; /*out*/
  double *p_xmax; /*out*/
  double *p_ymin; /*out*/
  double *p_ymax; /*out*/
  double _c1;
  double _c2;
  double _c3;
  double _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  p_xmin = &_c1;
  p_xmax = &_c2;
  p_ymin = &_c3;
  p_ymax = &_c4;
  c_plgvpw(p_xmin, p_xmax, p_ymin, p_ymax);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_double(*p_xmin);
    _vres[1] = copy_double(*p_xmax);
    _vres[2] = copy_double(*p_ymin);
    _vres[3] = copy_double(*p_ymax);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgxax(value _unit)
{
  int *p_digmax; /*out*/
  int *p_digits; /*out*/
  int _c1;
  int _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  p_digmax = &_c1;
  p_digits = &_c2;
  c_plgxax(p_digmax, p_digits);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(*p_digmax);
    _vres[1] = Val_int(*p_digits);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgyax(value _unit)
{
  int *p_digmax; /*out*/
  int *p_digits; /*out*/
  int _c1;
  int _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  p_digmax = &_c1;
  p_digits = &_c2;
  c_plgyax(p_digmax, p_digits);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(*p_digmax);
    _vres[1] = Val_int(*p_digits);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plgzax(value _unit)
{
  int *p_digmax; /*out*/
  int *p_digits; /*out*/
  int _c1;
  int _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  p_digmax = &_c1;
  p_digits = &_c2;
  c_plgzax(p_digmax, p_digits);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(*p_digmax);
    _vres[1] = Val_int(*p_digits);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plhist(
	value _v_data,
	value _v_datmin,
	value _v_datmax,
	value _v_nbin,
	value _v_opt)
{
  int n; /*in*/
  double *data; /*in*/
  double datmin; /*in*/
  double datmax; /*in*/
  int nbin; /*in*/
  plplot_hist_style opt; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_data) / Double_wosize;
  data = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    data[_c2] = Double_field(_v_data, _c2);
  }
  n = _c1;
  datmin = Double_val(_v_datmin);
  datmax = Double_val(_v_datmax);
  nbin = Int_val(_v_nbin);
  camlidl_ml2c_plplot_core_plplot_hist_style(_v_opt, &opt, _ctx);
  c_plhist(n, data, datmin, datmax, nbin, opt);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plhlsrgb(
	value _v_h,
	value _v_l,
	value _v_s)
{
  double h; /*in*/
  double l; /*in*/
  double s; /*in*/
  double *p_r; /*out*/
  double *p_g; /*out*/
  double *p_b; /*out*/
  double _c1;
  double _c2;
  double _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  h = Double_val(_v_h);
  l = Double_val(_v_l);
  s = Double_val(_v_s);
  p_r = &_c1;
  p_g = &_c2;
  p_b = &_c3;
  c_plhlsrgb(h, l, s, p_r, p_g, p_b);
  Begin_roots_block(_vres, 3)
    _vres[0] = copy_double(*p_r);
    _vres[1] = copy_double(*p_g);
    _vres[2] = copy_double(*p_b);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plinit(value _unit)
{
  c_plinit();
  return Val_unit;
}

value camlidl_plplot_core_c_pljoin(
	value _v_x1,
	value _v_y1,
	value _v_x2,
	value _v_y2)
{
  double x1; /*in*/
  double y1; /*in*/
  double x2; /*in*/
  double y2; /*in*/
  x1 = Double_val(_v_x1);
  y1 = Double_val(_v_y1);
  x2 = Double_val(_v_x2);
  y2 = Double_val(_v_y2);
  c_pljoin(x1, y1, x2, y2);
  return Val_unit;
}

value camlidl_plplot_core_c_pllab(
	value _v_xlabel,
	value _v_ylabel,
	value _v_tlabel)
{
  char const *xlabel; /*in*/
  char const *ylabel; /*in*/
  char const *tlabel; /*in*/
  xlabel = String_val(_v_xlabel);
  ylabel = String_val(_v_ylabel);
  tlabel = String_val(_v_tlabel);
  c_pllab(xlabel, ylabel, tlabel);
  return Val_unit;
}

value camlidl_plplot_core_c_pllightsource(
	value _v_x,
	value _v_y,
	value _v_z)
{
  double x; /*in*/
  double y; /*in*/
  double z; /*in*/
  x = Double_val(_v_x);
  y = Double_val(_v_y);
  z = Double_val(_v_z);
  c_pllightsource(x, y, z);
  return Val_unit;
}

value camlidl_plplot_core_c_plline(
	value _v_x,
	value _v_y)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  c_plline(n, x, y);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plline3(
	value _v_x,
	value _v_y,
	value _v_z)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  double *z; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  _c5 = Wosize_val(_v_z) / Double_wosize;
  z = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    z[_c6] = Double_field(_v_z, _c6);
  }
  n = _c5;
  c_plline3(n, x, y, z);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_pllsty(
	value _v_lin)
{
  int lin; /*in*/
  lin = Int_val(_v_lin);
  c_pllsty(lin);
  return Val_unit;
}

value camlidl_plplot_core_c_plmesh(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_opt)
{
  double *x; /*in*/
  double *y; /*in*/
  double **z; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  plplot3d_style opt; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  value _v7;
  mlsize_t _c8;
  mlsize_t _c9;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  nx = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  ny = _c3;
  _c5 = Wosize_val(_v_z);
  z = camlidl_malloc(_c5 * sizeof(double *), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    _v7 = Field(_v_z, _c6);
    _c8 = Wosize_val(_v7) / Double_wosize;
    z[_c6] = camlidl_malloc(_c8 * sizeof(double ), _ctx);
    for (_c9 = 0; _c9 < _c8; _c9++) {
      z[_c6][_c9] = Double_field(_v7, _c9);
    }
    ny = _c8;
  }
  nx = _c5;
  camlidl_ml2c_plplot_core_plplot3d_style(_v_opt, &opt, _ctx);
  c_plmesh(x, y, z, nx, ny, opt);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plmeshc(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_opt,
	value _v_clevel)
{
  double *x; /*in*/
  double *y; /*in*/
  double **z; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  plplot3d_style opt; /*in*/
  double *clevel; /*in*/
  int nlevel; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  value _v7;
  mlsize_t _c8;
  mlsize_t _c9;
  mlsize_t _c10;
  mlsize_t _c11;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  nx = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  ny = _c3;
  _c5 = Wosize_val(_v_z);
  z = camlidl_malloc(_c5 * sizeof(double *), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    _v7 = Field(_v_z, _c6);
    _c8 = Wosize_val(_v7) / Double_wosize;
    z[_c6] = camlidl_malloc(_c8 * sizeof(double ), _ctx);
    for (_c9 = 0; _c9 < _c8; _c9++) {
      z[_c6][_c9] = Double_field(_v7, _c9);
    }
    ny = _c8;
  }
  nx = _c5;
  camlidl_ml2c_plplot_core_plplot3d_style(_v_opt, &opt, _ctx);
  _c10 = Wosize_val(_v_clevel) / Double_wosize;
  clevel = camlidl_malloc(_c10 * sizeof(double ), _ctx);
  for (_c11 = 0; _c11 < _c10; _c11++) {
    clevel[_c11] = Double_field(_v_clevel, _c11);
  }
  nlevel = _c10;
  c_plmeshc(x, y, z, nx, ny, opt, clevel, nlevel);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plmkstrm(value _unit)
{
  int *p_strm; /*out*/
  int _c1;
  value _vres;

  p_strm = &_c1;
  c_plmkstrm(p_strm);
  _vres = Val_int(*p_strm);
  return _vres;
}

value camlidl_plplot_core_c_plmtex(
	value _v_side,
	value _v_disp,
	value _v_pos,
	value _v_just,
	value _v_text)
{
  char const *side; /*in*/
  double disp; /*in*/
  double pos; /*in*/
  double just; /*in*/
  char const *text; /*in*/
  side = String_val(_v_side);
  disp = Double_val(_v_disp);
  pos = Double_val(_v_pos);
  just = Double_val(_v_just);
  text = String_val(_v_text);
  c_plmtex(side, disp, pos, just, text);
  return Val_unit;
}

value camlidl_plplot_core_c_plmtex3(
	value _v_side,
	value _v_disp,
	value _v_pos,
	value _v_just,
	value _v_text)
{
  char const *side; /*in*/
  double disp; /*in*/
  double pos; /*in*/
  double just; /*in*/
  char const *text; /*in*/
  side = String_val(_v_side);
  disp = Double_val(_v_disp);
  pos = Double_val(_v_pos);
  just = Double_val(_v_just);
  text = String_val(_v_text);
  c_plmtex3(side, disp, pos, just, text);
  return Val_unit;
}

value camlidl_plplot_core_c_plot3d(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_opt,
	value _v_side)
{
  double *x; /*in*/
  double *y; /*in*/
  double **z; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  plplot3d_style opt; /*in*/
  int side; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  value _v7;
  mlsize_t _c8;
  mlsize_t _c9;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  nx = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  ny = _c3;
  _c5 = Wosize_val(_v_z);
  z = camlidl_malloc(_c5 * sizeof(double *), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    _v7 = Field(_v_z, _c6);
    _c8 = Wosize_val(_v7) / Double_wosize;
    z[_c6] = camlidl_malloc(_c8 * sizeof(double ), _ctx);
    for (_c9 = 0; _c9 < _c8; _c9++) {
      z[_c6][_c9] = Double_field(_v7, _c9);
    }
    ny = _c8;
  }
  nx = _c5;
  camlidl_ml2c_plplot_core_plplot3d_style(_v_opt, &opt, _ctx);
  side = Int_val(_v_side);
  c_plot3d(x, y, z, nx, ny, opt, side);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plot3dc(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_opt,
	value _v_clevel)
{
  double *x; /*in*/
  double *y; /*in*/
  double **z; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  plplot3d_style opt; /*in*/
  double *clevel; /*in*/
  int nlevel; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  value _v7;
  mlsize_t _c8;
  mlsize_t _c9;
  mlsize_t _c10;
  mlsize_t _c11;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  nx = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  ny = _c3;
  _c5 = Wosize_val(_v_z);
  z = camlidl_malloc(_c5 * sizeof(double *), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    _v7 = Field(_v_z, _c6);
    _c8 = Wosize_val(_v7) / Double_wosize;
    z[_c6] = camlidl_malloc(_c8 * sizeof(double ), _ctx);
    for (_c9 = 0; _c9 < _c8; _c9++) {
      z[_c6][_c9] = Double_field(_v7, _c9);
    }
    ny = _c8;
  }
  nx = _c5;
  camlidl_ml2c_plplot_core_plplot3d_style(_v_opt, &opt, _ctx);
  _c10 = Wosize_val(_v_clevel) / Double_wosize;
  clevel = camlidl_malloc(_c10 * sizeof(double ), _ctx);
  for (_c11 = 0; _c11 < _c10; _c11++) {
    clevel[_c11] = Double_field(_v_clevel, _c11);
  }
  nlevel = _c10;
  c_plot3dc(x, y, z, nx, ny, opt, clevel, nlevel);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plpat(
	value _v_inc,
	value _v_del)
{
  int nlin; /*in*/
  int *inc; /*in*/
  int *del; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_inc);
  inc = camlidl_malloc(_c1 * sizeof(int ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_inc, _c2);
    inc[_c2] = Int_val(_v3);
  }
  nlin = _c1;
  _c4 = Wosize_val(_v_del);
  del = camlidl_malloc(_c4 * sizeof(int ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_del, _c5);
    del[_c5] = Int_val(_v6);
  }
  nlin = _c4;
  c_plpat(nlin, inc, del);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plpath(
	value _v_n,
	value _v_x1,
	value _v_y1,
	value _v_x2,
	value _v_y2)
{
  int n; /*in*/
  double x1; /*in*/
  double y1; /*in*/
  double x2; /*in*/
  double y2; /*in*/
  n = Int_val(_v_n);
  x1 = Double_val(_v_x1);
  y1 = Double_val(_v_y1);
  x2 = Double_val(_v_x2);
  y2 = Double_val(_v_y2);
  c_plpath(n, x1, y1, x2, y2);
  return Val_unit;
}

value camlidl_plplot_core_c_plpoin(
	value _v_x,
	value _v_y,
	value _v_code)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  int code; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  code = Int_val(_v_code);
  c_plpoin(n, x, y, code);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plpoin3(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_code)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  double *z; /*in*/
  int code; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  _c5 = Wosize_val(_v_z) / Double_wosize;
  z = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    z[_c6] = Double_field(_v_z, _c6);
  }
  n = _c5;
  code = Int_val(_v_code);
  c_plpoin3(n, x, y, z, code);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plprec(
	value _v_setp,
	value _v_prec)
{
  int setp; /*in*/
  int prec; /*in*/
  setp = Int_val(_v_setp);
  prec = Int_val(_v_prec);
  c_plprec(setp, prec);
  return Val_unit;
}

value camlidl_plplot_core_c_plpsty(
	value _v_patt)
{
  int patt; /*in*/
  patt = Int_val(_v_patt);
  c_plpsty(patt);
  return Val_unit;
}

value camlidl_plplot_core_c_plptex(
	value _v_x,
	value _v_y,
	value _v_dx,
	value _v_dy,
	value _v_just,
	value _v_text)
{
  double x; /*in*/
  double y; /*in*/
  double dx; /*in*/
  double dy; /*in*/
  double just; /*in*/
  char const *text; /*in*/
  x = Double_val(_v_x);
  y = Double_val(_v_y);
  dx = Double_val(_v_dx);
  dy = Double_val(_v_dy);
  just = Double_val(_v_just);
  text = String_val(_v_text);
  c_plptex(x, y, dx, dy, just, text);
  return Val_unit;
}

value camlidl_plplot_core_c_plptex_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plptex(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_plptex3(
	value _v_wx,
	value _v_wy,
	value _v_wz,
	value _v_dx,
	value _v_dy,
	value _v_dz,
	value _v_sx,
	value _v_sy,
	value _v_sz,
	value _v_just,
	value _v_text)
{
  double wx; /*in*/
  double wy; /*in*/
  double wz; /*in*/
  double dx; /*in*/
  double dy; /*in*/
  double dz; /*in*/
  double sx; /*in*/
  double sy; /*in*/
  double sz; /*in*/
  double just; /*in*/
  char const *text; /*in*/
  wx = Double_val(_v_wx);
  wy = Double_val(_v_wy);
  wz = Double_val(_v_wz);
  dx = Double_val(_v_dx);
  dy = Double_val(_v_dy);
  dz = Double_val(_v_dz);
  sx = Double_val(_v_sx);
  sy = Double_val(_v_sy);
  sz = Double_val(_v_sz);
  just = Double_val(_v_just);
  text = String_val(_v_text);
  c_plptex3(wx, wy, wz, dx, dy, dz, sx, sy, sz, just, text);
  return Val_unit;
}

value camlidl_plplot_core_c_plptex3_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plptex3(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}

value camlidl_plplot_core_c_plrandd(value _unit)
{
  double _res;
  value _vres;

  _res = c_plrandd();
  _vres = copy_double(_res);
  return _vres;
}

value camlidl_plplot_core_c_plreplot(value _unit)
{
  c_plreplot();
  return Val_unit;
}

value camlidl_plplot_core_c_plrgbhls(
	value _v_r,
	value _v_g,
	value _v_b)
{
  double r; /*in*/
  double g; /*in*/
  double b; /*in*/
  double *p_h; /*out*/
  double *p_l; /*out*/
  double *p_s; /*out*/
  double _c1;
  double _c2;
  double _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  r = Double_val(_v_r);
  g = Double_val(_v_g);
  b = Double_val(_v_b);
  p_h = &_c1;
  p_l = &_c2;
  p_s = &_c3;
  c_plrgbhls(r, g, b, p_h, p_l, p_s);
  Begin_roots_block(_vres, 3)
    _vres[0] = copy_double(*p_h);
    _vres[1] = copy_double(*p_l);
    _vres[2] = copy_double(*p_s);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_c_plschr(
	value _v_def,
	value _v_scale)
{
  double def; /*in*/
  double scale; /*in*/
  def = Double_val(_v_def);
  scale = Double_val(_v_scale);
  c_plschr(def, scale);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap0(
	value _v_r,
	value _v_g,
	value _v_b)
{
  int *r; /*in*/
  int *g; /*in*/
  int *b; /*in*/
  int ncol0; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_r);
  r = camlidl_malloc(_c1 * sizeof(int ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_r, _c2);
    r[_c2] = Int_val(_v3);
  }
  ncol0 = _c1;
  _c4 = Wosize_val(_v_g);
  g = camlidl_malloc(_c4 * sizeof(int ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_g, _c5);
    g[_c5] = Int_val(_v6);
  }
  ncol0 = _c4;
  _c7 = Wosize_val(_v_b);
  b = camlidl_malloc(_c7 * sizeof(int ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_b, _c8);
    b[_c8] = Int_val(_v9);
  }
  ncol0 = _c7;
  c_plscmap0(r, g, b, ncol0);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap0a(
	value _v_r,
	value _v_g,
	value _v_b,
	value _v_a)
{
  int *r; /*in*/
  int *g; /*in*/
  int *b; /*in*/
  double *a; /*in*/
  int ncol0; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  mlsize_t _c10;
  mlsize_t _c11;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_r);
  r = camlidl_malloc(_c1 * sizeof(int ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_r, _c2);
    r[_c2] = Int_val(_v3);
  }
  ncol0 = _c1;
  _c4 = Wosize_val(_v_g);
  g = camlidl_malloc(_c4 * sizeof(int ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_g, _c5);
    g[_c5] = Int_val(_v6);
  }
  ncol0 = _c4;
  _c7 = Wosize_val(_v_b);
  b = camlidl_malloc(_c7 * sizeof(int ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_b, _c8);
    b[_c8] = Int_val(_v9);
  }
  ncol0 = _c7;
  _c10 = Wosize_val(_v_a) / Double_wosize;
  a = camlidl_malloc(_c10 * sizeof(double ), _ctx);
  for (_c11 = 0; _c11 < _c10; _c11++) {
    a[_c11] = Double_field(_v_a, _c11);
  }
  ncol0 = _c10;
  c_plscmap0a(r, g, b, a, ncol0);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap0n(
	value _v_ncol0)
{
  int ncol0; /*in*/
  ncol0 = Int_val(_v_ncol0);
  c_plscmap0n(ncol0);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap1(
	value _v_r,
	value _v_g,
	value _v_b)
{
  int *r; /*in*/
  int *g; /*in*/
  int *b; /*in*/
  int ncol1; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_r);
  r = camlidl_malloc(_c1 * sizeof(int ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_r, _c2);
    r[_c2] = Int_val(_v3);
  }
  ncol1 = _c1;
  _c4 = Wosize_val(_v_g);
  g = camlidl_malloc(_c4 * sizeof(int ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_g, _c5);
    g[_c5] = Int_val(_v6);
  }
  ncol1 = _c4;
  _c7 = Wosize_val(_v_b);
  b = camlidl_malloc(_c7 * sizeof(int ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_b, _c8);
    b[_c8] = Int_val(_v9);
  }
  ncol1 = _c7;
  c_plscmap1(r, g, b, ncol1);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap1a(
	value _v_r,
	value _v_g,
	value _v_b,
	value _v_a)
{
  int *r; /*in*/
  int *g; /*in*/
  int *b; /*in*/
  double *a; /*in*/
  int ncol1; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  mlsize_t _c10;
  mlsize_t _c11;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_r);
  r = camlidl_malloc(_c1 * sizeof(int ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_r, _c2);
    r[_c2] = Int_val(_v3);
  }
  ncol1 = _c1;
  _c4 = Wosize_val(_v_g);
  g = camlidl_malloc(_c4 * sizeof(int ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_g, _c5);
    g[_c5] = Int_val(_v6);
  }
  ncol1 = _c4;
  _c7 = Wosize_val(_v_b);
  b = camlidl_malloc(_c7 * sizeof(int ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_b, _c8);
    b[_c8] = Int_val(_v9);
  }
  ncol1 = _c7;
  _c10 = Wosize_val(_v_a) / Double_wosize;
  a = camlidl_malloc(_c10 * sizeof(double ), _ctx);
  for (_c11 = 0; _c11 < _c10; _c11++) {
    a[_c11] = Double_field(_v_a, _c11);
  }
  ncol1 = _c10;
  c_plscmap1a(r, g, b, a, ncol1);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap1l(
	value _v_itype,
	value _v_intensity,
	value _v_coord1,
	value _v_coord2,
	value _v_coord3,
	value _v_alt_hue_path)
{
  int itype; /*in*/
  int npts; /*in*/
  double *intensity; /*in*/
  double *coord1; /*in*/
  double *coord2; /*in*/
  double *coord3; /*in*/
  int *alt_hue_path; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  mlsize_t _c10;
  mlsize_t _c11;
  value _v12;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  itype = Int_val(_v_itype);
  _c1 = Wosize_val(_v_intensity) / Double_wosize;
  intensity = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    intensity[_c2] = Double_field(_v_intensity, _c2);
  }
  npts = _c1;
  _c3 = Wosize_val(_v_coord1) / Double_wosize;
  coord1 = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    coord1[_c4] = Double_field(_v_coord1, _c4);
  }
  npts = _c3;
  _c5 = Wosize_val(_v_coord2) / Double_wosize;
  coord2 = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    coord2[_c6] = Double_field(_v_coord2, _c6);
  }
  npts = _c5;
  _c7 = Wosize_val(_v_coord3) / Double_wosize;
  coord3 = camlidl_malloc(_c7 * sizeof(double ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    coord3[_c8] = Double_field(_v_coord3, _c8);
  }
  npts = _c7;
  if (_v_alt_hue_path == Val_int(0)) {
    alt_hue_path = NULL;
  } else {
    _v9 = Field(_v_alt_hue_path, 0);
    _c10 = Wosize_val(_v9);
    alt_hue_path = camlidl_malloc(_c10 * sizeof(int ), _ctx);
    for (_c11 = 0; _c11 < _c10; _c11++) {
      _v12 = Field(_v9, _c11);
      alt_hue_path[_c11] = Int_val(_v12);
    }
    npts = _c10;
  }
  c_plscmap1l(itype, npts, intensity, coord1, coord2, coord3, alt_hue_path);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap1l_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plscmap1l(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_plscmap1la(
	value _v_itype,
	value _v_intensity,
	value _v_coord1,
	value _v_coord2,
	value _v_coord3,
	value _v_a,
	value _v_alt_hue_path)
{
  int itype; /*in*/
  int npts; /*in*/
  double *intensity; /*in*/
  double *coord1; /*in*/
  double *coord2; /*in*/
  double *coord3; /*in*/
  double *a; /*in*/
  int *alt_hue_path; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  mlsize_t _c7;
  mlsize_t _c8;
  mlsize_t _c9;
  mlsize_t _c10;
  value _v11;
  mlsize_t _c12;
  mlsize_t _c13;
  value _v14;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  itype = Int_val(_v_itype);
  _c1 = Wosize_val(_v_intensity) / Double_wosize;
  intensity = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    intensity[_c2] = Double_field(_v_intensity, _c2);
  }
  npts = _c1;
  _c3 = Wosize_val(_v_coord1) / Double_wosize;
  coord1 = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    coord1[_c4] = Double_field(_v_coord1, _c4);
  }
  npts = _c3;
  _c5 = Wosize_val(_v_coord2) / Double_wosize;
  coord2 = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    coord2[_c6] = Double_field(_v_coord2, _c6);
  }
  npts = _c5;
  _c7 = Wosize_val(_v_coord3) / Double_wosize;
  coord3 = camlidl_malloc(_c7 * sizeof(double ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    coord3[_c8] = Double_field(_v_coord3, _c8);
  }
  npts = _c7;
  _c9 = Wosize_val(_v_a) / Double_wosize;
  a = camlidl_malloc(_c9 * sizeof(double ), _ctx);
  for (_c10 = 0; _c10 < _c9; _c10++) {
    a[_c10] = Double_field(_v_a, _c10);
  }
  npts = _c9;
  if (_v_alt_hue_path == Val_int(0)) {
    alt_hue_path = NULL;
  } else {
    _v11 = Field(_v_alt_hue_path, 0);
    _c12 = Wosize_val(_v11);
    alt_hue_path = camlidl_malloc(_c12 * sizeof(int ), _ctx);
    for (_c13 = 0; _c13 < _c12; _c13++) {
      _v14 = Field(_v11, _c13);
      alt_hue_path[_c13] = Int_val(_v14);
    }
    npts = _c12;
  }
  c_plscmap1la(itype, npts, intensity, coord1, coord2, coord3, a, alt_hue_path);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap1la_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plscmap1la(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

value camlidl_plplot_core_c_plscmap1n(
	value _v_ncol1)
{
  int ncol1; /*in*/
  ncol1 = Int_val(_v_ncol1);
  c_plscmap1n(ncol1);
  return Val_unit;
}

value camlidl_plplot_core_c_plscmap1_range(
	value _v_min_color,
	value _v_max_color)
{
  double min_color; /*in*/
  double max_color; /*in*/
  min_color = Double_val(_v_min_color);
  max_color = Double_val(_v_max_color);
  c_plscmap1_range(min_color, max_color);
  return Val_unit;
}

value camlidl_plplot_core_c_plscol0(
	value _v_icol0,
	value _v_r,
	value _v_g,
	value _v_b)
{
  int icol0; /*in*/
  int r; /*in*/
  int g; /*in*/
  int b; /*in*/
  icol0 = Int_val(_v_icol0);
  r = Int_val(_v_r);
  g = Int_val(_v_g);
  b = Int_val(_v_b);
  c_plscol0(icol0, r, g, b);
  return Val_unit;
}

value camlidl_plplot_core_c_plscol0a(
	value _v_icol0,
	value _v_r,
	value _v_g,
	value _v_b,
	value _v_a)
{
  int icol0; /*in*/
  int r; /*in*/
  int g; /*in*/
  int b; /*in*/
  double a; /*in*/
  icol0 = Int_val(_v_icol0);
  r = Int_val(_v_r);
  g = Int_val(_v_g);
  b = Int_val(_v_b);
  a = Double_val(_v_a);
  c_plscol0a(icol0, r, g, b, a);
  return Val_unit;
}

value camlidl_plplot_core_c_plscolbg(
	value _v_r,
	value _v_g,
	value _v_b)
{
  int r; /*in*/
  int g; /*in*/
  int b; /*in*/
  r = Int_val(_v_r);
  g = Int_val(_v_g);
  b = Int_val(_v_b);
  c_plscolbg(r, g, b);
  return Val_unit;
}

value camlidl_plplot_core_c_plscolbga(
	value _v_r,
	value _v_g,
	value _v_b,
	value _v_a)
{
  int r; /*in*/
  int g; /*in*/
  int b; /*in*/
  double a; /*in*/
  r = Int_val(_v_r);
  g = Int_val(_v_g);
  b = Int_val(_v_b);
  a = Double_val(_v_a);
  c_plscolbga(r, g, b, a);
  return Val_unit;
}

value camlidl_plplot_core_c_plscolor(
	value _v_color)
{
  int color; /*in*/
  color = Int_val(_v_color);
  c_plscolor(color);
  return Val_unit;
}

value camlidl_plplot_core_c_plscompression(
	value _v_compression)
{
  int compression; /*in*/
  compression = Int_val(_v_compression);
  c_plscompression(compression);
  return Val_unit;
}

value camlidl_plplot_core_c_plsdev(
	value _v_devname)
{
  char const *devname; /*in*/
  devname = String_val(_v_devname);
  c_plsdev(devname);
  return Val_unit;
}

value camlidl_plplot_core_c_plsdidev(
	value _v_mar,
	value _v_aspect,
	value _v_jx,
	value _v_jy)
{
  double mar; /*in*/
  double aspect; /*in*/
  double jx; /*in*/
  double jy; /*in*/
  mar = Double_val(_v_mar);
  aspect = Double_val(_v_aspect);
  jx = Double_val(_v_jx);
  jy = Double_val(_v_jy);
  c_plsdidev(mar, aspect, jx, jy);
  return Val_unit;
}

value camlidl_plplot_core_c_plsdimap(
	value _v_dimxmin,
	value _v_dimxmax,
	value _v_dimymin,
	value _v_dimymax,
	value _v_dimxpmm,
	value _v_dimypmm)
{
  int dimxmin; /*in*/
  int dimxmax; /*in*/
  int dimymin; /*in*/
  int dimymax; /*in*/
  double dimxpmm; /*in*/
  double dimypmm; /*in*/
  dimxmin = Int_val(_v_dimxmin);
  dimxmax = Int_val(_v_dimxmax);
  dimymin = Int_val(_v_dimymin);
  dimymax = Int_val(_v_dimymax);
  dimxpmm = Double_val(_v_dimxpmm);
  dimypmm = Double_val(_v_dimypmm);
  c_plsdimap(dimxmin, dimxmax, dimymin, dimymax, dimxpmm, dimypmm);
  return Val_unit;
}

value camlidl_plplot_core_c_plsdimap_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plsdimap(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_plsdiori(
	value _v_rot)
{
  double rot; /*in*/
  rot = Double_val(_v_rot);
  c_plsdiori(rot);
  return Val_unit;
}

value camlidl_plplot_core_c_plsdiplt(
	value _v_xmin,
	value _v_ymin,
	value _v_xmax,
	value _v_ymax)
{
  double xmin; /*in*/
  double ymin; /*in*/
  double xmax; /*in*/
  double ymax; /*in*/
  xmin = Double_val(_v_xmin);
  ymin = Double_val(_v_ymin);
  xmax = Double_val(_v_xmax);
  ymax = Double_val(_v_ymax);
  c_plsdiplt(xmin, ymin, xmax, ymax);
  return Val_unit;
}

value camlidl_plplot_core_c_plsdiplz(
	value _v_xmin,
	value _v_ymin,
	value _v_xmax,
	value _v_ymax)
{
  double xmin; /*in*/
  double ymin; /*in*/
  double xmax; /*in*/
  double ymax; /*in*/
  xmin = Double_val(_v_xmin);
  ymin = Double_val(_v_ymin);
  xmax = Double_val(_v_xmax);
  ymax = Double_val(_v_ymax);
  c_plsdiplz(xmin, ymin, xmax, ymax);
  return Val_unit;
}

value camlidl_plplot_core_c_plseed(
	value _v_s)
{
  unsigned int s; /*in*/
  s = Int64_val(_v_s);
  c_plseed(s);
  return Val_unit;
}

value camlidl_plplot_core_c_plsesc(
	value _v_esc)
{
  char esc; /*in*/
  esc = Int_val(_v_esc);
  c_plsesc(esc);
  return Val_unit;
}

value camlidl_plplot_core_c_plsfam(
	value _v_fam,
	value _v_num,
	value _v_bmax)
{
  int fam; /*in*/
  int num; /*in*/
  int bmax; /*in*/
  fam = Int_val(_v_fam);
  num = Int_val(_v_num);
  bmax = Int_val(_v_bmax);
  c_plsfam(fam, num, bmax);
  return Val_unit;
}

value camlidl_plplot_core_c_plsfci(
	value _v_fci)
{
  long long fci; /*in*/
  fci = Int64_val(_v_fci);
  c_plsfci(fci);
  return Val_unit;
}

value camlidl_plplot_core_c_plsfnam(
	value _v_fnam)
{
  char const *fnam; /*in*/
  fnam = String_val(_v_fnam);
  c_plsfnam(fnam);
  return Val_unit;
}

value camlidl_plplot_core_c_plsfont(
	value _v_family,
	value _v_style,
	value _v_weight)
{
  int family; /*in*/
  int style; /*in*/
  int weight; /*in*/
  family = camlidl_ml2c_plplot_core_enum_plplot_fci_family_enum(_v_family);
  style = camlidl_ml2c_plplot_core_enum_plplot_fci_style_enum(_v_style);
  weight = camlidl_ml2c_plplot_core_enum_plplot_fci_weight_enum(_v_weight);
  c_plsfont(family, style, weight);
  return Val_unit;
}

value camlidl_plplot_core_c_plsmaj(
	value _v_def,
	value _v_scale)
{
  double def; /*in*/
  double scale; /*in*/
  def = Double_val(_v_def);
  scale = Double_val(_v_scale);
  c_plsmaj(def, scale);
  return Val_unit;
}

value camlidl_plplot_core_c_plsmin(
	value _v_def,
	value _v_scale)
{
  double def; /*in*/
  double scale; /*in*/
  def = Double_val(_v_def);
  scale = Double_val(_v_scale);
  c_plsmin(def, scale);
  return Val_unit;
}

value camlidl_plplot_core_c_plsdrawmode(
	value _v_mode)
{
  int mode; /*in*/
  mode = camlidl_ml2c_plplot_core_enum_plplot_draw_mode_enum(_v_mode);
  c_plsdrawmode(mode);
  return Val_unit;
}

value camlidl_plplot_core_c_plsori(
	value _v_ori)
{
  int ori; /*in*/
  ori = Int_val(_v_ori);
  c_plsori(ori);
  return Val_unit;
}

value camlidl_plplot_core_c_plspage(
	value _v_xp,
	value _v_yp,
	value _v_xleng,
	value _v_yleng,
	value _v_xoff,
	value _v_yoff)
{
  double xp; /*in*/
  double yp; /*in*/
  int xleng; /*in*/
  int yleng; /*in*/
  int xoff; /*in*/
  int yoff; /*in*/
  xp = Double_val(_v_xp);
  yp = Double_val(_v_yp);
  xleng = Int_val(_v_xleng);
  yleng = Int_val(_v_yleng);
  xoff = Int_val(_v_xoff);
  yoff = Int_val(_v_yoff);
  c_plspage(xp, yp, xleng, yleng, xoff, yoff);
  return Val_unit;
}

value camlidl_plplot_core_c_plspage_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plspage(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_c_plspal0(
	value _v_filename)
{
  char const *filename; /*in*/
  filename = String_val(_v_filename);
  c_plspal0(filename);
  return Val_unit;
}

value camlidl_plplot_core_c_plspal1(
	value _v_filename,
	value _v_interpolate)
{
  char const *filename; /*in*/
  int interpolate; /*in*/
  filename = String_val(_v_filename);
  interpolate = Int_val(_v_interpolate);
  c_plspal1(filename, interpolate);
  return Val_unit;
}

value camlidl_plplot_core_c_plspause(
	value _v_pause)
{
  int pause; /*in*/
  pause = Int_val(_v_pause);
  c_plspause(pause);
  return Val_unit;
}

value camlidl_plplot_core_c_plsstrm(
	value _v_strm)
{
  int strm; /*in*/
  strm = Int_val(_v_strm);
  c_plsstrm(strm);
  return Val_unit;
}

value camlidl_plplot_core_c_plssub(
	value _v_nx,
	value _v_ny)
{
  int nx; /*in*/
  int ny; /*in*/
  nx = Int_val(_v_nx);
  ny = Int_val(_v_ny);
  c_plssub(nx, ny);
  return Val_unit;
}

value camlidl_plplot_core_c_plssym(
	value _v_def,
	value _v_scale)
{
  double def; /*in*/
  double scale; /*in*/
  def = Double_val(_v_def);
  scale = Double_val(_v_scale);
  c_plssym(def, scale);
  return Val_unit;
}

value camlidl_plplot_core_c_plstar(
	value _v_nx,
	value _v_ny)
{
  int nx; /*in*/
  int ny; /*in*/
  nx = Int_val(_v_nx);
  ny = Int_val(_v_ny);
  c_plstar(nx, ny);
  return Val_unit;
}

value camlidl_plplot_core_c_plstart(
	value _v_devname,
	value _v_nx,
	value _v_ny)
{
  char const *devname; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  devname = String_val(_v_devname);
  nx = Int_val(_v_nx);
  ny = Int_val(_v_ny);
  c_plstart(devname, nx, ny);
  return Val_unit;
}

value camlidl_plplot_core_c_plstring(
	value _v_x,
	value _v_y,
	value _v_string)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  char const *string; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  string = String_val(_v_string);
  c_plstring(n, x, y, string);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plstring3(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_string)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  double *z; /*in*/
  char const *string; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  _c5 = Wosize_val(_v_z) / Double_wosize;
  z = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    z[_c6] = Double_field(_v_z, _c6);
  }
  n = _c5;
  string = String_val(_v_string);
  c_plstring3(n, x, y, z, string);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plstripa(
	value _v_id,
	value _v_pen,
	value _v_x,
	value _v_y)
{
  int id; /*in*/
  int pen; /*in*/
  double x; /*in*/
  double y; /*in*/
  id = Int_val(_v_id);
  pen = Int_val(_v_pen);
  x = Double_val(_v_x);
  y = Double_val(_v_y);
  c_plstripa(id, pen, x, y);
  return Val_unit;
}

value camlidl_plplot_core_c_plstripd(
	value _v_id)
{
  int id; /*in*/
  id = Int_val(_v_id);
  c_plstripd(id);
  return Val_unit;
}

value camlidl_plplot_core_c_plimage(
	value _v_idata,
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax,
	value _v_zmin,
	value _v_zmax,
	value _v_Dxmin,
	value _v_Dxmax,
	value _v_Dymin,
	value _v_Dymax)
{
  double **idata; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  double zmin; /*in*/
  double zmax; /*in*/
  double Dxmin; /*in*/
  double Dxmax; /*in*/
  double Dymin; /*in*/
  double Dymax; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_idata);
  idata = camlidl_malloc(_c1 * sizeof(double *), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_idata, _c2);
    _c4 = Wosize_val(_v3) / Double_wosize;
    idata[_c2] = camlidl_malloc(_c4 * sizeof(double ), _ctx);
    for (_c5 = 0; _c5 < _c4; _c5++) {
      idata[_c2][_c5] = Double_field(_v3, _c5);
    }
    ny = _c4;
  }
  nx = _c1;
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  zmin = Double_val(_v_zmin);
  zmax = Double_val(_v_zmax);
  Dxmin = Double_val(_v_Dxmin);
  Dxmax = Double_val(_v_Dxmax);
  Dymin = Double_val(_v_Dymin);
  Dymax = Double_val(_v_Dymax);
  c_plimage(idata, nx, ny, xmin, xmax, ymin, ymax, zmin, zmax, Dxmin, Dxmax, Dymin, Dymax);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plimage_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plimage(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}

value camlidl_plplot_core_c_plstyl(
	value _v_mark,
	value _v_space)
{
  int nms; /*in*/
  int *mark; /*in*/
  int *space; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_mark);
  mark = camlidl_malloc(_c1 * sizeof(int ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_mark, _c2);
    mark[_c2] = Int_val(_v3);
  }
  nms = _c1;
  _c4 = Wosize_val(_v_space);
  space = camlidl_malloc(_c4 * sizeof(int ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_space, _c5);
    space[_c5] = Int_val(_v6);
  }
  nms = _c4;
  c_plstyl(nms, mark, space);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plsurf3d(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_opt,
	value _v_clevel)
{
  double *x; /*in*/
  double *y; /*in*/
  double **z; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  plplot3d_style opt; /*in*/
  double *clevel; /*in*/
  int nlevel; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  value _v7;
  mlsize_t _c8;
  mlsize_t _c9;
  mlsize_t _c10;
  mlsize_t _c11;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  nx = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  ny = _c3;
  _c5 = Wosize_val(_v_z);
  z = camlidl_malloc(_c5 * sizeof(double *), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    _v7 = Field(_v_z, _c6);
    _c8 = Wosize_val(_v7) / Double_wosize;
    z[_c6] = camlidl_malloc(_c8 * sizeof(double ), _ctx);
    for (_c9 = 0; _c9 < _c8; _c9++) {
      z[_c6][_c9] = Double_field(_v7, _c9);
    }
    ny = _c8;
  }
  nx = _c5;
  camlidl_ml2c_plplot_core_plplot3d_style(_v_opt, &opt, _ctx);
  _c10 = Wosize_val(_v_clevel) / Double_wosize;
  clevel = camlidl_malloc(_c10 * sizeof(double ), _ctx);
  for (_c11 = 0; _c11 < _c10; _c11++) {
    clevel[_c11] = Double_field(_v_clevel, _c11);
  }
  nlevel = _c10;
  c_plsurf3d(x, y, z, nx, ny, opt, clevel, nlevel);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plsvect(
	value _v_arrowx,
	value _v_arrowy,
	value _v_fill)
{
  double *arrowx; /*in*/
  double *arrowy; /*in*/
  int npts; /*in*/
  int fill; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_arrowx) / Double_wosize;
  arrowx = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    arrowx[_c2] = Double_field(_v_arrowx, _c2);
  }
  npts = _c1;
  _c3 = Wosize_val(_v_arrowy) / Double_wosize;
  arrowy = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    arrowy[_c4] = Double_field(_v_arrowy, _c4);
  }
  npts = _c3;
  fill = Int_val(_v_fill);
  c_plsvect(arrowx, arrowy, npts, fill);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plsvpa(
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax)
{
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  c_plsvpa(xmin, xmax, ymin, ymax);
  return Val_unit;
}

value camlidl_plplot_core_c_plsxax(
	value _v_digmax,
	value _v_digits)
{
  int digmax; /*in*/
  int digits; /*in*/
  digmax = Int_val(_v_digmax);
  digits = Int_val(_v_digits);
  c_plsxax(digmax, digits);
  return Val_unit;
}

value camlidl_plplot_core_plsxwin(
	value _v_window_id)
{
  int window_id; /*in*/
  window_id = Int_val(_v_window_id);
  plsxwin(window_id);
  return Val_unit;
}

value camlidl_plplot_core_c_plsyax(
	value _v_digmax,
	value _v_digits)
{
  int digmax; /*in*/
  int digits; /*in*/
  digmax = Int_val(_v_digmax);
  digits = Int_val(_v_digits);
  c_plsyax(digmax, digits);
  return Val_unit;
}

value camlidl_plplot_core_c_plsym(
	value _v_x,
	value _v_y,
	value _v_code)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  int code; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  code = Int_val(_v_code);
  c_plsym(n, x, y, code);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_c_plszax(
	value _v_digmax,
	value _v_digits)
{
  int digmax; /*in*/
  int digits; /*in*/
  digmax = Int_val(_v_digmax);
  digits = Int_val(_v_digits);
  c_plszax(digmax, digits);
  return Val_unit;
}

value camlidl_plplot_core_c_pltext(value _unit)
{
  c_pltext();
  return Val_unit;
}

value camlidl_plplot_core_c_pltimefmt(
	value _v_fmt)
{
  char const *fmt; /*in*/
  fmt = String_val(_v_fmt);
  c_pltimefmt(fmt);
  return Val_unit;
}

value camlidl_plplot_core_c_plvasp(
	value _v_aspect)
{
  double aspect; /*in*/
  aspect = Double_val(_v_aspect);
  c_plvasp(aspect);
  return Val_unit;
}

value camlidl_plplot_core_c_plvpas(
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax,
	value _v_aspect)
{
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  double aspect; /*in*/
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  aspect = Double_val(_v_aspect);
  c_plvpas(xmin, xmax, ymin, ymax, aspect);
  return Val_unit;
}

value camlidl_plplot_core_c_plvpor(
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax)
{
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  c_plvpor(xmin, xmax, ymin, ymax);
  return Val_unit;
}

value camlidl_plplot_core_c_plvsta(value _unit)
{
  c_plvsta();
  return Val_unit;
}

value camlidl_plplot_core_c_plw3d(
	value _v_basex,
	value _v_basey,
	value _v_height,
	value _v_xmin0,
	value _v_xmax0,
	value _v_ymin0,
	value _v_ymax0,
	value _v_zmin0,
	value _v_zmax0,
	value _v_alt,
	value _v_az)
{
  double basex; /*in*/
  double basey; /*in*/
  double height; /*in*/
  double xmin0; /*in*/
  double xmax0; /*in*/
  double ymin0; /*in*/
  double ymax0; /*in*/
  double zmin0; /*in*/
  double zmax0; /*in*/
  double alt; /*in*/
  double az; /*in*/
  basex = Double_val(_v_basex);
  basey = Double_val(_v_basey);
  height = Double_val(_v_height);
  xmin0 = Double_val(_v_xmin0);
  xmax0 = Double_val(_v_xmax0);
  ymin0 = Double_val(_v_ymin0);
  ymax0 = Double_val(_v_ymax0);
  zmin0 = Double_val(_v_zmin0);
  zmax0 = Double_val(_v_zmax0);
  alt = Double_val(_v_alt);
  az = Double_val(_v_az);
  c_plw3d(basex, basey, height, xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, alt, az);
  return Val_unit;
}

value camlidl_plplot_core_c_plw3d_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_c_plw3d(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}

value camlidl_plplot_core_c_plwidth(
	value _v_width)
{
  double width; /*in*/
  width = Double_val(_v_width);
  c_plwidth(width);
  return Val_unit;
}

value camlidl_plplot_core_c_plwind(
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax)
{
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  c_plwind(xmin, xmax, ymin, ymax);
  return Val_unit;
}

value camlidl_plplot_core_c_plxormod(
	value _v_mode)
{
  int mode; /*in*/
  int *status; /*out*/
  int _c1;
  value _vres;

  mode = Int_val(_v_mode);
  status = &_c1;
  c_plxormod(mode, status);
  _vres = Val_int(*status);
  return _vres;
}

value camlidl_plplot_core_c_plsetopt(
	value _v_opt,
	value _v_optarg)
{
  char const *opt; /*in*/
  char const *optarg; /*in*/
  nonzero_error_int _res;
  opt = String_val(_v_opt);
  optarg = String_val(_v_optarg);
  _res = c_plsetopt(opt, optarg);
  plplot_check_nonzero_result(_res);
  return Val_unit;
}

value camlidl_plplot_core_plMinMax2dGrid(
	value _v_f)
{
  double **f; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  double *fmax; /*out*/
  double *fmin; /*out*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  double _c6;
  double _c7;
  value _vresult;
  value _vres[2] = { 0, 0, };

  _c1 = Wosize_val(_v_f);
  f = camlidl_malloc(_c1 * sizeof(double *), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_f, _c2);
    _c4 = Wosize_val(_v3) / Double_wosize;
    f[_c2] = camlidl_malloc(_c4 * sizeof(double ), _ctx);
    for (_c5 = 0; _c5 < _c4; _c5++) {
      f[_c2][_c5] = Double_field(_v3, _c5);
    }
    ny = _c4;
  }
  nx = _c1;
  fmax = &_c6;
  fmin = &_c7;
  plMinMax2dGrid(f, nx, ny, fmax, fmin);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_double(*fmax);
    _vres[1] = copy_double(*fmin);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_plplot_core_ml_plcont(
	value _v_f,
	value _v_kx,
	value _v_lx,
	value _v_ky,
	value _v_ly,
	value _v_clevel)
{
  double **f; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  int kx; /*in*/
  int lx; /*in*/
  int ky; /*in*/
  int ly; /*in*/
  double *clevel; /*in*/
  int nlevel; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  mlsize_t _c7;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_f);
  f = camlidl_malloc(_c1 * sizeof(double *), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_f, _c2);
    _c4 = Wosize_val(_v3) / Double_wosize;
    f[_c2] = camlidl_malloc(_c4 * sizeof(double ), _ctx);
    for (_c5 = 0; _c5 < _c4; _c5++) {
      f[_c2][_c5] = Double_field(_v3, _c5);
    }
    ny = _c4;
  }
  nx = _c1;
  kx = Int_val(_v_kx);
  lx = Int_val(_v_lx);
  ky = Int_val(_v_ky);
  ly = Int_val(_v_ly);
  _c6 = Wosize_val(_v_clevel) / Double_wosize;
  clevel = camlidl_malloc(_c6 * sizeof(double ), _ctx);
  for (_c7 = 0; _c7 < _c6; _c7++) {
    clevel[_c7] = Double_field(_v_clevel, _c7);
  }
  nlevel = _c6;
  ml_plcont(f, nx, ny, kx, lx, ky, ly, clevel, nlevel);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_ml_plcont_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_ml_plcont(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_ml_plshade(
	value _v_a,
	value _v_left,
	value _v_right,
	value _v_bottom,
	value _v_top,
	value _v_shade_min,
	value _v_shade_max,
	value _v_sh_cmap,
	value _v_sh_color,
	value _v_sh_width,
	value _v_min_color,
	value _v_min_width,
	value _v_max_color,
	value _v_max_width,
	value _v_rectangular)
{
  double **a; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  double left; /*in*/
  double right; /*in*/
  double bottom; /*in*/
  double top; /*in*/
  double shade_min; /*in*/
  double shade_max; /*in*/
  int sh_cmap; /*in*/
  double sh_color; /*in*/
  double sh_width; /*in*/
  int min_color; /*in*/
  double min_width; /*in*/
  int max_color; /*in*/
  double max_width; /*in*/
  int rectangular; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_a);
  a = camlidl_malloc(_c1 * sizeof(double *), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_a, _c2);
    _c4 = Wosize_val(_v3) / Double_wosize;
    a[_c2] = camlidl_malloc(_c4 * sizeof(double ), _ctx);
    for (_c5 = 0; _c5 < _c4; _c5++) {
      a[_c2][_c5] = Double_field(_v3, _c5);
    }
    ny = _c4;
  }
  nx = _c1;
  left = Double_val(_v_left);
  right = Double_val(_v_right);
  bottom = Double_val(_v_bottom);
  top = Double_val(_v_top);
  shade_min = Double_val(_v_shade_min);
  shade_max = Double_val(_v_shade_max);
  sh_cmap = Int_val(_v_sh_cmap);
  sh_color = Double_val(_v_sh_color);
  sh_width = Double_val(_v_sh_width);
  min_color = Int_val(_v_min_color);
  min_width = Double_val(_v_min_width);
  max_color = Int_val(_v_max_color);
  max_width = Double_val(_v_max_width);
  rectangular = Int_val(_v_rectangular);
  ml_plshade(a, nx, ny, left, right, bottom, top, shade_min, shade_max, sh_cmap, sh_color, sh_width, min_color, min_width, max_color, max_width, rectangular);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_ml_plshade_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_ml_plshade(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10], argv[11], argv[12], argv[13], argv[14]);
}

value camlidl_plplot_core_ml_plshades(
	value _v_a,
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax,
	value _v_clevel,
	value _v_fill_width,
	value _v_cont_color,
	value _v_cont_width,
	value _v_rectangular)
{
  double **a; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  double *clevel; /*in*/
  int nlevel; /*in*/
  double fill_width; /*in*/
  int cont_color; /*in*/
  double cont_width; /*in*/
  int rectangular; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  mlsize_t _c7;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_a);
  a = camlidl_malloc(_c1 * sizeof(double *), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_a, _c2);
    _c4 = Wosize_val(_v3) / Double_wosize;
    a[_c2] = camlidl_malloc(_c4 * sizeof(double ), _ctx);
    for (_c5 = 0; _c5 < _c4; _c5++) {
      a[_c2][_c5] = Double_field(_v3, _c5);
    }
    ny = _c4;
  }
  nx = _c1;
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  _c6 = Wosize_val(_v_clevel) / Double_wosize;
  clevel = camlidl_malloc(_c6 * sizeof(double ), _ctx);
  for (_c7 = 0; _c7 < _c6; _c7++) {
    clevel[_c7] = Double_field(_v_clevel, _c7);
  }
  nlevel = _c6;
  fill_width = Double_val(_v_fill_width);
  cont_color = Int_val(_v_cont_color);
  cont_width = Double_val(_v_cont_width);
  rectangular = Int_val(_v_rectangular);
  ml_plshades(a, nx, ny, xmin, xmax, ymin, ymax, clevel, nlevel, fill_width, cont_color, cont_width, rectangular);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_ml_plshades_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_ml_plshades(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9]);
}

value camlidl_plplot_core_ml_plimagefr(
	value _v_idata,
	value _v_xmin,
	value _v_xmax,
	value _v_ymin,
	value _v_ymax,
	value _v_zmin,
	value _v_zmax,
	value _v_valuemin,
	value _v_valuemax)
{
  double **idata; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  double xmin; /*in*/
  double xmax; /*in*/
  double ymin; /*in*/
  double ymax; /*in*/
  double zmin; /*in*/
  double zmax; /*in*/
  double valuemin; /*in*/
  double valuemax; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_idata);
  idata = camlidl_malloc(_c1 * sizeof(double *), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_idata, _c2);
    _c4 = Wosize_val(_v3) / Double_wosize;
    idata[_c2] = camlidl_malloc(_c4 * sizeof(double ), _ctx);
    for (_c5 = 0; _c5 < _c4; _c5++) {
      idata[_c2][_c5] = Double_field(_v3, _c5);
    }
    ny = _c4;
  }
  nx = _c1;
  xmin = Double_val(_v_xmin);
  xmax = Double_val(_v_xmax);
  ymin = Double_val(_v_ymin);
  ymax = Double_val(_v_ymax);
  zmin = Double_val(_v_zmin);
  zmax = Double_val(_v_zmax);
  valuemin = Double_val(_v_valuemin);
  valuemax = Double_val(_v_valuemax);
  ml_plimagefr(idata, nx, ny, xmin, xmax, ymin, ymax, zmin, zmax, valuemin, valuemax);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_ml_plimagefr_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_ml_plimagefr(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
}

value camlidl_plplot_core_ml_plvect(
	value _v_u,
	value _v_v,
	value _v_scale)
{
  double **u; /*in*/
  double **v; /*in*/
  int nx; /*in*/
  int ny; /*in*/
  double scale; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  mlsize_t _c7;
  value _v8;
  mlsize_t _c9;
  mlsize_t _c10;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_u);
  u = camlidl_malloc(_c1 * sizeof(double *), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_u, _c2);
    _c4 = Wosize_val(_v3) / Double_wosize;
    u[_c2] = camlidl_malloc(_c4 * sizeof(double ), _ctx);
    for (_c5 = 0; _c5 < _c4; _c5++) {
      u[_c2][_c5] = Double_field(_v3, _c5);
    }
    ny = _c4;
  }
  nx = _c1;
  _c6 = Wosize_val(_v_v);
  v = camlidl_malloc(_c6 * sizeof(double *), _ctx);
  for (_c7 = 0; _c7 < _c6; _c7++) {
    _v8 = Field(_v_v, _c7);
    _c9 = Wosize_val(_v8) / Double_wosize;
    v[_c7] = camlidl_malloc(_c9 * sizeof(double ), _ctx);
    for (_c10 = 0; _c10 < _c9; _c10++) {
      v[_c7][_c10] = Double_field(_v8, _c10);
    }
    ny = _c9;
  }
  nx = _c6;
  scale = Double_val(_v_scale);
  ml_plvect(u, v, nx, ny, scale);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_ml_plmap(
	value _v_type,
	value _v_minlong,
	value _v_maxlong,
	value _v_minlat,
	value _v_maxlat)
{
  char const *type; /*in*/
  double minlong; /*in*/
  double maxlong; /*in*/
  double minlat; /*in*/
  double maxlat; /*in*/
  type = String_val(_v_type);
  minlong = Double_val(_v_minlong);
  maxlong = Double_val(_v_maxlong);
  minlat = Double_val(_v_minlat);
  maxlat = Double_val(_v_maxlat);
  ml_plmap(type, minlong, maxlong, minlat, maxlat);
  return Val_unit;
}

value camlidl_plplot_core_ml_plmeridians(
	value _v_dlong,
	value _v_dlat,
	value _v_minlong,
	value _v_maxlong,
	value _v_minlat,
	value _v_maxlat)
{
  double dlong; /*in*/
  double dlat; /*in*/
  double minlong; /*in*/
  double maxlong; /*in*/
  double minlat; /*in*/
  double maxlat; /*in*/
  dlong = Double_val(_v_dlong);
  dlat = Double_val(_v_dlat);
  minlong = Double_val(_v_minlong);
  maxlong = Double_val(_v_maxlong);
  minlat = Double_val(_v_minlat);
  maxlat = Double_val(_v_maxlat);
  ml_plmeridians(dlong, dlat, minlong, maxlong, minlat, maxlat);
  return Val_unit;
}

value camlidl_plplot_core_ml_plmeridians_bytecode(value * argv, int argn)
{
  return camlidl_plplot_core_ml_plmeridians(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_plplot_core_ml_plpoly3(
	value _v_x,
	value _v_y,
	value _v_z,
	value _v_draw,
	value _v_ifcc)
{
  int n; /*in*/
  double *x; /*in*/
  double *y; /*in*/
  double *z; /*in*/
  int ndraw; /*in*/
  int *draw; /*in*/
  int ifcc; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  mlsize_t _c3;
  mlsize_t _c4;
  mlsize_t _c5;
  mlsize_t _c6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _c1 = Wosize_val(_v_x) / Double_wosize;
  x = camlidl_malloc(_c1 * sizeof(double ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    x[_c2] = Double_field(_v_x, _c2);
  }
  n = _c1;
  _c3 = Wosize_val(_v_y) / Double_wosize;
  y = camlidl_malloc(_c3 * sizeof(double ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    y[_c4] = Double_field(_v_y, _c4);
  }
  n = _c3;
  _c5 = Wosize_val(_v_z) / Double_wosize;
  z = camlidl_malloc(_c5 * sizeof(double ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    z[_c6] = Double_field(_v_z, _c6);
  }
  n = _c5;
  _c7 = Wosize_val(_v_draw);
  draw = camlidl_malloc(_c7 * sizeof(int ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_draw, _c8);
    draw[_c8] = Int_val(_v9);
  }
  ndraw = _c7;
  ifcc = Int_val(_v_ifcc);
  ml_plpoly3(n, x, y, z, ndraw, draw, ifcc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_plplot_core_ml_pltr0(
	value _v_x,
	value _v_y)
{
  double x; /*in*/
  double y; /*in*/
  double *tx; /*out*/
  double *ty; /*out*/
  double _c1;
  double _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  x = Double_val(_v_x);
  y = Double_val(_v_y);
  tx = &_c1;
  ty = &_c2;
  ml_pltr0(x, y, tx, ty);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_double(*tx);
    _vres[1] = copy_double(*ty);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  return _vresult;
}

value camlidl_plplot_core_ml_plsvect_reset(value _unit)
{
  ml_plsvect_reset();
  return Val_unit;
}

value camlidl_plplot_core_plg_current_col0(value _unit)
{
  int _res;
  value _vres;

  _res = plg_current_col0();
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_plplot_core_plg_current_col1(value _unit)
{
  double _res;
  value _vres;

  _res = plg_current_col1();
  _vres = copy_double(_res);
  return _vres;
}

value camlidl_plplot_core_plgwidth(value _unit)
{
  double _res;
  value _vres;

  _res = plgwidth();
  _vres = copy_double(_res);
  return _vres;
}

value camlidl_plplot_core_plgchrht(value _unit)
{
  double _res;
  value _vres;

  _res = plgchrht();
  _vres = copy_double(_res);
  return _vres;
}

