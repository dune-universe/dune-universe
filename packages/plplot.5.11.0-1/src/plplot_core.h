/* File generated from plplot_core.idl */

#ifndef _CAMLIDL_PLPLOT_CORE_H
#define _CAMLIDL_PLPLOT_CORE_H

#ifdef __cplusplus
#define _CAMLIDL_EXTERN_C extern "C"
#else
#define _CAMLIDL_EXTERN_C extern
#endif

#ifdef _WIN32
#pragma pack(push,8) /* necessary for COM interfaces */
#endif

enum plplot3d_style_enum {
PL_DIFFUSE = 0,
PL_DRAW_LINEX = 1,
PL_DRAW_LINEY = 2,
PL_DRAW_LINEXY = 3,
PL_MAG_COLOR = 4,
PL_BASE_CONT = 8,
PL_TOP_CONT = 16,
PL_SURF_CONT = 32,
PL_DRAW_SIDES = 64,
PL_FACETED = 128,
PL_MESH = 256,
};

typedef int plplot3d_style;

enum plplot_bin_enum {
PL_BIN_DEFAULT = 0,
PL_BIN_CENTRED = 1,
PL_BIN_NOEXPAND = 2,
PL_BIN_NOEMPTY = 4,
};

typedef int plplot_bin_style;

enum plplot_hist_enum {
PL_HIST_DEFAULT = 0,
PL_HIST_NOSCALING = 1,
PL_HIST_IGNORE_OUTLIERS = 2,
PL_HIST_NOEXPAND = 8,
PL_HIST_NOEMPTY = 16,
};

typedef int plplot_hist_style;

enum plplot_run_level_enum {
PL_UNINITIALIZED = 0,
PL_INITIALIZED = 1,
PL_VIEWPORT_DEFINED = 2,
PL_WORLD_COORDINATES_DEFINED = 3,
};

typedef int plplot_run_level;

enum plplot_position_enum {
PL_POSITION_LEFT = 1,
PL_POSITION_RIGHT = 2,
PL_POSITION_TOP = 4,
PL_POSITION_BOTTOM = 8,
PL_POSITION_INSIDE = 16,
PL_POSITION_OUTSIDE = 32,
PL_POSITION_VIEWPORT = 64,
PL_POSITION_SUBPAGE = 128,
};

typedef int plplot_position_opt;

enum plplot_legend_enum {
PL_LEGEND_NONE = 1,
PL_LEGEND_COLOR_BOX = 2,
PL_LEGEND_LINE = 4,
PL_LEGEND_SYMBOL = 8,
PL_LEGEND_TEXT_LEFT = 16,
PL_LEGEND_BACKGROUND = 32,
PL_LEGEND_BOUNDING_BOX = 64,
PL_LEGEND_ROW_MAJOR = 128,
};

typedef int plplot_legend_opt;

enum plplot_colorbar_enum {
PL_COLORBAR_LABEL_LEFT = 1,
PL_COLORBAR_LABEL_RIGHT = 2,
PL_COLORBAR_LABEL_TOP = 4,
PL_COLORBAR_LABEL_BOTTOM = 8,
PL_COLORBAR_IMAGE = 16,
PL_COLORBAR_SHADE = 32,
PL_COLORBAR_GRADIENT = 64,
PL_COLORBAR_CAP_NONE = 128,
PL_COLORBAR_CAP_LOW = 256,
PL_COLORBAR_CAP_HIGH = 512,
PL_COLORBAR_SHADE_LABEL = 1024,
PL_COLORBAR_ORIENT_RIGHT = 2048,
PL_COLORBAR_ORIENT_TOP = 4096,
PL_COLORBAR_ORIENT_LEFT = 8192,
PL_COLORBAR_ORIENT_BOTTOM = 16384,
PL_COLORBAR_BACKGROUND = 32768,
PL_COLORBAR_BOUNDING_BOX = 65536,
};

typedef int plplot_colorbar_opt;

enum plplot_fci_family_enum {
PL_FCI_FAMILY_UNCHANGED = -1,
PL_FCI_SANS = 0,
PL_FCI_SERIF = 1,
PL_FCI_MONO = 2,
PL_FCI_SCRIPT = 3,
PL_FCI_SYMBOL = 4,
};

enum plplot_fci_style_enum {
PL_FCI_STYLE_UNCHANGED = -1,
PL_FCI_UPRIGHT = 0,
PL_FCI_ITALIC = 1,
PL_FCI_OBLIQUE = 2,
};

enum plplot_fci_weight_enum {
PL_FCI_WEIGHT_UNCHANGED = -1,
PL_FCI_MEDIUM = 0,
PL_FCI_BOLD = 1,
};

enum plplot_draw_mode_enum {
PL_DRAWMODE_UNKNOWN = 0,
PL_DRAWMODE_DEFAULT = 1,
PL_DRAWMODE_REPLACE = 2,
PL_DRAWMODE_XOR = 4,
};

typedef int nonzero_error_int;

void plplot_check_nonzero_result(int result);
_CAMLIDL_EXTERN_C void c_pl_setcontlabelformat(/*in*/ int lexp, /*in*/ int sigdig);

_CAMLIDL_EXTERN_C void c_pl_setcontlabelparam(/*in*/ double offset, /*in*/ double size, /*in*/ double spacing, /*in*/ int active);

_CAMLIDL_EXTERN_C void c_pladv(/*in*/ int page);

_CAMLIDL_EXTERN_C void c_plarc(/*in*/ double x, /*in*/ double y, /*in*/ double a, /*in*/ double b, /*in*/ double angle1, /*in*/ double angle2, /*in*/ double rotate, /*in*/ int fill);

_CAMLIDL_EXTERN_C void c_plaxes(/*in*/ double x0, /*in*/ double y0, /*in*/ char const *xopt, /*in*/ double xtick, /*in*/ int nxsub, /*in*/ char const *yopt, /*in*/ double ytick, /*in*/ int nysub);

_CAMLIDL_EXTERN_C void c_plbin(/*in*/ int nbin, /*in*/ double *x, /*in*/ double *y, /*in*/ plplot_bin_style opt);

_CAMLIDL_EXTERN_C void c_plbtime(/*out*/ int *year, /*out*/ int *month, /*out*/ int *day, /*out*/ int *hour, /*out*/ int *min, /*out*/ double *sec, /*in*/ double ctime);

_CAMLIDL_EXTERN_C void c_plbop(void);

_CAMLIDL_EXTERN_C void c_plbox(/*in*/ char const *xopt, /*in*/ double xtick, /*in*/ int nxsub, /*in*/ char const *yopt, /*in*/ double ytick, /*in*/ int nysub);

_CAMLIDL_EXTERN_C void c_plbox3(/*in*/ char const *xopt, /*in*/ char const *xlabel, /*in*/ double xtick, /*in*/ int nsubx, /*in*/ char const *yopt, /*in*/ char const *ylabel, /*in*/ double ytick, /*in*/ int nsuby, /*in*/ char const *zopt, /*in*/ char const *zlabel, /*in*/ double ztick, /*in*/ int nsubz);

_CAMLIDL_EXTERN_C void c_plcalc_world(/*in*/ double rx, /*in*/ double ry, /*out*/ double *wx, /*out*/ double *wy, /*out*/ int *window);

_CAMLIDL_EXTERN_C void c_plclear(void);

_CAMLIDL_EXTERN_C void c_plcol0(/*in*/ int icol0);

_CAMLIDL_EXTERN_C void c_plcol1(/*in*/ double col1);

_CAMLIDL_EXTERN_C void c_plconfigtime(/*in*/ double scale, /*in*/ double offset1, /*in*/ double offset2, /*in*/ int ccontrol, /*in*/ int ifbtime_offset, /*in*/ int year, /*in*/ int month, /*in*/ int day, /*in*/ int hour, /*in*/ int min, /*in*/ double sec);

_CAMLIDL_EXTERN_C void c_plcpstrm(/*in*/ int iplsr, /*in*/ int flags);

_CAMLIDL_EXTERN_C void c_plctime(/*in*/ int year, /*in*/ int month, /*in*/ int day, /*in*/ int hour, /*in*/ int min, /*in*/ double sec, /*out*/ double *ctime);

_CAMLIDL_EXTERN_C void c_plend(void);

_CAMLIDL_EXTERN_C void c_plend1(void);

_CAMLIDL_EXTERN_C void c_plenv(/*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax, /*in*/ int just, /*in*/ int axis);

_CAMLIDL_EXTERN_C void c_plenv0(/*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax, /*in*/ int just, /*in*/ int axis);

_CAMLIDL_EXTERN_C void c_pleop(void);

_CAMLIDL_EXTERN_C void c_plerrx(/*in*/ int n, /*in*/ double *xmin, /*in*/ double *xmax, /*in*/ double *y);

_CAMLIDL_EXTERN_C void c_plerry(/*in*/ int n, /*in*/ double *x, /*in*/ double *ymin, /*in*/ double *ymax);

_CAMLIDL_EXTERN_C void c_plfamadv(void);

_CAMLIDL_EXTERN_C void c_plfill(/*in*/ int n, /*in*/ double *x, /*in*/ double *y);

_CAMLIDL_EXTERN_C void c_plfill3(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ double *z);

_CAMLIDL_EXTERN_C void c_plflush(void);

_CAMLIDL_EXTERN_C void c_plfont(/*in*/ int ifont);

_CAMLIDL_EXTERN_C void c_plfontld(/*in*/ int fnt);

_CAMLIDL_EXTERN_C void c_plgchr(/*out*/ double *p_def, /*out*/ double *p_ht);

_CAMLIDL_EXTERN_C void c_plgcmap1_range(/*out*/ double *min_color, /*out*/ double *max_color);

_CAMLIDL_EXTERN_C void c_plgcol0(/*in*/ int icol0, /*out*/ int *r, /*out*/ int *g, /*out*/ int *b);

_CAMLIDL_EXTERN_C void c_plgcol0a(/*in*/ int icol0, /*out*/ int *r, /*out*/ int *g, /*out*/ int *b, /*out*/ double *a);

_CAMLIDL_EXTERN_C void c_plgcolbg(/*out*/ int *r, /*out*/ int *g, /*out*/ int *b);

_CAMLIDL_EXTERN_C void c_plgcolbga(/*out*/ int *r, /*out*/ int *g, /*out*/ int *b, /*out*/ double *a);

_CAMLIDL_EXTERN_C void c_plgcompression(/*out*/ int *compression);

_CAMLIDL_EXTERN_C void c_plgdev(/*out*/ char *p_dev);

_CAMLIDL_EXTERN_C void c_plgdidev(/*out*/ double *p_mar, /*out*/ double *p_aspect, /*out*/ double *p_jx, /*out*/ double *p_jy);

_CAMLIDL_EXTERN_C void c_plgdiori(/*out*/ double *p_rot);

_CAMLIDL_EXTERN_C void c_plgdiplt(/*out*/ double *p_xmin, /*out*/ double *p_ymin, /*out*/ double *p_xmax, /*out*/ double *p_ymax);

_CAMLIDL_EXTERN_C int c_plgdrawmode(void);

_CAMLIDL_EXTERN_C void c_plgfci(/*out*/ long long *pfci);

_CAMLIDL_EXTERN_C void c_plgfam(/*out*/ int *p_fam, /*out*/ int *p_num, /*out*/ int *p_bmax);

_CAMLIDL_EXTERN_C void c_plgfnam(/*out*/ char *fnam);

_CAMLIDL_EXTERN_C void c_plgfont(/*out*/ int *p_family, /*out*/ int *p_style, /*out*/ int *p_weight);

_CAMLIDL_EXTERN_C void c_plglevel(/*out*/ plplot_run_level *p_level);

_CAMLIDL_EXTERN_C void c_plgpage(/*out*/ double *p_xp, /*out*/ double *p_yp, /*out*/ int *p_xleng, /*out*/ int *p_yleng, /*out*/ int *p_xoff, /*out*/ int *p_yoff);

_CAMLIDL_EXTERN_C void c_plgra(void);

_CAMLIDL_EXTERN_C void c_plgradient(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ double angle);

_CAMLIDL_EXTERN_C void c_plgspa(/*out*/ double *xmin, /*out*/ double *xmax, /*out*/ double *ymin, /*out*/ double *ymax);

_CAMLIDL_EXTERN_C void c_plgstrm(/*out*/ int *p_strm);

_CAMLIDL_EXTERN_C void c_plgver(/*out*/ char *p_ver);

_CAMLIDL_EXTERN_C void c_plgvpd(/*out*/ double *p_xmin, /*out*/ double *p_xmax, /*out*/ double *p_ymin, /*out*/ double *p_ymax);

_CAMLIDL_EXTERN_C void c_plgvpw(/*out*/ double *p_xmin, /*out*/ double *p_xmax, /*out*/ double *p_ymin, /*out*/ double *p_ymax);

_CAMLIDL_EXTERN_C void c_plgxax(/*out*/ int *p_digmax, /*out*/ int *p_digits);

_CAMLIDL_EXTERN_C void c_plgyax(/*out*/ int *p_digmax, /*out*/ int *p_digits);

_CAMLIDL_EXTERN_C void c_plgzax(/*out*/ int *p_digmax, /*out*/ int *p_digits);

_CAMLIDL_EXTERN_C void c_plhist(/*in*/ int n, /*in*/ double *data, /*in*/ double datmin, /*in*/ double datmax, /*in*/ int nbin, /*in*/ plplot_hist_style opt);

_CAMLIDL_EXTERN_C void c_plhlsrgb(/*in*/ double h, /*in*/ double l, /*in*/ double s, /*out*/ double *p_r, /*out*/ double *p_g, /*out*/ double *p_b);

_CAMLIDL_EXTERN_C void c_plinit(void);

_CAMLIDL_EXTERN_C void c_pljoin(/*in*/ double x1, /*in*/ double y1, /*in*/ double x2, /*in*/ double y2);

_CAMLIDL_EXTERN_C void c_pllab(/*in*/ char const *xlabel, /*in*/ char const *ylabel, /*in*/ char const *tlabel);

_CAMLIDL_EXTERN_C void c_pllightsource(/*in*/ double x, /*in*/ double y, /*in*/ double z);

_CAMLIDL_EXTERN_C void c_plline(/*in*/ int n, /*in*/ double *x, /*in*/ double *y);

_CAMLIDL_EXTERN_C void c_plline3(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ double *z);

_CAMLIDL_EXTERN_C void c_pllsty(/*in*/ int lin);

_CAMLIDL_EXTERN_C void c_plmesh(/*in*/ double *x, /*in*/ double *y, /*in*/ double **z, /*in*/ int nx, /*in*/ int ny, /*in*/ plplot3d_style opt);

_CAMLIDL_EXTERN_C void c_plmeshc(/*in*/ double *x, /*in*/ double *y, /*in*/ double **z, /*in*/ int nx, /*in*/ int ny, /*in*/ plplot3d_style opt, /*in*/ double *clevel, /*in*/ int nlevel);

_CAMLIDL_EXTERN_C void c_plmkstrm(/*out*/ int *p_strm);

_CAMLIDL_EXTERN_C void c_plmtex(/*in*/ char const *side, /*in*/ double disp, /*in*/ double pos, /*in*/ double just, /*in*/ char const *text);

_CAMLIDL_EXTERN_C void c_plmtex3(/*in*/ char const *side, /*in*/ double disp, /*in*/ double pos, /*in*/ double just, /*in*/ char const *text);

_CAMLIDL_EXTERN_C void c_plot3d(/*in*/ double *x, /*in*/ double *y, /*in*/ double **z, /*in*/ int nx, /*in*/ int ny, /*in*/ plplot3d_style opt, /*in*/ int side);

_CAMLIDL_EXTERN_C void c_plot3dc(/*in*/ double *x, /*in*/ double *y, /*in*/ double **z, /*in*/ int nx, /*in*/ int ny, /*in*/ plplot3d_style opt, /*in*/ double *clevel, /*in*/ int nlevel);

_CAMLIDL_EXTERN_C void c_plpat(/*in*/ int nlin, /*in*/ int *inc, /*in*/ int *del);

_CAMLIDL_EXTERN_C void c_plpath(/*in*/ int n, /*in*/ double x1, /*in*/ double y1, /*in*/ double x2, /*in*/ double y2);

_CAMLIDL_EXTERN_C void c_plpoin(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ int code);

_CAMLIDL_EXTERN_C void c_plpoin3(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ double *z, /*in*/ int code);

_CAMLIDL_EXTERN_C void c_plprec(/*in*/ int setp, /*in*/ int prec);

_CAMLIDL_EXTERN_C void c_plpsty(/*in*/ int patt);

_CAMLIDL_EXTERN_C void c_plptex(/*in*/ double x, /*in*/ double y, /*in*/ double dx, /*in*/ double dy, /*in*/ double just, /*in*/ char const *text);

_CAMLIDL_EXTERN_C void c_plptex3(/*in*/ double wx, /*in*/ double wy, /*in*/ double wz, /*in*/ double dx, /*in*/ double dy, /*in*/ double dz, /*in*/ double sx, /*in*/ double sy, /*in*/ double sz, /*in*/ double just, /*in*/ char const *text);

_CAMLIDL_EXTERN_C double c_plrandd(void);

_CAMLIDL_EXTERN_C void c_plreplot(void);

_CAMLIDL_EXTERN_C void c_plrgbhls(/*in*/ double r, /*in*/ double g, /*in*/ double b, /*out*/ double *p_h, /*out*/ double *p_l, /*out*/ double *p_s);

_CAMLIDL_EXTERN_C void c_plschr(/*in*/ double def, /*in*/ double scale);

_CAMLIDL_EXTERN_C void c_plscmap0(/*in*/ int *r, /*in*/ int *g, /*in*/ int *b, /*in*/ int ncol0);

_CAMLIDL_EXTERN_C void c_plscmap0a(/*in*/ int *r, /*in*/ int *g, /*in*/ int *b, /*in*/ double *a, /*in*/ int ncol0);

_CAMLIDL_EXTERN_C void c_plscmap0n(/*in*/ int ncol0);

_CAMLIDL_EXTERN_C void c_plscmap1(/*in*/ int *r, /*in*/ int *g, /*in*/ int *b, /*in*/ int ncol1);

_CAMLIDL_EXTERN_C void c_plscmap1a(/*in*/ int *r, /*in*/ int *g, /*in*/ int *b, /*in*/ double *a, /*in*/ int ncol1);

_CAMLIDL_EXTERN_C void c_plscmap1l(/*in*/ int itype, /*in*/ int npts, /*in*/ double *intensity, /*in*/ double *coord1, /*in*/ double *coord2, /*in*/ double *coord3, /*in*/ int *alt_hue_path);

_CAMLIDL_EXTERN_C void c_plscmap1la(/*in*/ int itype, /*in*/ int npts, /*in*/ double *intensity, /*in*/ double *coord1, /*in*/ double *coord2, /*in*/ double *coord3, /*in*/ double *a, /*in*/ int *alt_hue_path);

_CAMLIDL_EXTERN_C void c_plscmap1n(/*in*/ int ncol1);

_CAMLIDL_EXTERN_C void c_plscmap1_range(/*in*/ double min_color, /*in*/ double max_color);

_CAMLIDL_EXTERN_C void c_plscol0(/*in*/ int icol0, /*in*/ int r, /*in*/ int g, /*in*/ int b);

_CAMLIDL_EXTERN_C void c_plscol0a(/*in*/ int icol0, /*in*/ int r, /*in*/ int g, /*in*/ int b, /*in*/ double a);

_CAMLIDL_EXTERN_C void c_plscolbg(/*in*/ int r, /*in*/ int g, /*in*/ int b);

_CAMLIDL_EXTERN_C void c_plscolbga(/*in*/ int r, /*in*/ int g, /*in*/ int b, /*in*/ double a);

_CAMLIDL_EXTERN_C void c_plscolor(/*in*/ int color);

_CAMLIDL_EXTERN_C void c_plscompression(/*in*/ int compression);

_CAMLIDL_EXTERN_C void c_plsdev(/*in*/ char const *devname);

_CAMLIDL_EXTERN_C void c_plsdidev(/*in*/ double mar, /*in*/ double aspect, /*in*/ double jx, /*in*/ double jy);

_CAMLIDL_EXTERN_C void c_plsdimap(/*in*/ int dimxmin, /*in*/ int dimxmax, /*in*/ int dimymin, /*in*/ int dimymax, /*in*/ double dimxpmm, /*in*/ double dimypmm);

_CAMLIDL_EXTERN_C void c_plsdiori(/*in*/ double rot);

_CAMLIDL_EXTERN_C void c_plsdiplt(/*in*/ double xmin, /*in*/ double ymin, /*in*/ double xmax, /*in*/ double ymax);

_CAMLIDL_EXTERN_C void c_plsdiplz(/*in*/ double xmin, /*in*/ double ymin, /*in*/ double xmax, /*in*/ double ymax);

_CAMLIDL_EXTERN_C void c_plseed(/*in*/ unsigned int s);

_CAMLIDL_EXTERN_C void c_plsesc(/*in*/ char esc);

_CAMLIDL_EXTERN_C void c_plsfam(/*in*/ int fam, /*in*/ int num, /*in*/ int bmax);

_CAMLIDL_EXTERN_C void c_plsfci(/*in*/ long long fci);

_CAMLIDL_EXTERN_C void c_plsfnam(/*in*/ char const *fnam);

_CAMLIDL_EXTERN_C void c_plsfont(/*in*/ int family, /*in*/ int style, /*in*/ int weight);

_CAMLIDL_EXTERN_C void c_plsmaj(/*in*/ double def, /*in*/ double scale);

_CAMLIDL_EXTERN_C void c_plsmin(/*in*/ double def, /*in*/ double scale);

_CAMLIDL_EXTERN_C void c_plsdrawmode(/*in*/ int mode);

_CAMLIDL_EXTERN_C void c_plsori(/*in*/ int ori);

_CAMLIDL_EXTERN_C void c_plspage(/*in*/ double xp, /*in*/ double yp, /*in*/ int xleng, /*in*/ int yleng, /*in*/ int xoff, /*in*/ int yoff);

_CAMLIDL_EXTERN_C void c_plspal0(/*in*/ char const *filename);

_CAMLIDL_EXTERN_C void c_plspal1(/*in*/ char const *filename, /*in*/ int interpolate);

_CAMLIDL_EXTERN_C void c_plspause(/*in*/ int pause);

_CAMLIDL_EXTERN_C void c_plsstrm(/*in*/ int strm);

_CAMLIDL_EXTERN_C void c_plssub(/*in*/ int nx, /*in*/ int ny);

_CAMLIDL_EXTERN_C void c_plssym(/*in*/ double def, /*in*/ double scale);

_CAMLIDL_EXTERN_C void c_plstar(/*in*/ int nx, /*in*/ int ny);

_CAMLIDL_EXTERN_C void c_plstart(/*in*/ char const *devname, /*in*/ int nx, /*in*/ int ny);

_CAMLIDL_EXTERN_C void c_plstring(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ char const *string);

_CAMLIDL_EXTERN_C void c_plstring3(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ double *z, /*in*/ char const *string);

_CAMLIDL_EXTERN_C void c_plstripa(/*in*/ int id, /*in*/ int pen, /*in*/ double x, /*in*/ double y);

_CAMLIDL_EXTERN_C void c_plstripd(/*in*/ int id);

_CAMLIDL_EXTERN_C void c_plimage(/*in*/ double **idata, /*in*/ int nx, /*in*/ int ny, /*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax, /*in*/ double zmin, /*in*/ double zmax, /*in*/ double Dxmin, /*in*/ double Dxmax, /*in*/ double Dymin, /*in*/ double Dymax);

_CAMLIDL_EXTERN_C void c_plstyl(/*in*/ int nms, /*in*/ int *mark, /*in*/ int *space);

_CAMLIDL_EXTERN_C void c_plsurf3d(/*in*/ double *x, /*in*/ double *y, /*in*/ double **z, /*in*/ int nx, /*in*/ int ny, /*in*/ plplot3d_style opt, /*in*/ double *clevel, /*in*/ int nlevel);

_CAMLIDL_EXTERN_C void c_plsvect(/*in*/ double *arrowx, /*in*/ double *arrowy, /*in*/ int npts, /*in*/ int fill);

_CAMLIDL_EXTERN_C void c_plsvpa(/*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax);

_CAMLIDL_EXTERN_C void c_plsxax(/*in*/ int digmax, /*in*/ int digits);

_CAMLIDL_EXTERN_C void plsxwin(/*in*/ int window_id);

_CAMLIDL_EXTERN_C void c_plsyax(/*in*/ int digmax, /*in*/ int digits);

_CAMLIDL_EXTERN_C void c_plsym(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ int code);

_CAMLIDL_EXTERN_C void c_plszax(/*in*/ int digmax, /*in*/ int digits);

_CAMLIDL_EXTERN_C void c_pltext(void);

_CAMLIDL_EXTERN_C void c_pltimefmt(/*in*/ char const *fmt);

_CAMLIDL_EXTERN_C void c_plvasp(/*in*/ double aspect);

_CAMLIDL_EXTERN_C void c_plvpas(/*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax, /*in*/ double aspect);

_CAMLIDL_EXTERN_C void c_plvpor(/*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax);

_CAMLIDL_EXTERN_C void c_plvsta(void);

_CAMLIDL_EXTERN_C void c_plw3d(/*in*/ double basex, /*in*/ double basey, /*in*/ double height, /*in*/ double xmin0, /*in*/ double xmax0, /*in*/ double ymin0, /*in*/ double ymax0, /*in*/ double zmin0, /*in*/ double zmax0, /*in*/ double alt, /*in*/ double az);

_CAMLIDL_EXTERN_C void c_plwidth(/*in*/ double width);

_CAMLIDL_EXTERN_C void c_plwind(/*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax);

_CAMLIDL_EXTERN_C void c_plxormod(/*in*/ int mode, /*out*/ int *status);

_CAMLIDL_EXTERN_C nonzero_error_int c_plsetopt(/*in*/ char const *opt, /*in*/ char const *optarg);

_CAMLIDL_EXTERN_C void plMinMax2dGrid(/*in*/ double **f, /*in*/ int nx, /*in*/ int ny, /*out*/ double *fmax, /*out*/ double *fmin);

_CAMLIDL_EXTERN_C void ml_plcont(/*in*/ double **f, /*in*/ int nx, /*in*/ int ny, /*in*/ int kx, /*in*/ int lx, /*in*/ int ky, /*in*/ int ly, /*in*/ double *clevel, /*in*/ int nlevel);

_CAMLIDL_EXTERN_C void ml_plshade(/*in*/ double **a, /*in*/ int nx, /*in*/ int ny, /*in*/ double left, /*in*/ double right, /*in*/ double bottom, /*in*/ double top, /*in*/ double shade_min, /*in*/ double shade_max, /*in*/ int sh_cmap, /*in*/ double sh_color, /*in*/ double sh_width, /*in*/ int min_color, /*in*/ double min_width, /*in*/ int max_color, /*in*/ double max_width, /*in*/ int rectangular);

_CAMLIDL_EXTERN_C void ml_plshades(/*in*/ double **a, /*in*/ int nx, /*in*/ int ny, /*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax, /*in*/ double *clevel, /*in*/ int nlevel, /*in*/ double fill_width, /*in*/ int cont_color, /*in*/ double cont_width, /*in*/ int rectangular);

_CAMLIDL_EXTERN_C void ml_plimagefr(/*in*/ double **idata, /*in*/ int nx, /*in*/ int ny, /*in*/ double xmin, /*in*/ double xmax, /*in*/ double ymin, /*in*/ double ymax, /*in*/ double zmin, /*in*/ double zmax, /*in*/ double valuemin, /*in*/ double valuemax);

_CAMLIDL_EXTERN_C void ml_plvect(/*in*/ double **u, /*in*/ double **v, /*in*/ int nx, /*in*/ int ny, /*in*/ double scale);

_CAMLIDL_EXTERN_C void ml_plmap(/*in*/ char const *type, /*in*/ double minlong, /*in*/ double maxlong, /*in*/ double minlat, /*in*/ double maxlat);

_CAMLIDL_EXTERN_C void ml_plmeridians(/*in*/ double dlong, /*in*/ double dlat, /*in*/ double minlong, /*in*/ double maxlong, /*in*/ double minlat, /*in*/ double maxlat);

_CAMLIDL_EXTERN_C void ml_plpoly3(/*in*/ int n, /*in*/ double *x, /*in*/ double *y, /*in*/ double *z, /*in*/ int ndraw, /*in*/ int *draw, /*in*/ int ifcc);

_CAMLIDL_EXTERN_C void ml_pltr0(/*in*/ double x, /*in*/ double y, /*out*/ double *tx, /*out*/ double *ty);

_CAMLIDL_EXTERN_C void ml_plsvect_reset(void);

_CAMLIDL_EXTERN_C int plg_current_col0(void);

_CAMLIDL_EXTERN_C double plg_current_col1(void);

_CAMLIDL_EXTERN_C double plgwidth(void);

_CAMLIDL_EXTERN_C double plgchrht(void);

#ifdef _WIN32
#pragma pack(pop)
#endif


#endif /* !_CAMLIDL_PLPLOT_CORE_H */
