/***********************************************************************/
/*                                                                     */
/*                           Active dvi                                */
/*                                                                     */
/*            Jun Furuse, Pierre Weis, Didier Rémy                     */
/*            projet Cristal, INRIA Rocquencourt                       */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <caml/memory.h>
#include <caml/alloc.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#define XK_MISCELLANY
#include <X11/keysymdef.h>
#include <caml/fail.h>
#include "libgraph.h"
#include "image.h"

#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <unistd.h>


#include "grwm.h"
/* the HAVE_XINERAMA variable should be defined by configure */
/* #define HAVE_XINERAMA */
#ifdef HAVE_XINERAMA
#include <X11/extensions/Xinerama.h>
#endif

value caml_gr_get_color(void)
{
  return Val_int(caml_gr_color);
}

value caml_gr_bsize_x(void)
{
  caml_gr_check_open();
  return Val_int(caml_gr_bstore.w);
}

value caml_gr_raw_draw_image_area(value im, value x, value y, value w, value h,
                         value vx, value vy)
{
  int dest_x = Int_val(vx);
  int dest_y = Int_val(vy);
  int wdest_y = Wcvt(dest_y) + 1 - Height_im(im);
  int bdest_y = Bcvt(dest_y) + 1 - Height_im(im);
  int width = Int_val(w);
  int height = Int_val(h);
  int src_x = Int_val(x);
  int src_y = Int_val(y);

  caml_gr_check_open();
  if (width > Width_im(im) - src_x) width = Width_im(im) - src_x;
  if (height > Height_im(im) - src_y) height = Width_im(im) - src_y;
  if (Mask_im(im) != None) {
    if(caml_gr_remember_modeflag) {
      XSetClipOrigin(caml_gr_display, caml_gr_bstore.gc, dest_x, bdest_y);
      XSetClipMask(caml_gr_display, caml_gr_bstore.gc, Mask_im(im));
    }
    if(caml_gr_display_modeflag) {
      XSetClipOrigin(caml_gr_display, caml_gr_window.gc, dest_x, wdest_y);
      XSetClipMask(caml_gr_display, caml_gr_window.gc, Mask_im(im));
    }
  }
  if(caml_gr_remember_modeflag)
    XCopyArea(caml_gr_display, Data_im(im), caml_gr_bstore.win, caml_gr_bstore.gc,
              src_x, src_y,
              width, height,
              dest_x, bdest_y);
  if(caml_gr_display_modeflag)
    XCopyArea(caml_gr_display, Data_im(im), caml_gr_window.win, caml_gr_window.gc,
              src_x, src_y,
              width, height,
              dest_x, wdest_y);
  if (Mask_im(im) != None) {
    if(caml_gr_remember_modeflag)
      XSetClipMask(caml_gr_display, caml_gr_bstore.gc, None);
    if(caml_gr_display_modeflag)
      XSetClipMask(caml_gr_display, caml_gr_window.gc, None);
  }
  if(caml_gr_display_modeflag)
    XFlush(caml_gr_display);
  return Val_unit;
}

value caml_gr_draw_image_area(value im, value xywh, value vx, value vy)
{
  value x = Field (xywh, 0);
  value y = Field (xywh, 1);
  value w = Field (xywh, 2);
  value h = Field (xywh, 3);
  caml_gr_raw_draw_image_area(im, x, y, w, h, vx, vy);
  return Val_unit;
}
/****
value caml_gr_draw_image_area(value im, value xywh, value vx, value vy)
{
  int dest_x = Int_val(vx);
  int dest_y = Int_val(vy);
  int wdest_y = Wcvt(dest_y) + 1 - Height_im(im);
  int bdest_y = Bcvt(dest_y) + 1 - Height_im(im);
  int width = Int_val(Field (xywh, 2));
  int height = Int_val(Field (xywh, 3));
  int src_x = Int_val(Field (xywh, 0));
  int src_y = Int_val(Field (xywh, 1));

  caml_gr_check_open();
  if (width > Width_im(im) - src_x) width = Width_im(im) - src_x;
  if (height > Height_im(im) - src_y) height = Width_im(im) - src_y;
  if (Mask_im(im) != None) {
    if(caml_gr_remember_modeflag) {
      XSetClipOrigin(caml_gr_display, caml_gr_bstore.gc, dest_x, bdest_y);
      XSetClipMask(caml_gr_display, caml_gr_bstore.gc, Mask_im(im));
    }
    if(caml_gr_display_modeflag) {
      XSetClipOrigin(caml_gr_display, caml_gr_window.gc, dest_x, wdest_y);
      XSetClipMask(caml_gr_display, caml_gr_window.gc, Mask_im(im));
    }
  }
  if(caml_gr_remember_modeflag)
    XCopyArea(caml_gr_display, Data_im(im), caml_gr_bstore.win, caml_gr_bstore.gc,
              src_x, src_y,
              width, height,
              dest_x, bdest_y);
  if(caml_gr_display_modeflag)
    XCopyArea(caml_gr_display, Data_im(im), caml_gr_window.win, caml_gr_window.gc,
              src_x, src_y,
              width, height,
              dest_x, wdest_y);
  if (Mask_im(im) != None) {
    if(caml_gr_remember_modeflag)
      XSetClipMask(caml_gr_display, caml_gr_bstore.gc, None);
    if(caml_gr_display_modeflag)
      XSetClipMask(caml_gr_display, caml_gr_window.gc, None);
  }
  if(caml_gr_display_modeflag)
    XFlush(caml_gr_display);
  return Val_unit;
}
***/

/* Should be :
value caml_gr_draw_image(value im, value vx, value vy)
{
  caml_gr_raw_draw_image_area(im, 0, 0, (Width_im(im)), (Height_im(im)), vx, vy);
  return Val_unit;
} */

value caml_gr_anti_synchronize(void)
{
  caml_gr_check_open();
  XCopyArea(caml_gr_display, caml_gr_window.win, caml_gr_bstore.win, caml_gr_window.gc,
            0, caml_gr_bstore.h - caml_gr_window.h,
            caml_gr_window.w, caml_gr_window.h,
            0, 0);
  XFlush(caml_gr_display);
  return Val_unit;
}

value caml_gr_window_point_color(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XImage * im;
  int rgb;

  caml_gr_check_open();
  im = XGetImage(caml_gr_display, caml_gr_window.win, x, Bcvt(y), 1, 1, (-1), ZPixmap);
  rgb = caml_gr_rgb_pixel(XGetPixel(im, 0, 0));
  XDestroyImage(im);
  return Val_int(rgb);
}

value caml_gr_bsize_y(void)
{
  caml_gr_check_open();
  return Val_int(caml_gr_bstore.h);
}

value caml_gr_screen_x(void)
{
  XWindowAttributes att;
  caml_gr_check_open();
  XGetWindowAttributes (caml_gr_display, DefaultRootWindow(caml_gr_display), &att); 
  return Val_int(att.width);
}

value caml_gr_screen_y(void)
{
  XWindowAttributes att;
  caml_gr_check_open();
  XGetWindowAttributes (caml_gr_display, DefaultRootWindow(caml_gr_display), &att); 
  return Val_int(att.height);
}

void caml_gr_origin(int* x, int* y)
{
  Window win, root, parent, r;
  Window* children;
  int dx, dy;
  unsigned int  w, h, b, d; 
  caml_gr_check_open();
  win = caml_gr_window.win;
  root = DefaultRootWindow(caml_gr_display);
  *x = 0; *y = 0;
  while (win != root) {
    XGetGeometry (caml_gr_display, win, &r, &dx, &dy, &w, &h, &b, &d);
    *x += dx; *y += dy;
    XQueryTree (caml_gr_display, win, &r, &parent, &children, &b);
    if(children != NULL) XFree(children); 
    win = parent;
  }
  return;
}

value caml_gr_origin_x(void)
{
  int x, y;
  caml_gr_origin (&x, &y); 
  return Val_int(x);
}
value caml_gr_origin_y(void)
{
  int x, y;
  caml_gr_origin (&x, &y); 
  return Val_int(y);
}

/* Should be caml_gr_window_id and caml_gr_window should disapear. */
/* Graphics now uses the name caml_gr_window */
value caml_gr_get_window_id(void)
{ unsigned int w; 
  caml_gr_check_open();
  w = caml_gr_window.win;
  return copy_int32 (w);
}

value caml_gr_get_bstore_id(void)
{ unsigned int w; 
  caml_gr_check_open();
  w = caml_gr_bstore.win;
  return copy_int32 (w); 
}

value caml_gr_flush(void)
{
  caml_gr_check_open();
  XFlush(caml_gr_display);
  return Val_unit ;
}

value caml_gr_sync(void)
{
  caml_gr_check_open();
  XSync(caml_gr_display, 0);
  return Val_unit ;
}

/* The following is not the best, since it unsets the selection 
   It would be better to own the selection. However, the event loop should
   then be changed */
value caml_gr_cut (value string) {
  /* The following, suggested by Fabrice does not work */
  /* 
  Atom cut_buffers[] = 
     { XA_CUT_BUFFER0,
       XA_CUT_BUFFER1, 
       XA_CUT_BUFFER2, 
       XA_CUT_BUFFER3, 
       XA_CUT_BUFFER4, 
       XA_CUT_BUFFER5, 
       XA_CUT_BUFFER6, 
       XA_CUT_BUFFER7
     };
  XRotateWindowProperties (caml_gr_display, caml_gr_window.win,
                           cut_buffers, 8, 1);
  */
  /* XRotateBuffers(caml_gr_display, 1); */
  /*
  XChangeProperty (caml_gr_display, caml_gr_window.win, 
                   XA_CUT_BUFFER0,	      
                   XA_STRING,                  
                   8,       			
                   PropModeReplace, 		
                   String_val (string),
                   String_length (string)
                   );
  */
  XStoreBytes (caml_gr_display, String_val (string), string_length (string)); 
  XSetSelectionOwner (caml_gr_display,
                      XA_PRIMARY,
                      None,
                      CurrentTime);
  XSync (caml_gr_display, False);
  return Val_unit; 
}

value caml_gr_set_named_atom_property (value name, value string) {
  Atom a = XInternAtom (caml_gr_display, String_val(name), 0);
  XChangeProperty (caml_gr_display, caml_gr_window.win, 
                   a,       			/* property */ 
                   XA_STRING,                   /* xa_string */ 
                   8,       			/* format */
                   PropModeReplace, 		/* mode */
                   (unsigned char*) 
                     String_val(string),	/* data */
                   string_length (string)       /* nelements */
                   );
  XSync(caml_gr_display, False);
  return Val_unit;
}

value caml_gr_set_cursor(value glyphid) {
  Cursor c;
  int gid;
  gid = Int_val(glyphid);
  caml_gr_check_open();
  if (gid < 0 || gid >= XC_num_glyphs) {
    invalid_argument("set_cursor");
  }
  c = XCreateFontCursor(caml_gr_display, gid);
  XDefineCursor(caml_gr_display, caml_gr_window.win, c);
  XSync(caml_gr_display, False);
  return Val_unit;
}

value caml_gr_unset_cursor(value unit) {
  XUndefineCursor(caml_gr_display, caml_gr_window.win);
  XSync(caml_gr_display, False);
  return Val_unit;
}

/* may not be correct ? may use the output of xwininfo ? */
void get_position_against_root( Window w, int *pos )
{
  Window root, parent;
  Window *children;
  unsigned int nchildren;
  XWindowAttributes attr;

  caml_gr_check_open();
  XGetWindowAttributes(caml_gr_display, w, &attr);
  pos[0] += attr.x;
  pos[1] += attr.y;
  XQueryTree(caml_gr_display, w, &root, &parent, &children, &nchildren);
  if(children != NULL){
    XFree(children);
  }
  if( root == parent ){ 
    return; 
  } else {
    get_position_against_root( parent, pos );
  }
}

value caml_gr_get_geometry(value unit){
  CAMLparam1(unit);
  CAMLlocal1(res);
  XWindowAttributes attr;
  int pos[2] = {0,0};

  caml_gr_check_open();
  XGetWindowAttributes(caml_gr_display, caml_gr_window.win, &attr);
  get_position_against_root( caml_gr_window.win, pos );

  res = alloc_tuple(4);
  Field(res,0) = Val_int(attr.width);
  Field(res,1) = Val_int(attr.height);
  Field(res,2) = Val_int(pos[0]);
  Field(res,3) = Val_int(pos[1]);
  CAMLreturn(res);
}

/* get modifiers... */
value caml_gr_get_modifiers(void)
{
  int button;
  /* int mouse_x, mouse_y, key, keypressed; */
  Window rootwin, childwin;
  int root_x, root_y, win_x, win_y;
  unsigned int modifiers;
  /* unsigned int i; */

  caml_gr_check_open();
  if (XQueryPointer(caml_gr_display, caml_gr_window.win,
                    &rootwin, &childwin,
                    &root_x, &root_y, &win_x, &win_y,
                    &modifiers)) {
    button = 0;
    /*     fprintf(stderr,"C modifiers = %u\n", modifiers); */
    if (modifiers & Button1Mask) button = button | 0x1;
    if (modifiers & Button2Mask) button = button | 0x2;
    if (modifiers & Button3Mask) button = button | 0x4;
    if (modifiers & Button4Mask) button = button | 0x8;
    if (modifiers & Button5Mask) button = button | 0x10;
    
    if (modifiers & ShiftMask) button = button | 0x100;
    if (modifiers & ControlMask) button = button | 0x200;
    if (modifiers & Mod1Mask) button = button | 0x400;
    if (modifiers & Mod2Mask) button = button | 0x800;
    if (modifiers & Mod3Mask) button = button | 0x1000;
  } else {
    button = -1;
  }
  return Val_int(button);
}

/* Sub windows */
value caml_gr_open_sub_window(value vx, value vy, value width, value height)
{
  Window win;

  int h = Int_val(height);
  int w = Int_val(width);
  int x = Int_val(vx);
  int y = Int_val(vy);

  caml_gr_check_open();
  win = XCreateSimpleWindow(caml_gr_display, caml_gr_window.win,
                            x, Wcvt(y + h), 
                            w, h,
                            0, caml_gr_black, caml_gr_background);
  XMapWindow(caml_gr_display, win);
  XFlush(caml_gr_display);

  return (caml_gr_id_of_window(win));
}

/* In caml_gr_aphics */
value caml_gr_close_subwindow2(value wid)
{
  Window win;

  caml_gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XDestroyWindow(caml_gr_display, win);
  XFlush(caml_gr_display);
  return Val_unit;
}

value caml_gr_map_window(value wid)
{
  Window win;

  caml_gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XMapWindow(caml_gr_display, win);
  XFlush(caml_gr_display);
  return Val_unit;
}

value caml_gr_unmap_window(value wid)
{
  Window win;

  caml_gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XUnmapWindow(caml_gr_display, win);
  XFlush(caml_gr_display);
  return Val_unit;
}

value caml_gr_move_window (value wid, value caml_gr_x, value caml_gr_y, value height)
{
  Window win;

  int x = Int_val(caml_gr_x);
  int y = Int_val(caml_gr_y);
  int h = Int_val(height);

  caml_gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XMoveWindow(caml_gr_display, win, x, Wcvt(y + h));
  XFlush(caml_gr_display);
  return Val_unit;
}

value caml_gr_resize_subwindow (value wid, value w, value h)
{
  Window win;

  caml_gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XResizeWindow(caml_gr_display, win, Int_val(w), Int_val(h));
  XFlush(caml_gr_display);
  return Val_unit;
}


/* screen is the screen number */

value caml_gr_reposition (value x, value y, value w, value h, value scr)
{
  /* Window r; */
  int posx, posy, width, height, screen;
#ifdef HAVE_XINERAMA
  int xinerama_x=0, xinerama_y=0;
#endif
  Bool fullscreen;
  XWindowAttributes att;

  caml_gr_check_open(); 

  posx = Int_val(x);
  posy = Int_val(y);
  width = Int_val(w);
  height = Int_val(h);
  screen = Int_val(scr);

  /* create the X atoms: should be done only once */
  init_atoms(caml_gr_display);

  if (width < 0) {    /* means fullscreen */
    XGetWindowAttributes(caml_gr_display, DefaultRootWindow(caml_gr_display), &att);
    width = att.width; height = att.height;
#ifdef HAVE_XINERAMA
 if(XineramaIsActive(caml_gr_display))
  {
  XineramaScreenInfo *screens;
  int num_screens;
  int theScreen=0;

  screens = XineramaQueryScreens(caml_gr_display, &num_screens);
  fprintf(stderr,"num_screens=%d, screen=%d\n",num_screens,screen);
  if (screen < num_screens) {theScreen=screen;}; 
  width=screens[theScreen].width;
  height=screens[theScreen].height;
  xinerama_x=screens[theScreen].x_org;
  xinerama_y=screens[theScreen].y_org;
  };
#endif
    fullscreen = True;}
  else fullscreen=False;
  
  /* add or remove the decorations */
  x11_decoration(caml_gr_display,caml_gr_window.win,!fullscreen);

  /* update size hints, essential for KDE */
  x11_sizehint(caml_gr_display, caml_gr_window.win, posx, posy, width, height); 

  x11_fullscreen(caml_gr_display,caml_gr_window.win,posx,posy,width,height, fullscreen ? True:False);

  XMoveResizeWindow(caml_gr_display, caml_gr_window.win, posx, posy, width, height);
#ifdef HAVE_XINERAMA
 if(XineramaIsActive(caml_gr_display))
  {
    if (fullscreen==True) {XMoveWindow(caml_gr_display,caml_gr_window.win,xinerama_x,xinerama_y);};
  }
#endif
  XMapRaised(caml_gr_display,caml_gr_window.win );
  XRaiseWindow(caml_gr_display,caml_gr_window.win );

  caml_gr_window.w = width;
  caml_gr_window.h = height;
  if (caml_gr_window.w > caml_gr_bstore.w || caml_gr_window.h > caml_gr_bstore.h) {

    /* Allocate a new backing store large enough to accomodate
         both the old backing store and the current window. */
    struct canvas newbstore;
    newbstore.w = max(caml_gr_window.w, caml_gr_bstore.w);
    newbstore.h = max(caml_gr_window.h, caml_gr_bstore.h);
    newbstore.win =
      XCreatePixmap(caml_gr_display, caml_gr_window.win, newbstore.w, newbstore.h,
                    XDefaultDepth(caml_gr_display, caml_gr_screen));
    newbstore.gc = XCreateGC(caml_gr_display, newbstore.win, 0, NULL);
    XSetBackground(caml_gr_display, newbstore.gc, caml_gr_white);
    XSetForeground(caml_gr_display, newbstore.gc, caml_gr_white);
    XFillRectangle(caml_gr_display, newbstore.win, newbstore.gc,
                   0, 0, newbstore.w, newbstore.h);
    XSetForeground(caml_gr_display, newbstore.gc, caml_gr_color);
    if (caml_gr_font != NULL)
      XSetFont(caml_gr_display, newbstore.gc, caml_gr_font->fid);

    /* Copy the old backing store into the new one */
    XCopyArea(caml_gr_display, caml_gr_bstore.win, newbstore.win, newbstore.gc,
              0, 0, caml_gr_bstore.w, caml_gr_bstore.h, 0, newbstore.h - caml_gr_bstore.h);

    /* Free the old backing store */
    XFreeGC(caml_gr_display, caml_gr_bstore.gc);
    XFreePixmap(caml_gr_display, caml_gr_bstore.win);

    /* Use the new backing store */
    caml_gr_bstore = newbstore;
    }

  XFlush(caml_gr_display);
  return Val_unit;
}

value caml_gr_rebind_keysyms(value unit)
{
  KeySym modifiers[1] = { 0 };
  unsigned char* str; 
  str = (unsigned char*) "N";
  XRebindKeysym(caml_gr_display, XK_Next, modifiers, 0, str, 1);
  str = (unsigned char*) "j";
  XRebindKeysym(caml_gr_display, XK_Down, modifiers, 0, str, 1);
  str = (unsigned char*)"P";
  XRebindKeysym(caml_gr_display, XK_Prior, modifiers, 0, str, 1);
  str = (unsigned char*) "k";
  XRebindKeysym(caml_gr_display, XK_Up, modifiers, 0, str, 1);
  str = (unsigned char*)",";
  XRebindKeysym(caml_gr_display, XK_Home, modifiers, 0, str, 1);
  str = (unsigned char*)".";
  XRebindKeysym(caml_gr_display, XK_End, modifiers, 0, str, 1);
  str = (unsigned char*)"\b";
  XRebindKeysym(caml_gr_display, XK_Left, modifiers, 0, str, 1);
  str = (unsigned char*)"\r";
  XRebindKeysym(caml_gr_display, XK_Right, modifiers, 0, str, 1);
  str = (unsigned char*)"t";
  XRebindKeysym(caml_gr_display, XK_Insert, modifiers, 0, str, 1);
  return Val_unit;
}


value caml_gr_get_button(value m)
{
  int modifiers = Int_val(m);
  int button = 0;
  if (modifiers & Button1Mask) button = 1;
  else if (modifiers & Button2Mask) button = 2;
  else if (modifiers & Button3Mask) button = 3;
  else if (modifiers & Button4Mask) button = 4;
  else if (modifiers & Button5Mask) button = 5;
  return Val_int(button);
}

