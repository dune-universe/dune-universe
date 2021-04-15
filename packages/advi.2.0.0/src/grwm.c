/***********************************************************************/
/*                                                                     */
/*                           Active dvi                                */
/*                                                                     */
/*            Roberto Di Cosmo                                        */
/*            projet Cristal, INRIA Rocquencourt                       */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* Inspired from mplayer */

/* But see http://freedesktop.org/Standards/wm-spec/wm-spec-1.3.html */
/* for "official" specifications                                     */

#include <string.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include "libgraph.h"

#include "grwm.h"

static Atom XA_NET_SUPPORTED;
static Atom XA_NET_WM_STATE;
static Atom XA_NET_WM_STATE_FULLSCREEN;
static Atom XA_NET_WM_STATE_ABOVE;
static Atom XA_NET_WM_STATE_STAYS_ON_TOP;
static Atom XA_NET_WM_STATE_BELOW;
static Atom XA_NET_WM_PID;
static Atom XA_WIN_PROTOCOLS;
static Atom XA_WIN_LAYER;
static Atom XA_WIN_HINTS;
static Atom XA_BLACKBOX_PID;

void init_atoms(Display * mDisplay)
{
 XA_NET_SUPPORTED  = XInternAtom(mDisplay, "_NET_SUPPORTED", 0 ) ;
 XA_NET_WM_STATE  = XInternAtom(mDisplay, "_NET_WM_STATE", 0 ) ;
 XA_NET_WM_STATE_FULLSCREEN  = XInternAtom(mDisplay, "_NET_WM_STATE_FULLSCREEN", 0 ) ;
 XA_NET_WM_STATE_ABOVE  = XInternAtom(mDisplay, "_NET_WM_STATE_ABOVE", 0 ) ;
 XA_NET_WM_STATE_STAYS_ON_TOP  = XInternAtom(mDisplay, "_NET_WM_STATE_STAYS_ON_TOP", 0 ) ;
 XA_NET_WM_STATE_BELOW  = XInternAtom(mDisplay, "_NET_WM_STATE_BELOW", 0 ) ;
 XA_NET_WM_PID  = XInternAtom(mDisplay, "_NET_WM_PID", 0 ) ;
 XA_WIN_PROTOCOLS  = XInternAtom(mDisplay, "_WIN_PROTOCOLS", 0 ) ;
 XA_WIN_LAYER  = XInternAtom(mDisplay, "_WIN_LAYER", 0 ) ;
 XA_WIN_HINTS  = XInternAtom(mDisplay, "_WIN_HINTS", 0 ) ;
 XA_BLACKBOX_PID  = XInternAtom(mDisplay, "_BLACKBOX_PID", 0 ) ;
}

typedef struct
{
  long flags;
  long functions;
  long decorations;
  long input_mode;
  long state;
} MotifWmHints;

void x11_decoration( Display *dpy,Window w,int decorate)
{
  MotifWmHints MotifWmHints;
  Atom MotifHints;

  MotifHints=XInternAtom( dpy,"_MOTIF_WM_HINTS",0 );
  if ( MotifHints != None )
  {
   memset( &MotifWmHints,0,sizeof( MotifWmHints ) );
   MotifWmHints.flags=MWM_HINTS_FUNCTIONS | MWM_HINTS_DECORATIONS;
   if ( decorate )
    {
      MotifWmHints.functions=MWM_FUNC_MOVE | MWM_FUNC_CLOSE | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE;
      decorate=MWM_DECOR_ALL|MWM_DECOR_MENU;
    }
   
   MotifWmHints.decorations=decorate;
   printf("   MotifWmHints.decorations=%d\n",decorate);
   XChangeProperty( dpy,w,MotifHints,MotifHints,32,
                    PropModeReplace,(unsigned char *)&MotifWmHints,5);
  }
}

void x11_sizehint(Display *mDisplay, Window window, int x, int y, int width, int height)
{
XSizeHints hint;

 hint.flags=PPosition | PSize | PWinGravity | PAspect;
 hint.min_aspect.x = width;
 hint.min_aspect.y = height;
 hint.max_aspect.x = width;
 hint.max_aspect.y = height;

 hint.x=x; hint.y=y; hint.width=width; hint.height=height;
 hint.max_width=width; hint.max_height=height;
 hint.flags|=PMaxSize;
 hint.win_gravity=StaticGravity;
 XSetWMNormalHints( mDisplay,window,&hint );
}

static int x11_get_property(Display *dpy, Window w,
                            Atom type, Atom **args, unsigned long *nitems)
{
  int             format;
  unsigned long   bytesafter;

  return (Success == XGetWindowProperty( dpy,w,type,0,16384,
                                         False,AnyPropertyType,&type,&format,nitems,&bytesafter,
                                         (unsigned char **) args ) && *nitems > 0 );
}

#define wm_LAYER 1
#define wm_FULLSCREEN 2
#define wm_STAYS_ON_TOP 4
#define wm_ABOVE 8
#define wm_BELOW 16
#define wm_NETWM (wm_FULLSCREEN | wm_STAYS_ON_TOP | wm_ABOVE | wm_BELOW)

static int net_wm_support_state_test(Atom atom)
{
#define NET_WM_STATE_TEST(x) { if (atom == XA_NET_WM_STATE_##x) { return wm_##x; } }
 
 NET_WM_STATE_TEST(FULLSCREEN);
 NET_WM_STATE_TEST(ABOVE);
 NET_WM_STATE_TEST(STAYS_ON_TOP);
 NET_WM_STATE_TEST(BELOW);
 return 0;
}

int wm_detect(Display *dpy, Window w)
{
 int             i;
 int             wm = 0;
 int             metacity_hack = 0;
 unsigned long   nitems;
 Atom          * args = NULL;
 
// -- supports layers
  if (x11_get_property(dpy, w,XA_WIN_PROTOCOLS, &args, &nitems))
  {
   for (i = 0; i < nitems; i++)
   {
     if ( args[i] == XA_WIN_LAYER) {
       wm |= wm_LAYER;
       metacity_hack |= 1;
     } else
       // metacity is the only manager mplayer's authors know which reports support only for _WIN_LAYER
       // hint in _WIN_PROTOCOLS (what's more support for it, they say, is broken)
       metacity_hack |= 2;
   }
   XFree( args );
   if (wm && (metacity_hack == 1))
   {
     // metacity reports that it supports layers, but it is not really truth :-)
     wm ^= wm_LAYER;
   }
  }

// --- netwm 
  if (x11_get_property(dpy,w,XA_NET_SUPPORTED, &args, &nitems))
  {
   for (i = 0; i < nitems; i++)
     wm |= net_wm_support_state_test (args[i]);
   XFree( args );
  }

 return wm;
}    

void x11_setlayer(Display * dpy,Window w,int fs_type, int fs)
{
   XClientMessageEvent  xev;
   char *state;
   if (fs_type & wm_NETWM) /* the following distorts non NETWM aware WMs */
 {
   memset( &xev,0,sizeof( xev ) );
   xev.type= 33 ;
   xev.message_type=XA_NET_WM_STATE;
   xev.display=dpy;
   xev.window=w;
   xev.format=32;
   xev.data.l[0]=fs; /* True sets the property, False removes it */

   if ( fs_type & wm_STAYS_ON_TOP )
     xev.data.l[1]=XA_NET_WM_STATE_STAYS_ON_TOP;
   else
   if ( fs_type & wm_ABOVE )
     xev.data.l[1]=XA_NET_WM_STATE_ABOVE;
   else
   if ( fs_type & wm_FULLSCREEN )
     xev.data.l[1]=XA_NET_WM_STATE_FULLSCREEN;
   else
   if ( fs_type & wm_BELOW )
     // This is not fallback. We can safely assume that situation where
     // only NETWM_STATE_BELOW is supported and others not, doesn't exist.
     xev.data.l[1]=XA_NET_WM_STATE_BELOW;

   xev.data.l[1]=XA_NET_WM_STATE_STAYS_ON_TOP;
   
   XSendEvent(dpy,RootWindow(dpy,DefaultScreen(dpy)),0 ,(1L<<20) ,(XEvent*)&xev );
   state = XGetAtomName (dpy, xev.data.l[1]);
   XFree (state);
 }
}


void x11_fullscreen(Display * dpy,Window w, int posx, int posy, int width, int height, int fs)
{
  int fs_style;
  Window root;
  int screen;

  root = DefaultRootWindow(dpy);
  screen = DefaultScreen(dpy);

  /* try to figure out what kind of fs capabilities the wm offers */
  fs_style=wm_detect(dpy, root);

  fprintf(stderr,"FS_STYLE=%d\n",fs_style);

  /* If fullscreen, make sure our window is in front of all the others */
  x11_setlayer(dpy, w, fs_style, fs? True: False); 

  // This is required for MWM, but kills the decorations on other WMs.. (sawfish, fvwm95)
  //if(fs_style==0) XWithdrawWindow(dpy,w,screen); // required for MWM
}
