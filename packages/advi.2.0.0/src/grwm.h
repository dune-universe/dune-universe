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


/* Window manager manipulation functions to handle "fullscreen" mode */
/* The code from here to gr_reposition is extracted and reorganized
   from the WM detection logic and fullscreen routines of mplayer    */

/* TODO : add motif detection to configure */

#ifdef __grwm__

#else

#define __grwm__

void x11_fullscreen(Display *dpy, Window w, int posx, int posy,int width, int height, int fs);

void x11_decoration(Display *dpy,Window w,int decorate);

void x11_sizehint(Display *mDisplay, Window window, 
                  int x, int y, int width, int height);
void x11_setlayer(Display * dpy, Window w, int fs_type, int fs);
int wm_detect(Display *dpy, Window w);
void init_atoms(Display * mDisplay);

#ifdef HAVE_MOTIF

#include <X11/Xm/MwmUtil.h>

#else

/* bit definitions for MwmHints.flags */
#define MWM_HINTS_FUNCTIONS	(1L << 0)
#define MWM_HINTS_DECORATIONS	(1L << 1)
#define MWM_HINTS_INPUT_MODE	(1L << 2)
#define MWM_HINTS_STATUS	(1L << 3)

/* bit definitions for MwmHints.functions */
#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)

/* bit definitions for MwmHints.decorations */
#define MWM_DECOR_ALL		(1L << 0)
#define MWM_DECOR_BORDER	(1L << 1)
#define MWM_DECOR_RESIZEH	(1L << 2)
#define MWM_DECOR_TITLE		(1L << 3)
#define MWM_DECOR_MENU		(1L << 4)
#define MWM_DECOR_MINIMIZE	(1L << 5)
#define MWM_DECOR_MAXIMIZE	(1L << 6)

typedef struct
{
    unsigned long	flags;
    unsigned long	functions;
    unsigned long	decorations;
    long 	        inputMode;
    unsigned long	status;
} PropMotifWmHints;

#define PROP_MOTIF_WM_HINTS_ELEMENTS	5

#endif

#endif
