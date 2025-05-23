/*
 * $Id: Xstuff.h,v 1.8 2002/02/28 11:07:08 spoel Exp $
 * 
 *                This source code is part of
 * 
 *                 G   R   O   M   A   C   S
 * 
 *          GROningen MAchine for Chemical Simulations
 * 
 *                        VERSION 3.1
 * Copyright (c) 1991-2001, University of Groningen, The Netherlands
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * If you want to redistribute modifications, please consider that
 * scientific software is very special. Version control is crucial -
 * bugs must be traceable. We will be happy to consider code for
 * inclusion in the official distribution, but derived work must not
 * be called official GROMACS. Details are found in the README & COPYING
 * files - if they are missing, get the official version at www.gromacs.org.
 * 
 * To help us fund GROMACS development, we humbly ask that you cite
 * the papers on the package - you can find them in the top README file.
 * 
 * For more info, check our website at http://www.gromacs.org
 * 
 * And Hey:
 * Glycine aRginine prOline Methionine Alanine Cystine Serine
 */

#ifndef _Xstuff_h
#define _Xstuff_h

static char *SRCID_Xstuff_h = "$Id: Xstuff.h,v 1.8 2002/02/28 11:07:08 spoel Exp $";
#ifdef HAVE_IDENT
#ident	"@(#) Xstuff.h 1.16 11/23/92"
#endif /* HAVE_IDENT */

/* The altivec extensions for ppc define some stupid overlapping
 * macros like pixel and vector - turn them off here, we only
 * need them in the inner loops.
 */
#ifdef __VEC__
#undef pixel
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include <X11/Xresource.h>

#ifdef _acc_
typedef unsigned char bmchar;
#else
typedef char bmchar;
#endif /* _acc */

#define XTextHeight(font) ((font)->max_bounds.ascent+(font)->max_bounds.descent)
#define XDrawCircle(disp,win,gc,x,y,rad) \
  XDrawArc(disp,win,gc,x-rad,y-rad,2*rad,2*rad,0,64*360)
#define XFillCircle(disp,win,gc,x,y,rad) \
  XFillArc(disp,win,gc,x-rad,y-rad,2*rad,2*rad,0,64*360)

#ifdef NEED_XSTUFF

extern	void	XSelectInput(Display *display,Window w,long event_mask);
extern	void	XChangeWindowAttributes(Display *display,Window w,
                                        unsigned long valuemask,
                                        XSetWindowAttributes *attributes);
extern  void    XSetWindowBackgroundPixmap(Display *disp,Drawable d,Pixmap pm);
extern	Status	XMatchVisualInfo(Display *display,int screen,int depth,
                                 int class,XVisualInfo *vinfo_return);
extern	Status	XParseColor(Display *display,Colormap colormap,char *spec,
                            XColor *exact_def_return);
extern	Status	XAllocColor(Display *display,Colormap colormap,
                            XColor *screen_in_out);
extern  Status  XAllocNamedColor(Display *disp, Colormap cmap, char *colorname,
				 XColor *colorcell_def, XColor *rgb_db_def);
extern  Status  XQueryColor(Display *disp, Colormap cmap, 
			    XColor *colorcell_def);
extern	void	XDrawArc(Display *display,Drawable d,GC gc,int x,int y,
                         unsigned int width,unsigned int height,int angle1,
                         int angle2);
extern	void	XFillArc(Display *display,Drawable d,GC gc,int x,int y,
                         unsigned int width,unsigned int height,int angle1,
                         int angle2);
extern	void	XDrawLine(Display *display,Drawable d,GC gc,int x1,int y1,
                          int x2,int y2);
extern	void	XDrawLines(Display *display,Drawable d,GC gc,XPoint *points,
			   int npoints, int mode);

extern	void	XMapWindow(Display *display,Window w);
extern	void	XMapSubwindows(Display *display,Window w);
extern  void    XUnmapSubwindows(Display *display, Window w);
extern  void    XUnmapWindow(Display *display, Window w);

extern	void	XNextEvent(Display *display,XEvent *report);
extern	Bool	XCheckTypedEvent(Display *display,int event_type,
                                 XEvent *event_return);
extern	void	XSetForeground(Display *display,GC gc,
                               unsigned long foreground);
extern  void    XClearWindow(Display *disp,Window w);
extern	void	XClearArea(Display *display,Window w,int x,int y,
                           unsigned int width,unsigned int height,
                           Bool exposures);
extern	void	XDrawRectangle(Display *display,Drawable d,GC gc,
                               int x, int y, int width, int height);
extern	void	XDrawRectangles(Display *display,Drawable d,GC gc,
                                XRectangle rectangles[],int nrectangles);
extern	void	XDrawString(Display *display,Drawable d,GC gc,int x,int y,
                            char *string,int length);
extern	void	XDrawLine(Display *display,Drawable d,GC gc,
                          int x1,int y1,int x2,int y2);
extern	void	XSetStandardProperties(Display *display,Window w,
                                       char *window_name,char *icon_name,
                                       Pixmap icon_pixmap,char **argv,int argc,
                                       XSizeHints *hints);
extern	int	XLookupString(XKeyEvent *event_struct,char *buffer_return,
                              int bytes_buffer,KeySym *keysym_return,
                              XComposeStatus *status_in_out);
extern	void	XSetGraphicsExposures(Display *display,GC gc,
                                      Bool graphics_exposures);
extern	void	XMapRaised(Display *display,Window w);
extern	void	XSync(Display *display,Bool discard);
extern	void	XFlush(Display *display);
extern	void	XSetStandardProperties(Display *display,Window w,
                                       char *window_name,char *icon_name,
                                       Pixmap icon_pixmap,char **argv,int argc,
                                       XSizeHints *hints);
extern	Status	XSendEvent(Display *display,Window w,Bool propagate,
                           long event_mask,XEvent *event_send);
extern	void	XFreeGC(Display *display,GC gc);
extern	void	XCloseDisplay(Display *display);

extern	void	XNextEvent(Display *display,XEvent *report);
extern	Bool	XCheckTypedEvent(Display *display,int event_type,
                                 XEvent *event_return);
extern	void	XSetForeground(Display *display,GC gc,
                               unsigned long foreground);
extern	Bool	XCheckMaskEvent(Display *display,long event_mask,
                                XEvent *event_return);

extern  int	XResizeWindow(Display *display, Window w, 
			      unsigned int width, unsigned int height);
extern  int     XMoveWindow(Display *display, Window w, 
			    unsigned int x, unsigned int y);
extern	void	XFreePixmap(Display *display,Pixmap pixmap);
extern	void	XCopyGC(Display *display, GC src, long valuemask,
			GC dest);
extern	int	XTextWidth(XFontStruct *font_struct, char *string, int count);
extern	void	XSetDashes(Display *display, GC gc, int dash_offset,
			   unsigned char dash_list[], int n);
extern  void    XBell(Display *disp,int volume);
extern  void    XDrawRectangle(Display *disp,Drawable d, GC gc,
			       int x,int y, int w,int h);
extern  void    XFillRectangle(Display *disp,Drawable d, GC gc,
			       int x,int y, int w,int h);

extern  void    XDestroySubwindows(Display *disp, Window Win);
extern  void    XDestroyWindow(Display *disp, Window Win);
extern  void    XQueryPointer(Display *disp,Window Win,Window *root, 
			      Window *child,int *root_x,int *root_y,
			      int *win_x,int *win_y, unsigned int *keybut);
extern  void	XWarpPointer(Display *disp, Window src, Window dest,
			     int src_x, int src_y, unsigned int src_w, unsigned int src_h,
			     int dest_x, int dest_y);
extern  void    XGetGeometry(Display *disp, Window w,Window *root, 
			     int *x, int *y, unsigned int *width, unsigned int *height,
			     unsigned int *border_width, unsigned int *depth);
#endif /* NEED_XSTUFF */

#endif	/* _Xstuff_h */
