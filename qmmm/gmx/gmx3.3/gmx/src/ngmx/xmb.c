/*
 * $Id: xmb.c,v 1.7 2002/02/28 11:07:10 spoel Exp $
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
static char *SRCID_xmb_c = "$Id: xmb.c,v 1.7 2002/02/28 11:07:10 spoel Exp $";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "typedefs.h"
#include "macros.h"
#include "Xstuff.h"
#include "x11.h"
#include "xdlg.h"
#include "xmb.h"
#include "fatal.h"
#include "gromacs.bm"
#include "stop.bm"
#include "info.bm"
#include "alert.bm"

#define ID_BOX     -3
#define ID_ICON    -2
#define ID_TEXT    -1

static bmchar *icon_bits=NULL;
static int     icon_width=0;
static int     icon_height=0;
static unsigned long   icon_fg=0;
static unsigned long   icon_bg=0;

void SetIcon(unsigned char *bits, int w, int h, unsigned long fg, unsigned long bg)
{
  icon_bits=(bmchar *)bits;
  icon_width=w;
  icon_height=h;
  icon_fg=fg;
  icon_bg=bg;
}

t_dlg *MessageBox(t_x11 *x11, Window Parent, char *title,
		  int nlines, char *lines[], unsigned long Flags,
		  DlgCallback *cb, void *data)
{
  t_dlg         *dlg;
  int		width,nicon;
  int           x,y,x0;
  unsigned long         nFlag;
  unsigned long         bg;
  
  /* Check flags for inconsistencies */
  if (((Flags & MB_OK) && (Flags & MB_YES)) 	||
      ((Flags & MB_NO) && (Flags & MB_CANCEL))  ||
      (!(Flags & MB_OK) && !(Flags & MB_YES))) {
    fprintf(stderr,"Invalid button selection in MessageBox\n");
    exit(1);
  }
  nicon=0;
  if (Flags & MB_ICONSTOP) nicon++;
  if (Flags & MB_ICONINFORMATION) nicon++;
  if (Flags & MB_ICONEXCLAMATION) nicon++;
  if (Flags & MB_ICONGMX) nicon++;
  if (nicon > 1) 
    fatal_error(0,"More than one (%d) icon selected in MessageBox",nicon);
  /* Input seems ok */
  bg=x11->bg;
  if (nicon > 0) {
    if (Flags & MB_ICONSTOP)
      SetIcon(stop_bits,stop_width,stop_height,RED,bg);
    if (Flags & MB_ICONINFORMATION)
      SetIcon(info_bits,info_width,info_height,BLUE,bg);
    if (Flags & MB_ICONEXCLAMATION)
      SetIcon(alert_bits,alert_width,alert_height,GREEN,bg);
    if (Flags & MB_ICONGMX)
      SetIcon(gromacs_bits,gromacs_width,gromacs_height,BLUE,bg);
  }
  
  dlg=CreateDlg(x11,Parent,title,0,0,0,0,3,x11->fg,bg,cb,data);
  x=2*OFFS_X;
  if (nicon > 0) {
    AddDlgItem(dlg,CreatePixmap
	       (x11,XCreatePixmapFromBitmapData
		(x11->disp,dlg->win.self,icon_bits,icon_width,icon_height,
		 icon_fg,icon_bg,x11->depth),
		ID_ICON,ID_BOX,2*OFFS_X,2*OFFS_Y,icon_width,icon_height,0));
    x+=QueryDlgItemW(dlg,ID_ICON)+2*OFFS_X;
  }
  
  AddDlgItem(dlg,CreateStaticText(x11,nlines,lines,ID_TEXT,ID_BOX,
				  x,2*OFFS_Y,0,0,0));

  y=QueryDlgItemY(dlg,ID_TEXT)+QueryDlgItemH(dlg,ID_TEXT);
  if (nicon > 0) {
    int yi;
    yi=QueryDlgItemY(dlg,ID_ICON)+QueryDlgItemH(dlg,ID_ICON);
    if (yi > y)
      SetDlgItemPos(dlg,ID_TEXT,x,2*OFFS_Y+(yi-y)/2);
    else
      SetDlgItemPos(dlg,ID_ICON,2*OFFS_X,2*OFFS_Y+(y-yi)/2);
    y=max(y,yi);
  }
  x+=QueryDlgItemW(dlg,ID_TEXT)+2*OFFS_X;
  y+=2*OFFS_Y;
  width=(x-8*OFFS_X)/2;
  
  if (((Flags & MB_OKCANCEL) == MB_OKCANCEL) ||
      ((Flags & MB_YESNO) == MB_YESNO))
    x0=2*OFFS_X;
  else
    x0=(x-width)/2;

#define CB(name,butx,id) AddDlgItem(dlg,CreateButton(x11,name,\
						     TRUE,id,ID_BOX,\
						     butx,y,width,0,0))
  if (Flags & MB_OK) CB("OK",x0,MB_OK);
  if (Flags & MB_CANCEL) CB("Cancel",x/2+2*OFFS_X,MB_CANCEL);
  if (Flags & MB_YES) CB("Yes",x0,MB_YES);
  if (Flags & MB_NO) CB("No",x/2+2*OFFS_X,MB_NO);

  SetDlgSize(dlg,x,y+2*OFFS_Y+
	     QueryDlgItemH(dlg,(Flags & MB_OK) ? MB_OK : MB_YES),TRUE);

  if (Flags & MB_SYSTEMMODAL)
    nFlag=DLG_SYSTEMMODAL;
  else if (Flags & MB_APPLMODAL)
    nFlag=DLG_APPLMODAL;
  else
    nFlag=0;
  nFlag=nFlag | DLG_FREEONBUTTON;
  dlg->flags=nFlag;

  if (!(Flags & MB_DONTSHOW))
    ShowDlg(dlg);

  return dlg;
}
