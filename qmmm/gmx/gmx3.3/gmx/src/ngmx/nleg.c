/*
 * $Id: nleg.c,v 1.11 2002/02/28 11:07:09 spoel Exp $
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
static char *SRCID_nleg_c = "$Id: nleg.c,v 1.11 2002/02/28 11:07:09 spoel Exp $";
#include <ctype.h>
#include <string.h>
#include <smalloc.h>
#include <macros.h>
#include "buttons.h"
#include "nleg.h"
#include "writeps.h"

typedef struct {
  char  *tp;
  unsigned long *col;
  t_rgb rgb;
} t_atomcolor;

static t_atomcolor ac[] = {
  { "O",  &LIGHTRED,     { 1,  0,  0   } },
  { "N",  &LIGHTCYAN,    { 0,  0,  1   } },
  { "NA", &LIGHTGREY,    { 0.6,0.6,0.6 } },
  { "S",  &YELLOW,       { 1,  1,  0   } },
  { "C",  &LIGHTGREEN,   { 0,  1,  0   } },
  { "CL", &VIOLET,       { 1,  0,  1   } },
  { "F",  &LIGHTGREY,    { 0.6,0.6,0.6 } },
  { "Z",  &LIGHTGREY,    { 0.6,0.6,0.6 } },
  { "P",  &LIGHTBLUE,    { 0.4,0.4,1.0 } },
  { "H",  &WHITE,        { 0.8,0.8,0.8 } }
};
#define NAC asize(ac)

int search_ac(char *type)
{
  int i,nb,mij,best,besti;

  best=0;
  besti=0;
  if (type) {
    for(i=0; (i<NAC); i++) {
      mij=min((int)strlen(type),(int)strlen(ac[i].tp));
      for(nb=0; (nb<mij); nb++)
	if (type[nb] != ac[i].tp[nb])
	  break;
      if (nb > best) {
	best=nb;
	besti=i;
      }
    }
  }
  return besti;
}

unsigned long Type2Color(char *type)
{
  int i;
  
  i=search_ac(type);
  
  return *(ac[i].col);
}

t_rgb *Type2RGB(char *type)
{
  int i;
  
  i=search_ac(type);
  
  return &(ac[i].rgb);
}

void DrawLegend(t_x11 *x11,t_windata *Win)
{
#define NLAB 6
#define COLS 3
  static char *lab[NLAB] = { "C", "O", "H", "S", "N", "P" };
  int  i,i0,dh,dw,w,y,x1,x0;
  unsigned long cind;
  real h_2;
  
  XClearWindow(x11->disp, Win->self);
  w=Win->width;
  h_2=Win->height/(2.0*NLAB/COLS);
  dh=h_2-2;
  dw=dh;

  for (i=0; (i<NLAB); i++) {
    i0=i % (NLAB/COLS);
    x0=(i / (NLAB/COLS))*(Win->width/COLS)+AIR;
    x1=x0+2*dw+AIR;
    cind=Type2Color(lab[i]);
    XSetForeground(x11->disp,x11->gc,cind);
    y=((2*i0+1)*h_2);
    XFillRectangle (x11->disp,Win->self,x11->gc,x0,y-dh,2*dw,2*dh);
    XSetForeground(x11->disp,x11->gc,WHITE);
    TextInRect(x11,Win->self,lab[i],x1,y-dh,w-x1,2*dh,
	       eXLeft,eYCenter);
  }
  XSetForeground(x11->disp,x11->gc,x11->fg);
}

static bool LegWCallBack(t_x11 *x11,XEvent *event, Window w, void *data)
{
  t_legendwin *lw;

  lw=(t_legendwin *)data;
  switch(event->type) {
  case Expose:
    DrawLegend(x11,&lw->wd);
    break;
  default:
    break;
  }
  return FALSE;
}

t_legendwin *init_legw(t_x11 *x11,Window Parent,
		       int x,int y,int width,int height,
		       unsigned long fg,unsigned long bg)
{
  t_legendwin *lw;
  
  snew(lw,1);
  InitWin(&lw->wd,x,y,width,height,1,"Legend Window");
  lw->wd.self=XCreateSimpleWindow(x11->disp,Parent,x,y,1,1,1,fg,bg);
  x11->RegisterCallback(x11,lw->wd.self,Parent,LegWCallBack,lw);
  x11->SetInputMask(x11,lw->wd.self,ExposureMask);

  return lw;
}

void map_legw(t_x11 *x11,t_legendwin *lw)
{
  XMapWindow(x11->disp,lw->wd.self);
}


void done_legw(t_x11 *x11,t_legendwin *lw)
{
  x11->UnRegisterCallback(x11,lw->wd.self);
  sfree(lw);
}

