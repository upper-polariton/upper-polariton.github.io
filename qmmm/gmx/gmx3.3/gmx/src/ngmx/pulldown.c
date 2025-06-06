/*
 * $Id: pulldown.c,v 1.7 2002/02/28 11:07:09 spoel Exp $
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
static char *SRCID_pulldown_c = "$Id: pulldown.c,v 1.7 2002/02/28 11:07:09 spoel Exp $";
#include <string.h>
#include <smalloc.h>
#include <x11.h>
#include <macros.h>
#include "popup.h"
#include "pulldown.h"

static bool PDCallBack(t_x11 *x11,XEvent *event,Window w,void *data)
{
  t_pulldown *pd;
  int        i,x,x1,y,nsel;

  pd=(t_pulldown *)data;
  y=pd->wd.height;
  switch(event->type) {
  case Expose:
    XSetForeground(x11->disp,x11->gc,x11->fg);
    XDrawLine(x11->disp,w,x11->gc,0,y-1,pd->wd.width,y-1);
    for(i=0; (i<pd->nmenu); i++)
      XDrawString(x11->disp,pd->wd.self,x11->gc,pd->xpos[i],x11->font->ascent,
		  pd->title[i],strlen(pd->title[i]));
    break;
  case ButtonPress:
    if (pd->nsel==-1) {
      x=event->xbutton.x;
      for(nsel=0; (pd->xpos[nsel+1] < x) && (nsel < pd->nmenu-1); nsel++);
      pd->nsel=nsel;
      x1=max(0,min(pd_width(pd)-menu_width(pd->m[nsel]),pd->xpos[nsel]));
      show_menu(x11,pd->m[nsel],x1,y+1,FALSE);
    }
    break;
  case ButtonRelease:
    hide_pd(x11,pd);
    break;
  default:
    break;
  }
  return FALSE;
}

t_pulldown *init_pd(t_x11 *x11,Window Parent,int width,int height,
		    unsigned long fg,unsigned long bg,
		    int nmenu,int *nsub,t_mentry *ent[],char **title)
{
  t_pulldown *pd;
  int        i;

  snew(pd,1);
  pd->title=title;
  pd->nmenu=nmenu;
  pd->nsel=-1;
  snew(pd->m,nmenu);
  snew(pd->xpos,nmenu+1);
  pd->xpos[0]=5;
  for(i=1; (i<=nmenu); i++)
    pd->xpos[i]=20+pd->xpos[i-1]+
      XTextWidth(x11->font,title[i-1],strlen(title[i-1]));
  if (pd->xpos[nmenu] > width) 
    printf("Menu too wide\n");

  InitWin(&(pd->wd),0,0,width,XTextHeight(x11->font)+2,0,"PullDown");
  pd->wd.self=XCreateSimpleWindow(x11->disp,Parent,
				  pd->wd.x, pd->wd.y,
				  pd->wd.width,pd->wd.height,
				  pd->wd.bwidth,fg,bg);
  x11->RegisterCallback(x11,pd->wd.self,Parent,PDCallBack,pd);
  x11->SetInputMask(x11,pd->wd.self,ExposureMask | ButtonPressMask | 
		    OwnerGrabButtonMask | ButtonReleaseMask);
  XMapWindow(x11->disp,pd->wd.self);

  for(i=0; (i<nmenu); i++) 
    pd->m[i]=init_menu(x11,Parent,fg,bg,nsub[i],ent[i],1);

  return pd;
}

void hide_pd(t_x11 *x11,t_pulldown *pd)
{
  if (pd->nsel != -1)
    hide_menu(x11,pd->m[pd->nsel]);
  pd->nsel=-1;
}

void check_pd_item(t_pulldown *pd,int nreturn,bool bStatus)
{
  int i;

  for(i=0; (i<pd->nmenu); i++)
    check_menu_item(pd->m[i],nreturn,bStatus);
}

void done_pd(t_x11 *x11,t_pulldown *pd)
{
  int i;

  for(i=0; (i<pd->nmenu); i++)
    done_menu(x11,pd->m[i]);
  x11->UnRegisterCallback(x11,pd->wd.self);
  sfree(pd->m);
  sfree(pd->xpos);
}

int pd_width(t_pulldown *pd)
{
  int i,w;
  
  w=0;
  for(i=0; (i<pd->nmenu); i++)
    w=max(w,menu_width(pd->m[i]));
  w=max(w,pd->xpos[pd->nmenu]);
  return w;
}

int pd_height(t_pulldown *pd)
{
  int i,h;
  
  h=0;
  for(i=0; (i<pd->nmenu); i++)
    h=max(h,menu_height(pd->m[i]));

  return h;
}

