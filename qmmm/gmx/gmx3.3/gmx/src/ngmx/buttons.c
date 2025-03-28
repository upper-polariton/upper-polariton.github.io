/*
 * $Id: buttons.c,v 1.9 2002/02/28 11:07:08 spoel Exp $
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
 * Gromacs Runs On Most of All Computer Systems
 */
static char *SRCID_buttons_c = "$Id: buttons.c,v 1.9 2002/02/28 11:07:08 spoel Exp $";
#include <sysstuff.h>
#include <string.h>
#include <smalloc.h>
#include <macros.h>
#include <x11.h>
#include <xutil.h>
#include "buttons.h"
#include "stop_ani.bm"
#include "play.bm"
#include "ff.bm"
#include "rewind.bm"

static void move_bbox(t_x11 *x11,t_butbox *bbox)
{
  int       x0,y0;
  int       i,bw;
  real      idb,bh;
  t_windata *wd;

  bw=max(1,bbox->wd.width-2*(AIR+BORDER));
  idb=bbox->nbut;
  bh=(bbox->wd.height-AIR*(bbox->nbut+1));
  bh/=idb;
  bh=max(bh,1.0);

  x0=AIR;
  y0=AIR;
  for (i=0; (i<bbox->nbut); i++) {
    wd=&(bbox->b[i].wd);
    wd->width=bw;
    wd->height=bh;
    wd->color = WHITE;
    XMoveWindow(x11->disp,wd->self,x0,y0);
    XResizeWindow(x11->disp,wd->self,wd->width,wd->height);
    y0+=AIR+bh;
  }
}

static bool BBCallBack(t_x11 *x11,XEvent *event, Window w,void *data)
{
  t_butbox *bbox;

  if (event->type==ConfigureNotify) {
    bbox=(t_butbox *)data;
    bbox->wd.width=event->xconfigure.width;
    bbox->wd.height=event->xconfigure.height;
    move_bbox(x11,bbox);
  }
  return FALSE;
}

static bool VBCallBack(t_x11 *x11,XEvent *event, Window w,void *data)
{
  t_butbox *vbox;
  int        y0;

  if (event->type==Expose) {
    vbox=(t_butbox *)data;
    y0=XTextHeight(x11->font)+2*AIR+1;
    XSetForeground(x11->disp,x11->gc,WHITE);
    XClearArea(x11->disp,vbox->wd.self,1,1,vbox->wd.width-2,y0-1,False);
    TextInRect(x11,vbox->wd.self,vbox->wd.text,
	       1,1,vbox->wd.width-2,y0-1,eXLeft,eYCenter);
    XDrawLine(x11->disp,vbox->wd.self,x11->gc,0,y0,vbox->wd.width,y0);
    XSetForeground(x11->disp,x11->gc,x11->fg);
  }
  return FALSE;
}

void set_vbtime(t_x11 *x11,t_butbox *vbox,char *text)
{
  sfree(vbox->wd.text);
  vbox->wd.text=strdup(text);
  ExposeWin(x11->disp,vbox->wd.self);
}

static bool ButtonCallBack(t_x11 *x11,XEvent *event, Window w, void *data)
{
  XEvent    letter;
  t_mwbut   *but;
  t_windata *wd;

  but=(t_mwbut *)data;
  wd=&(but->wd);
  switch(event->type) {
  case Expose:
    XSetForeground(x11->disp,x11->gc,WHITE);
    XDrawRoundRect(x11->disp, wd->self, x11->gc,
		   0,0,wd->width-1,wd->height-1);    
    TextInWin(x11,wd,wd->text,eXCenter,eYCenter);
    XSetForeground(x11->disp,x11->gc,x11->fg);
    break;

  case EnterNotify:
    /*    LightBorder(x11->disp,wd->self,WHITE);*/
    XSetForeground(x11->disp,x11->gc,WHITE);
    XDrawRoundRect(x11->disp, wd->self, x11->gc,
		   1,1,wd->width-3,wd->height-3);    
    XSetForeground(x11->disp,x11->gc,x11->fg);
    break;
  case LeaveNotify:
    /*    LightBorder(x11->disp,wd->self,BLUE);*/
    XSetForeground(x11->disp,x11->gc,BLUE);
    XDrawRoundRect(x11->disp, wd->self, x11->gc,
		   1,1,wd->width-3,wd->height-3);    
    XSetForeground(x11->disp,x11->gc,x11->fg);

    break;

  case ButtonPress:
    letter.type=ClientMessage;
    letter.xclient.display=x11->disp;
    letter.xclient.window=wd->Parent;
    letter.xclient.message_type=0;
    letter.xclient.format=32;
    letter.xclient.data.l[0]=but->ID;
    letter.xclient.data.l[1]=(long)event->xbutton.button;
    XSendEvent(x11->disp,wd->Parent,True,0,&letter);
    break;
  default:
    break;
  }
  return FALSE;
}

t_butbox *init_vbox(t_x11 *x11,Window Parent,Window SendTo,unsigned long fg,unsigned long bg)
{
  Pixmap   pm;
  unsigned char     *data;
  t_butbox *vb;
  int      i,ID,x,y0;

  snew(vb,1);
  vb->nbut=IDNR-IDBUTNR-1;
  snew(vb->b,vb->nbut);

  /* VBox holder */
  y0=XTextHeight(x11->font)+2*AIR+2;
  InitWin(&vb->wd,0,0,vb->nbut*(play_width+AIR)+AIR,
	  y0+play_height+2*AIR,1,"VCR - Control");
  vb->wd.self=XCreateSimpleWindow(x11->disp,Parent,
				  vb->wd.x,vb->wd.y,vb->wd.width,vb->wd.height,
				  vb->wd.bwidth,WHITE,BLACK);
  x11->RegisterCallback(x11,vb->wd.self,Parent,VBCallBack,vb);
  x11->SetInputMask(x11,vb->wd.self,ExposureMask);
  
  x=AIR;
  (void)CWBackPixmap;
  for(i=0; (i<vb->nbut); i++) {
    ID=IDBUTNR+i+1;
    switch (ID) {
    case IDREWIND:
      data=&(rewind_bits[0]);
      break;
    case IDSTEP:
      data=play_bits;
      break;
    case IDFF:
      data=ff_bits;
      break;
    case IDSTOP_ANI:
      data=stop_ani_bits;
      break;
    default:
      fprintf(stderr,"Invalid bitmap in init_vbox %d\n",ID);
      exit(1);
    }
    /* Rely on the fact that all bitmaps are equal size */
    pm=XCreatePixmapFromBitmapData(x11->disp,x11->root,
				   (char *)data,play_width,play_height,
				   BLACK,LIGHTGREY,x11->depth);
    vb->b[i].ID=ID;
    vb->b[i].wd.Parent=SendTo;
    vb->b[i].wd.self=
      XCreateSimpleWindow(x11->disp,vb->wd.self,
			  x,y0+AIR,play_width,play_height,0,WHITE,BLACK);
    XSetWindowBackgroundPixmap(x11->disp,vb->b[i].wd.self,pm);
			       
    x11->RegisterCallback(x11,vb->b[i].wd.self,vb->wd.self,
			  ButtonCallBack,&(vb->b[i]));
    x11->SetInputMask(x11,vb->b[i].wd.self,
		      ButtonPressMask | StructureNotifyMask);
    x+=play_width+AIR;
  }
  
  return vb;
}

void show_but(t_x11 *x11,t_butbox *bbox)
{
  XMapWindow(x11->disp,bbox->wd.self);
  XMapSubwindows(x11->disp,bbox->wd.self);
}

void hide_but(t_x11 *x11,t_butbox *bbox)
{
  XUnmapWindow(x11->disp,bbox->wd.self);
  XUnmapSubwindows(x11->disp,bbox->wd.self);
}

t_butbox *init_bbox(t_x11 *x11,Window Parent,Window SendTo,
		    int width,unsigned long fg,unsigned long bg)
{
  t_butbox *bbox;
  static char *lbut[IDBUTNR] = {
    "< X-Rotate >", "< Y-Rotate >", "< Z-Rotate >", 
    "< X-Move >", "< Y-Move >", "< Z-Move >", "< Scale >", 
    };
  int       i,y0,h0;
  t_mwbut   *but;
  Window    DrawOn;

  snew(bbox,1);
  bbox->nbut=IDBUTNR;
  snew(bbox->b,bbox->nbut);
  y0=XTextHeight(x11->font)+2*(AIR+BORDER);
  
  InitWin(&(bbox->wd),0,0,/*width,(y0+AIR)*IDBUTNR+AIR+2*BORDER,*/1,1,
	  1,"Button Box");
  width-=2*AIR+2*BORDER;
  bbox->wd.self=XCreateSimpleWindow(x11->disp,Parent,
				    bbox->wd.x,bbox->wd.y,bbox->wd.width,
				    bbox->wd.height,bbox->wd.bwidth,
				    fg,bg);
  x11->RegisterCallback(x11,bbox->wd.self,Parent,BBCallBack,bbox);
  x11->SetInputMask(x11,bbox->wd.self,StructureNotifyMask);

  DrawOn=bbox->wd.self;
  h0=AIR;
  for (i=0; (i<bbox->nbut); i++) {
    but=&(bbox->b[i]);
    InitWin(&but->wd,AIR,h0,width,y0,1,lbut[i]);
    h0+=y0+AIR;
    but->wd.Parent=SendTo;
    but->ID=i;
    but->wd.self=XCreateSimpleWindow(x11->disp,DrawOn,
				     but->wd.x,but->wd.y,
				     but->wd.width,but->wd.height,
				     but->wd.bwidth,bg,bg);
    x11->RegisterCallback(x11,but->wd.self,DrawOn,ButtonCallBack,but);
    x11->SetInputMask(x11,but->wd.self,ExposureMask | ButtonPressMask |
		      EnterLeave);
  }
  return bbox;
}

void done_bbox(t_x11 *x11,t_butbox *bbox)
{
  int i;

  for(i=0; (i<bbox->nbut); i++)
    x11->UnRegisterCallback(x11,bbox->b[i].wd.self);
  x11->UnRegisterCallback(x11,bbox->wd.self);
  sfree(bbox->b);
  sfree(bbox);
}
