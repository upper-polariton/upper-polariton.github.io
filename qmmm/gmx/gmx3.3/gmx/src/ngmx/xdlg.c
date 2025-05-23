/*
 * $Id: xdlg.c,v 1.8 2002/02/28 11:07:09 spoel Exp $
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
static char *SRCID_xdlg_c = "$Id: xdlg.c,v 1.8 2002/02/28 11:07:09 spoel Exp $";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "string2.h"
#include "assert.h"
#include "macros.h"
#include "smalloc.h"
#include "Xstuff.h"
#include "xutil.h"
#include "xdlg.h"
#include "xmb.h"
#include "fatal.h"
/*****************************
 *
 * Helpful routines
 *
 ****************************/
t_dlgitem *FindItem(t_dlg *dlg, t_id id)
{
  int i;
  
  for(i=0; (i<dlg->nitem); i++)
    if (dlg->dlgitem[i]->ID==id)
      return dlg->dlgitem[i];
  return NULL;
}

t_dlgitem *FindWin(t_dlg *dlg, Window win)
{
  int i;
  
  for(i=0; (i<dlg->nitem); i++)
    if (dlg->dlgitem[i]->win.self==win)
      return dlg->dlgitem[i];
  return NULL;
}

/*****************************
 *
 * Routines to manipulate items on a dialog box
 *
 ****************************/
bool QueryDlgItemSize(t_dlg *dlg,t_id id,int *w,int *h)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL) {
    *w=dlgitem->win.width;
    *h=dlgitem->win.height;
    return TRUE;
  }
  return FALSE;
}

bool QueryDlgItemPos(t_dlg *dlg,t_id id,int *x0,int *y0)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL) {
    *x0=dlgitem->win.x;
    *y0=dlgitem->win.y;
    return TRUE;
  }
  return FALSE;
}

int QueryDlgItemX(t_dlg *dlg, t_id id)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL) 
    return dlgitem->win.x;
  return 0;
}

int QueryDlgItemY(t_dlg *dlg, t_id id)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL) 
    return dlgitem->win.y;
  return 0;
}

int QueryDlgItemW(t_dlg *dlg, t_id id)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL) 
    return dlgitem->win.width;
  return 0;
}

int QueryDlgItemH(t_dlg *dlg, t_id id)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL) 
    return dlgitem->win.height;
  return 0;
}

bool SetDlgItemSize(t_dlg *dlg,t_id id,int w,int h)
{
  t_dlgitem *dlgitem;
#ifdef DEBUG
  int old_w, old_h;
#endif

  if ((dlgitem=FindItem(dlg,id)) != NULL) {
#ifdef DEBUG
    old_w=dlgitem->win.width;
    old_h=dlgitem->win.height;
#endif
    if (w)
      dlgitem->win.width=w;
    if (h)
      dlgitem->win.height=h;
#ifdef DEBUG
    fprintf(dlg->x11->console,
	    "Size window from: %dx%d to %dx%d\n",old_w,old_h,
	    dlgitem->win.width,dlgitem->win.height);
    dlg->x11->Flush(dlg->x11);
#endif
    if (dlgitem->win.self)
      XResizeWindow(dlg->x11->disp,dlgitem->win.self,dlgitem->win.width,
		    dlgitem->win.height);
    if ((w) && (dlgitem->type==edlgGB)) {
      int i;
      t_id gid=dlgitem->GroupID;
      t_id id=dlgitem->ID;
      for (i=0; (i<dlg->nitem); i++) {
	t_dlgitem *child=dlg->dlgitem[i];
	if ((child->GroupID==gid) && (child->ID!=id))
	  SetDlgItemSize(dlg,child->ID,w-4*OFFS_X,0);
      }
    }
    return TRUE;
  }
  return FALSE;
}

bool SetDlgItemPos(t_dlg *dlg,t_id id,int x0,int y0)
{
  t_dlgitem *dlgitem;
  int old_x,old_y;

  if ((dlgitem=FindItem(dlg,id)) != NULL) {
    old_x=dlgitem->win.x;
    old_y=dlgitem->win.y;
    dlgitem->win.x=x0;
    dlgitem->win.y=y0;
#ifdef DEBUG
    fprintf(dlg->x11->console,
	    "Move window from: %d,%d to %d,%d\n",old_x,old_y,x0,y0);
    dlg->x11->Flush(dlg->x11);
#endif
    if (dlgitem->win.self)
      XMoveWindow(dlg->x11->disp,dlgitem->win.self,x0,y0);
    if (dlgitem->type==edlgGB) {
      int i,x,y;
      t_id gid=dlgitem->GroupID;
      t_id id=dlgitem->ID;
      x=dlgitem->win.x+2*OFFS_X-old_x;
      y=dlgitem->win.y+2*OFFS_Y-old_y;
      for (i=0; (i<dlg->nitem); i++) {
	t_dlgitem *child=dlg->dlgitem[i];
	if ((child->GroupID==gid) && (child->ID!=id))
	  SetDlgItemPos(dlg,child->ID,child->win.x+x,child->win.y+y);
      }
    }
    return TRUE;
  }
  return FALSE;
}

/*****************************
 *
 * Routines to extract information from the dlg proc
 * after dlg is exec'ed
 *
 ****************************/
bool IsCBChecked(t_dlg *dlg,t_id id)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL)
    if (dlgitem->type==edlgCB)
      return dlgitem->u.checkbox.bChecked;
  
  return FALSE;
}

t_id RBSelected(t_dlg *dlg,int gid)
{
  int i;
  
  for(i=0; (i<dlg->nitem); i++)
    if ((dlg->dlgitem[i]->type==edlgRB) && 
	(dlg->dlgitem[i]->u.radiobutton.bSelect) &&
	(dlg->dlgitem[i]->GroupID==gid))
      return dlg->dlgitem[i]->ID;
  
  return -1;
}

int EditTextLen(t_dlg *dlg,t_id id)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL)
    if (dlgitem->type==edlgET)
      return strlen(dlgitem->u.edittext.buf);
  
  return 0;
}

char *EditText(t_dlg *dlg,t_id id)
{
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindItem(dlg,id)) != NULL)
    if (dlgitem->type==edlgET)
      return dlgitem->u.edittext.buf;
  
  return NULL;
}

/*****************************
 *
 * Exececute the dialog box procedure
 * Returns when a button is pushed.
 * return value is the ID of the button
 *
 ****************************/
void ShowDlg(t_dlg *dlg)
{
  int i;
  t_dlgitem *dlgitem;

  XMapWindow(dlg->x11->disp,dlg->win.self);
  XMapSubwindows(dlg->x11->disp,dlg->win.self);
  for (i=0; (i<dlg->nitem); i++)
    LightBorder(dlg->x11->disp,dlg->dlgitem[i]->win.self,dlg->bg);
  XSetForeground(dlg->x11->disp,dlg->x11->gc,dlg->x11->fg);
  for(i=0; (i<dlg->nitem); i++) {
    dlgitem=dlg->dlgitem[i];
    if ((dlgitem->type==edlgBN) &&
	(dlgitem->u.button.bDefault)) {
      PushMouse(dlg->x11->disp,dlgitem->win.self,
		dlgitem->win.width/2,dlgitem->win.height/2);
      dlg->bPop=TRUE;
      break;
    }
  }
  dlg->bGrab=FALSE;
}

void HideDlg(t_dlg *dlg)
{
  if (dlg->bPop)
    PopMouse(dlg->x11->disp);

  XUnmapSubwindows(dlg->x11->disp,dlg->win.self);
  XUnmapWindow(dlg->x11->disp,dlg->win.self);
}

void NoHelp(t_dlg *dlg)
{
  char **lines=NULL;

  snew(lines,2);
  lines[0]=strdup("Error");
  lines[1]=strdup("No help for this item");
  MessageBox(dlg->x11,dlg->wDad,"No Help",2,lines,
	     MB_OK | MB_ICONSTOP | MB_APPLMODAL,NULL,NULL);
  sfree(lines[0]); 
  sfree (lines[1]); 
  sfree(lines);
}

void HelpDlg(t_dlg *dlg)
{
  char *lines[] = {
    "Place the cursor over one of the items",
    "and press the F1 key to get more help.",
    "First press the OK button."
  };
  MessageBox(dlg->x11,dlg->win.self,"Help Dialogbox",
	     3,lines,MB_OK | MB_ICONINFORMATION | MB_APPLMODAL,NULL,NULL);
}

void HelpNow(t_dlg *dlg, t_dlgitem *dlgitem)
{
  char buf[80];
  bool bCont=TRUE;
  int  i,nlines=0;
  char **lines=NULL;

  if (!dlgitem->help) {
    NoHelp(dlg);
    return;
  }

  printf("%s\n",dlgitem->help);
  do {
    fgets2(buf,79,stdin);
#ifdef DEBUG
    fprintf(dlg->x11->console,"buffer: '%s'\n",buf);
    dlg->x11->Flush(dlg->x11);
#endif
    if (strcasecmp(buf,"nok")==0) {
      /* An error occurred */
      for(i=0; (i<nlines); i++)
	sfree(lines[i]);
      sfree(lines);
      NoHelp(dlg);
      return;
    }
    else {
      bCont=(strcasecmp(buf,"ok") != 0);
      if (bCont) {
	srenew(lines,++nlines);
	lines[nlines-1]=strdup(buf);
      }
    }
  } while (bCont);
  MessageBox(dlg->x11,dlg->wDad,"Help",
	     nlines,lines,MB_OK | MB_ICONINFORMATION | MB_APPLMODAL,NULL,NULL);
  for(i=0; (i<nlines); i++)
    sfree(lines[i]);
  sfree(lines);
}

static void EnterDlg(t_dlg *dlg)
{
  if (dlg->flags & DLG_APPLMODAL)
    dlg->bGrab=GrabOK(dlg->x11->console,
		      XGrabPointer(dlg->x11->disp,dlg->win.self,
				   True,0,GrabModeAsync,GrabModeAsync,
				   dlg->win.self,None,CurrentTime));
  dlg->x11->Flush(dlg->x11);
}

static void ExitDlg(t_dlg *dlg)
{
  if (dlg->bGrab) {
    XUngrabPointer(dlg->x11->disp,CurrentTime);
    dlg->bGrab=FALSE;
  }
  HideDlg(dlg);
  if (dlg->flags & DLG_FREEONBUTTON)
    FreeDlg(dlg);
}

static bool DlgCB(t_x11 *x11,XEvent *event, Window w, void *data)
{
  t_dlg     *dlg=(t_dlg *)data;
  int	    i,nWndProc;
  t_dlgitem *dlgitem;
  
  if ((dlgitem=FindWin(dlg,w))!=NULL) {
    nWndProc=(dlgitem->WndProc)(x11,dlgitem,event);
#ifdef DEBUG
    fprintf(x11->console,
	    "window: %s, nWndProc: %d\n",dlgitem->win.text,nWndProc);
    x11->Flush(x11);
#endif
    switch (nWndProc) {
    case ENTERPRESSED:
      if ((dlgitem->type==edlgBN) && (dlgitem->u.button.bDefault)) {
	if (dlg->cb)
	  dlg->cb(x11,DLG_EXIT,dlgitem->ID,dlgitem->win.text,dlg->data);
	else
	  ExitDlg(dlg);
      }
      else {
	for(i=0; (i<dlg->nitem); i++)
	  if ((dlg->dlgitem[i]->type==edlgBN) && 
	      (dlg->dlgitem[i]->u.button.bDefault)) {
	    PushMouse(x11->disp,dlg->dlgitem[i]->win.self,
		      dlg->dlgitem[i]->win.width/2,
		      dlg->dlgitem[i]->win.height/2);
	    break;
	  }
      }
      break;
    case BNPRESSED:
      if (dlg->cb)
	dlg->cb(x11,DLG_EXIT,dlgitem->ID,dlgitem->win.text,dlg->data);
      else 
	ExitDlg(dlg);
      break;
    case RBPRESSED: {
      int  gid=dlgitem->GroupID;
      t_id tid=RBSelected(dlg,gid);
#ifdef DEBUG
      fprintf(stderr,"RBPRESSED\n");
#endif
      if (tid != -1) {
	t_dlgitem *dit=FindItem(dlg,tid);
	dit->u.radiobutton.bSelect=FALSE;
	ExposeWin(x11->disp,dit->win.self);
      }
      else
	fatal_error(0,"No RB Selected initially!\n");
      dlgitem->u.radiobutton.bSelect=TRUE;
      ExposeWin(x11->disp,dlgitem->win.self);
      if (dlg->cb)
	dlg->cb(x11,DLG_SET,dlgitem->ID,dlgitem->win.text,dlg->data);
      break;
    }
    case CBPRESSED:
      ExposeWin(x11->disp,dlgitem->win.self);
      if (dlg->cb)
	dlg->cb(x11,DLG_SET,dlgitem->ID,dlgitem->set,dlg->data);
      break;
    case ETCHANGED:
      ExposeWin(x11->disp,dlgitem->win.self);
      if (dlg->cb)
	dlg->cb(x11,DLG_SET,dlgitem->ID,dlgitem->u.edittext.buf,dlg->data);
      break;
    case HELPPRESSED:
      HelpNow(dlg,dlgitem);
      break;
    case ITEMOK:
      break;
    default:
      fatal_error(0,"Invalid return code (%d) from wndproc\n",nWndProc);
    }
  }
  else if (w==dlg->win.self) {
    switch(event->type) {
    case Expose:
      EnterDlg(dlg);
      break;
    case ButtonPress:
    case KeyPress:
      if (HelpPressed(event))
	HelpDlg(dlg);
      else
	XBell(x11->disp,50);
      break;
    default:
      break;
    }
  }
  return FALSE;
}

/*****************************
 *
 * Routine to add an item to the dialog box
 * The pointer to the item is copied to the dlg struct,
 * the item itself may not be freed until the dlg is done with
 *
 ****************************/
void DoCreateDlg(t_dlg *dlg)
{
  XSizeHints           hints;
  XSetWindowAttributes attr;
  unsigned long Val;

  attr.border_pixel=dlg->x11->fg;
  attr.background_pixel=dlg->bg;
  attr.override_redirect=False;
  attr.save_under=True;
  attr.cursor=XCreateFontCursor(dlg->x11->disp,XC_hand2);
  Val=CWBackPixel | CWBorderPixel | CWOverrideRedirect | CWSaveUnder |
    CWCursor;
  dlg->win.self=XCreateWindow(dlg->x11->disp,dlg->wDad,
			      dlg->win.x,dlg->win.y,
			      dlg->win.width,dlg->win.height,
			      dlg->win.bwidth,CopyFromParent,
			      InputOutput,CopyFromParent,
			      Val,&attr);
  dlg->x11->RegisterCallback(dlg->x11,dlg->win.self,dlg->wDad,
			     DlgCB,dlg);
  dlg->x11->SetInputMask(dlg->x11,dlg->win.self, 
			 ExposureMask | ButtonPressMask | KeyPressMask);

  if (!CheckWindow(dlg->win.self))
    exit(1);
  hints.x=dlg->win.x;
  hints.y=dlg->win.y;
  hints.flags=PPosition;
  XSetStandardProperties(dlg->x11->disp,dlg->win.self,dlg->title,
			 dlg->title,None,NULL,0,&hints);
}

void AddDlgItem(t_dlg *dlg, t_dlgitem *new)
{
#define EnterLeaveMask (EnterWindowMask | LeaveWindowMask)
#define UserMask (ButtonPressMask | KeyPressMask)
  static unsigned long InputMask[edlgNR] = {
    ExposureMask | UserMask | EnterLeaveMask, /* edlgBN */ 
    ExposureMask | UserMask | EnterLeaveMask, /* edlgRB */ 
    ExposureMask,                             /* edlgGB */
    ExposureMask | UserMask | EnterLeaveMask, /* edlgCB */ 
    0,                                        /* edlgPM */ 
    ExposureMask,                             /* edlgST */ 
    ExposureMask | UserMask | EnterLeaveMask  /* edlgET */ 
  };
  
  if (!dlg->win.self)
    DoCreateDlg(dlg);
  srenew(dlg->dlgitem,dlg->nitem+1);
  assert(new);
  new->win.self=
    XCreateSimpleWindow(dlg->x11->disp,dlg->win.self,new->win.x,new->win.y,
			new->win.width,new->win.height,
			new->win.bwidth,dlg->x11->fg,dlg->x11->bg);
  CheckWindow(new->win.self);

  dlg->x11->RegisterCallback(dlg->x11,new->win.self,dlg->win.self,
			     DlgCB,dlg);  
  dlg->x11->SetInputMask(dlg->x11,new->win.self,InputMask[new->type]);

  switch (new->type) {
  case edlgPM:
    XSetWindowBackgroundPixmap(dlg->x11->disp,new->win.self,new->u.pixmap.pm);
    break;
  default:
    break;
  }
  dlg->dlgitem[dlg->nitem]=new;

  dlg->nitem++;
}

void AddDlgItems(t_dlg *dlg,int nitem,t_dlgitem *new[])
{
  int i;
  
  for(i=0; (i<nitem); i++) {
#ifdef DEBUG
    fprintf(dlg->x11->console,
	    "Adding item: %d from group %d\n",new[i]->ID,new[i]->GroupID);
    dlg->x11->Flush(dlg->x11);
#endif
    AddDlgItem(dlg,new[i]);
  }
}

void FreeDlgItem(t_dlg *dlg, t_id id)
{
  t_dlgitem *dlgitem;
  int i;

  if ((dlgitem=FindItem(dlg,id)) != NULL) {
    dlg->x11->UnRegisterCallback(dlg->x11,dlgitem->win.self);
    if (dlgitem->win.self)
      XDestroyWindow(dlg->x11->disp,dlgitem->win.self);
    FreeWin(dlg->x11->disp,&(dlgitem->win));
    switch(dlgitem->type) {
    case edlgBN:
    case edlgRB: 
      break;
    case edlgGB:
      sfree(dlgitem->u.groupbox.item);
      break;
    case edlgCB:
      break;
    case edlgPM:
      XFreePixmap(dlg->x11->disp,dlgitem->u.pixmap.pm);
      break;
    case edlgST:
      for (i=0; (i<dlgitem->u.statictext.nlines); i++)
	sfree(dlgitem->u.statictext.lines[i]);
      sfree(dlgitem->u.statictext.lines);
      break;
    case edlgET:
      sfree(dlgitem->u.edittext.buf);
      break;
    default:
      break;
    }
  }
}

void FreeDlg(t_dlg *dlg)
{
  int i;

  if (dlg->dlgitem) {
    HideDlg(dlg);
    dlg->x11->UnRegisterCallback(dlg->x11,dlg->win.self);
    for(i=0; (i<dlg->nitem); i++) {
      FreeDlgItem(dlg,dlg->dlgitem[i]->ID);
      if (dlg->dlgitem[i])
	sfree(dlg->dlgitem[i]);
    }
    sfree(dlg->dlgitem);
    if (dlg->win.self)
      XDestroyWindow(dlg->x11->disp,dlg->win.self);
    dlg->dlgitem=NULL;
  }
}

/*****************************
 *
 * Routine to create the DLG structure, returns NULL on failure
 *
 ****************************/
t_dlg *CreateDlg(t_x11 *x11, Window Parent, char *title,
		 int x0,int y0,int w,int h,int bw, unsigned long fg, unsigned long bg,
		 DlgCallback *cb,void *data)
{
  t_dlg   *dlg;
  int     x=0,y=0;
  
  snew(dlg,1);
  dlg->x11=x11;
  dlg->cb=cb;
  dlg->data=data;
  if (title)
    dlg->title=strdup(title);
  else
    dlg->title=NULL;
  if (w==0) w=1;
  if (h==0) h=1;
  if (!Parent) {
    Parent=x11->root;
    dlg->xmax=DisplayWidth(x11->disp,x11->screen);
    dlg->ymax=DisplayHeight(x11->disp,x11->screen);
  }
  else {
    Window root;
    unsigned int   dum;

    XGetGeometry(x11->disp,Parent,&root,&x,&y,
		 &(dlg->xmax),&(dlg->ymax),&dum,&dum);
#ifdef DEBUG
    fprintf(x11->console,
	    "Daddy is %d x %d at %d, %d\n",dlg->xmax,dlg->ymax,x,y);
    dlg->x11->Flush(dlg->x11);
#endif
  }
  if (x0) x=x0;
  if (y0) y=y0;
  InitWin(&(dlg->win),x,y,w,h,bw,NULL);
  SetDlgSize(dlg,w,h,x0 || y0);

  dlg->wDad=Parent;
  dlg->fg=x11->fg;
  dlg->bg=x11->bg;
  dlg->nitem=0;
  dlg->dlgitem=NULL;

  DoCreateDlg(dlg);
  return dlg;
}

void SetDlgSize(t_dlg *dlg,int w,int h, bool bAutoPosition)
{
  if (bAutoPosition) {
    int x,y;
    
    x=(dlg->xmax-w)/2;
    y=(dlg->ymax-h)/2;
    dlg->win.x=x;
    dlg->win.y=y;
  }
  dlg->win.width=w;
  dlg->win.height=h;
  
#ifdef DEBUG
  fprintf(dlg->x11->console,"SetDlgSize: Dialog is %dx%d, at %d,%d\n",
	  dlg->win.width,dlg->win.height,dlg->win.x,dlg->win.y);
  dlg->x11->Flush(dlg->x11);
#endif
  if (dlg->win.self) {
    XMoveWindow(dlg->x11->disp,dlg->win.self,dlg->win.x,dlg->win.y);
    XResizeWindow(dlg->x11->disp,dlg->win.self,w,h);
  }
}

