/*
 * $Id: xdlgitem.h,v 1.6 2002/02/28 11:07:10 spoel Exp $
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

#ifndef _xdlgitem_h
#define _xdlgitem_h

static char *SRCID_xdlgitem_h = "$Id: xdlgitem.h,v 1.6 2002/02/28 11:07:10 spoel Exp $";
#ifdef HAVE_IDENT
#ident	"@(#) xdlgitem.h 1.4 9/29/92"
#endif /* HAVE_IDENT */

#include <typedefs.h>
#include <Xstuff.h>
#include <xutil.h>
#include <x11.h>

#define XCARET 	2

enum { 
  ITEMOK, RBPRESSED, BNPRESSED, CBPRESSED, ETCHANGED, HELPPRESSED, ENTERPRESSED
};

typedef int t_id;

typedef struct {
  bool bDefault;	/* This is the default button */
} t_button;

typedef struct {
  bool bSelect;   	/* Is this rb selected ? */
} t_radiobutton;

typedef struct {
  bool bChecked;	/* Is this cb checked ? */
} t_checkbox;

typedef struct {
  Pixmap pm;		/* The pixmap bits */
} t_pixmap;

typedef struct {
  int  nlines;
  char **lines;
} t_statictext;

typedef struct {
  int  buflen,strbegin; /* Length of the screen buf and begin of string  */
  int  pos/*,len*/;	/* Current length of the string and pos of caret */
                        /* Pos is relative to strbegin, and is the pos   */
                        /* in the window.                                */
  bool bChanged;
  char *buf;
} t_edittext;

typedef struct {
  int  nitems;
  t_id *item;
} t_groupbox;

typedef enum {
  edlgBN, edlgRB, edlgGB, edlgCB, edlgPM, edlgST, edlgET, edlgNR
} edlgitem;

typedef struct t_dlgitem {
  t_windata     win;
  t_id		ID,GroupID;
  bool          bUseMon;
  char          *set,*get,*help;
  edlgitem      type;
  int		(*WndProc)(t_x11 *x11,struct t_dlgitem *dlgitem,XEvent *event);
  union {
    t_button 	  button;
    t_radiobutton radiobutton;
    t_groupbox    groupbox;
    t_checkbox    checkbox;
    t_pixmap      pixmap;
    t_statictext  statictext;
    t_edittext    edittext;
  } u;
} t_dlgitem;

/*****************************
 *
 * Routines to create dialog items, all items have an id
 * which you can use to extract info. It is possible to have
 * multiple items with the same id but it may then not be possible
 * to extract information.
 * All routines take the position relative to the parent dlg
 * and the size and border width.
 * If the width and height are set to zero initially, they will
 * be calculated and set by the routine. With the dlgitem manipulation
 * routines listed below, the application can then move the items around
 * on the dlg box, and if wished resize them.
 *
 ****************************/
extern t_dlgitem *CreateButton(t_x11 *x11, char *szLab,bool bDef,
			       t_id id,t_id groupid,
			       int x0,int y0,int w,int h,int bw);

extern t_dlgitem *CreateRadioButton(t_x11 *x11,
				    char *szLab,bool bSet,t_id id,
				    t_id groupid,
				    int x0,int y0,int w,int h,int bw);

extern t_dlgitem *CreateGroupBox(t_x11 *x11,char *szLab,t_id id,
				 int nitems, t_id items[],
				 int x0,int y0,int w,int h,int bw);

extern t_dlgitem *CreateCheckBox(t_x11 *x11,char *szLab,
				 bool bCheckedInitial,
				 t_id id,t_id groupid,
				 int x0,int y0,int w,int h,int bw);

extern t_dlgitem *CreatePixmap(t_x11 *x11,Pixmap pm,t_id id,t_id groupid,
			       int x0,int y0,int w,int h,int bw);

extern t_dlgitem *CreateStaticText(t_x11 *x11,
				   int nlines,char **lines,t_id id,
				   t_id groupid,
				   int x0,int y0,int w,int h,int bw);

extern t_dlgitem *CreateEditText(t_x11 *x11,char *title, 
				 int screenbuf,char *buf, t_id id,t_id groupid,
				 int x0,int y0,int w,int h,int bw);

extern void SetDlgitemOpts(t_dlgitem *dlgitem,bool bUseMon,
			   char *set, char *get, char *help);

#endif	/* _xdlgitem_h */
