/*
 * $Id: dialogs.c,v 1.11 2002/02/28 11:07:08 spoel Exp $
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
static char *SRCID_dialogs_c = "$Id: dialogs.c,v 1.11 2002/02/28 11:07:08 spoel Exp $";
#include "smalloc.h"
#include "sysstuff.h"
#include "macros.h"
#include "string2.h"
#include "x11.h"
#include "xdlghi.h"
#include "xmb.h"
#include "dialogs.h"
#include "names.h"
#include "nmol.h"
#include "manager.h"
#include "futil.h"

#define MBFLAGS /* MB_APPLMODAL | */ MB_DONTSHOW

void write_gmx(t_x11 *x11,t_gmx *gmx,int mess)
{
  XEvent letter;

  letter.type=ClientMessage;
  letter.xclient.display=x11->disp;
  letter.xclient.window=gmx->wd->self;
  letter.xclient.message_type=0;
  letter.xclient.format=32;
  letter.xclient.data.l[0]=mess;
  letter.xclient.data.l[1]=Button1;
  XSendEvent(x11->disp,letter.xclient.window,True,0,&letter);
}

static void shell_comm(char *title,char *script,int nsleep)
{
  FILE *tfil;
  char command[STRLEN];
  char tmp[32];

  strcpy(tmp,"dialogXXXXXX");
  gmx_tmpnam(tmp);
  
  tfil = ffopen(tmp,"w");
  fprintf(tfil,"%s\n",script);
  fprintf(tfil,"sleep %d\n",nsleep);
  fclose(tfil);

  sprintf(command,"xterm -title %s -e sh %s",title,tmp);
#ifdef DEBUG
  fprintf(stderr,"command: %s\n",command);
#endif
  system(command);
#ifdef DEBUG
  unlink(tmp)
#endif
}

void show_mb(t_gmx *gmx,int mb)
{
  if (mb >=0 && mb < emNR) {
    gmx->which_mb=mb;
    ShowDlg(gmx->mboxes[mb]);
  }
}

static void hide_mb(t_gmx *gmx)
{
  if (gmx->which_mb > 0 && gmx->which_mb < emNR)
    HideDlg(gmx->mboxes[gmx->which_mb]);
}

static void MBCallback(t_x11 *x11,int dlg_mess,int item_id,
		       char *set,void *data)
{
  t_gmx *gmx;

  gmx=(t_gmx *)data;
  if (dlg_mess==DLG_EXIT) 
    hide_mb(gmx);
}

static t_dlg *about_mb(t_x11 *x11,t_gmx *gmx)
{
  static char *lines[]={
    "         G R O M A C S",
    " Machine for Simulating Chemistry",
    "       Copyright (c) 1992-2000",
    "  Dept. of Biophysical Chemistry",
    "    University of Groningen"
    };
  
  return MessageBox(x11,gmx->wd->self,gmx->wd->text,
		    asize(lines),lines,MB_OK | MB_ICONGMX | MBFLAGS,
		    MBCallback,gmx);
}

static void QuitCB(t_x11 *x11,int dlg_mess,int item_id,
		   char *set,void *data)
{
  t_gmx  *gmx;
  gmx=(t_gmx *)data;

  if (dlg_mess==DLG_EXIT) {
    hide_mb(gmx);
    if (strcasecmp("yes",set)==0) 
      write_gmx(x11,gmx,IDTERM);
  }
}

static t_dlg *quit_mb(t_x11 *x11,t_gmx *gmx)
{
  static char *lines[]={
    " Do you really want to Quit ?"
    };

  return MessageBox(x11,gmx->wd->self,gmx->wd->text,
		    asize(lines),lines,
		    MB_YESNO | MB_ICONSTOP | MBFLAGS,
		    QuitCB,gmx);
}

static t_dlg *help_mb(t_x11 *x11,t_gmx *gmx)
{
  static char *lines[]={
    " Help will soon be added"
    };
  
  return MessageBox(x11,gmx->wd->self,gmx->wd->text,
		    asize(lines),lines,
		    MB_OK | MB_ICONINFORMATION | MBFLAGS,
		    MBCallback,gmx);
}

static t_dlg *ni_mb(t_x11 *x11,t_gmx *gmx)
{
  static char *lines[]={
    " This feature has not been",
    " implemented yet."
    };
  
  return MessageBox(x11,gmx->wd->self,gmx->wd->text,
		    asize(lines),lines,
		    MB_OK | MB_ICONEXCLAMATION | MBFLAGS,
		    MBCallback,gmx);
}

enum { eExE, eExGrom, eExPdb, eExConf, eExNR };

static void ExportCB(t_x11 *x11,int dlg_mess,int item_id,
		     char *set,void *data)
{
  bool   bOk;
  t_gmx  *gmx;
  t_dlg  *dlg;

  gmx=(t_gmx *)data;
  dlg=gmx->dlgs[edExport];
  switch (dlg_mess) {
  case DLG_SET:
    switch (item_id) {
    case eExGrom:
      gmx->ExpMode=eExpGromos;
      break;
    case eExPdb:
      gmx->ExpMode=eExpPDB;
      break;
    default:
      break;
    }
#ifdef DEBUG
    fprintf(stderr,"exportcb: item_id=%d\n",item_id);
#endif
    break;
  case DLG_EXIT:
    if ((bOk=(strcasecmp("ok",set))==0)) 
      strcpy(gmx->confout,EditText(dlg,eExConf));
    HideDlg(dlg);
    if (bOk)
      write_gmx(x11,gmx,IDDOEXPORT);
    break;
  }
}

enum { eg0, egTOPOL, egCONFIN, egPARAM, eg1, eg1PROC, eg32PROC };

static void Extract(t_dlg *dlg,int ID,char *buf)
{
  char *et;
  
  et=EditText(dlg,ID);
  if (et)
    strcpy(buf,et);
}

enum bond_set { ebShowH=11, ebDPlus, ebRMPBC, ebCue, ebSkip, ebWait };

static void BondsCB(t_x11 *x11,int dlg_mess,int item_id,
		    char *set,void *data)
{
  static int ebond=-1;
  static int ebox=-1;
  bool   bOk,bBond=FALSE;
  int    nskip,nwait;
  t_gmx  *gmx;

  gmx=(t_gmx *)data;
  if (ebond==-1) {
    ebond = gmx->man->molw->bond_type;
    ebox  = gmx->man->molw->boxtype;
  }
  switch (dlg_mess) {
  case DLG_SET:
    if (item_id <= eBNR) {
      ebond=item_id-1;
      bBond=FALSE;
    }
    else if (item_id <= eBNR+esbNR+1) {
      ebox = item_id-eBNR-2;
      bBond=TRUE;
    }
    else {

#define DO_NOT(b) (b) = (!(b))

      switch (item_id) {
      case ebShowH:
	toggle_hydrogen(x11,gmx->man->molw);
	break;
      case ebDPlus:
	DO_NOT(gmx->man->bPlus);
#ifdef DEBUG
	fprintf(stderr,"gmx->man->bPlus=%s\n",bool_names[gmx->man->bPlus]);
#endif
	break;
	/*case ebSBox:
	set_box_type(x11,gmx->man->molw,ebond);
	break;*/
      case ebRMPBC:
	toggle_pbc(gmx->man);
	break;
      case ebCue:
	DO_NOT(gmx->man->bSort);
#ifdef DEBUG
	fprintf(stderr,"gmx->man->bSort=%s\n",bool_names[gmx->man->bSort]);
#endif
	break;
      case ebSkip:
	sscanf(set,"%d",&nskip);
#ifdef DEBUG
	fprintf(stderr,"nskip: %d frames\n",nskip);
#endif
	if (nskip >= 0)
	  gmx->man->nSkip=nskip;
	break;
      case ebWait:
	sscanf(set,"%d",&nwait);
#ifdef DEBUG
	fprintf(stderr,"wait: %d ms\n",nwait);
#endif
	if (nwait >= 0)
	  gmx->man->nWait=nwait;
      default:
#ifdef DEBUG
	fprintf(stderr,"item_id: %d, set: %s\n",item_id,set);
#endif
	break;
      }
    }
    break;
  case DLG_EXIT:
    bOk=(strcasecmp("ok",set)==0);
    HideDlg(gmx->dlgs[edBonds]);
    if (bOk) {
      if (bBond) {
	switch (ebond) {
	case eBThin:
	  write_gmx(x11,gmx,IDTHIN);
	  break;
	case eBFat:
	  write_gmx(x11,gmx,IDFAT);
	  break;
	case eBVeryFat:
	  write_gmx(x11,gmx,IDVERYFAT);
	  break;
	case eBSpheres:
	  write_gmx(x11,gmx,IDBALLS);
	  break;
	default:
	  fatal_error(0,"Invalid bond type %d at %s, %d",
		      ebond,__FILE__,__LINE__);
	}
      }
      else {
	switch(ebox) {
	case esbNone:
	  write_gmx(x11,gmx,IDNOBOX);
	  break;
	case esbRect:
	  write_gmx(x11,gmx,IDRECTBOX);
	  break;
	case esbTri:
	  write_gmx(x11,gmx,IDTRIBOX);
	  break;
	case esbTrunc:
	  write_gmx(x11,gmx,IDTOBOX);
	  break;
	default:
	  fatal_error(0,"Invalid box type %d at %s, %d",
		      ebox,__FILE__,__LINE__);
	}
      }
    }
    break;
  }
}

enum { esFUNCT=1, esBSHOW, esINFIL, esINDEXFIL, esLSQ, esSHOW, esPLOTFIL };

static bool in_set(int i,int n,int set[])
{
  int j;
  for(j=0; (j<n); j++)
    if (set[j]==i)
      return TRUE;
  return FALSE;
}

typedef t_dlg *t_mmb(t_x11 *x11,t_gmx *gmx);

typedef struct {
  eDialogs    ed;
  char        *dlgfile;
  DlgCallback *cb;
} t_dlginit;

typedef struct {
  eMBoxes     ed;
  t_mmb       *mmb;
  DlgCallback *cb;
} t_mbinit;

void init_dlgs(t_x11 *x11,t_gmx *gmx)
{
  static t_dlginit di[] = {
    { edExport,   "export.dlg",   ExportCB },
    { edBonds,    "bonds.dlg",    BondsCB  }
  };
  static t_mbinit mi[emNR] = {
    { emQuit, 		quit_mb,	QuitCB     },
    { emHelp,		help_mb,	MBCallback },
    { emAbout, 		about_mb,	MBCallback },
    { emNotImplemented,	ni_mb,		MBCallback }
  };
  int i;

  snew(gmx->dlgs,edNR);
  for(i=0; (i<asize(di)); i++)
    gmx->dlgs[i]=ReadDlg(x11,gmx->wd->self,di[i].dlgfile,
			 x11->fg,x11->bg,di[i].dlgfile, 
			 0,0,TRUE,FALSE,di[i].cb,gmx);

  gmx->dlgs[edFilter]=select_filter(x11,gmx);
  
  snew(gmx->mboxes,emNR);
  for(i=0; (i<emNR); i++)
    gmx->mboxes[i]=mi[i].mmb(x11,gmx);
}

void done_dlgs(t_gmx *gmx)
{
  int i;

  for(i=0; (i<edNR); i++)
    FreeDlg(gmx->dlgs[i]);
  for(i=0; (i<emNR); i++)
    FreeDlg(gmx->mboxes[i]);
}

void edit_file(char *fn)
{
  if (fork()==0) {
    char script[256];

    sprintf(script,"vi  %s",fn);
    shell_comm(fn,script,0);
    exit(0);
  }
}
