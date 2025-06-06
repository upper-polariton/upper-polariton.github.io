/*
 * $Id: dialogs.h,v 1.7 2002/02/28 11:07:08 spoel Exp $
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

#ifndef _dialogs_h
#define _dialogs_h

static char *SRCID_dialogs_h = "$Id: dialogs.h,v 1.7 2002/02/28 11:07:08 spoel Exp $";
#include "xdlg.h"
#include "pulldown.h"
#include "manager.h"
#include "logo.h"

typedef struct {
  bool  bMapped;
  t_dlg *dlg;
} t_dialogs;

typedef enum { edExport, edBonds, edFilter, edNR } eDialogs;
	 
typedef enum {
  emQuit, emHelp, emAbout, emNotImplemented, emNR
} eMBoxes;

typedef enum {
  eExpGromos, eExpPDB, eExpNR
} eExport;

typedef struct {
  char         confout[256];	/* Export file			*/
  int          ExpMode;		/* Export mode			*/
  t_dlg        **dlgs;	        /* Temporary storage for dlgs	*/
  int          which_mb;        /* Which mb is visible          */
  t_dlg        **mboxes;        /* id for message boxes         */
  t_filter     *filter; 	/* Filter for visibility etc.	*/
  t_windata    *wd;		/* The main window		*/
  t_pulldown   *pd;		/* The pull-down menu		*/
  t_manager    *man;		/* The manager			*/
  /*t_statrec    *sr;*/		/* The statistics dlg		*/
  t_logo       *logo;           /* The gromacs logo             */
} t_gmx;

enum { 
  IDNEW,IDOPEN,IDOPENED,IDCLOSE,IDIMPORT,IDEXPORT,IDDOEXPORT,IDQUIT,IDTERM,
  IDEDITTOP,IDEDITCOORDS,IDEDITPARAMS,
  IDGROMPP,IDRUNMD,IDDOGROMPP,IDGSTAT,IDDOGSTAT,IDDORUNMD,
  IDFILTER,IDDOFILTER,
  IDANIMATE,IDSHOWBOX,IDRMPBC,IDHYDROGEN,IDLABELSOFF,IDRESETVIEW,IDPHOTO,
  IDDUMPWIN,IDDODUMP,
  IDBONDOPTS,IDTHIN,IDFAT,IDVERYFAT,IDBALLS,
  IDNOBOX,IDRECTBOX,IDTRIBOX,IDTOBOX,
  IDBOND,IDANGLE,IDDIH,IDRMS,IDRDF,IDENERGIES,IDCORR,
  IDHELP,IDABOUT,
  
  /* Last line specifies how many IDs there are */
  IDMENUNR
  };

extern void run_grompp(t_gmx *gmx);

extern void run_mdrun(t_gmx *gmx);

extern void write_gmx(t_x11 *x11,t_gmx *gmx,int mess);

/*extern void run_sr(t_statrec *sr);

extern t_statrec *init_sr();*/

extern void init_dlgs(t_x11 *x11,t_gmx *gmx);

extern void show_mb(t_gmx *gmx,int mb);

extern void done_dlgs(t_gmx *gmx);

extern void edit_file(char *fn);

extern t_filter *init_filter(t_atoms *atoms, char *fn, int natom_trx);

extern t_dlg *select_filter(t_x11 *x11,t_gmx *gmx);

#endif
