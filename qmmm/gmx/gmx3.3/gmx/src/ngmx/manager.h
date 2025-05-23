/*
 * $Id: manager.h,v 1.12 2002/02/28 11:07:09 spoel Exp $
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

#ifndef _manager_h
#define _manager_h

static char *SRCID_manager_h = "$Id: manager.h,v 1.12 2002/02/28 11:07:09 spoel Exp $";
#include <stdio.h>
#include "typedefs.h"
#include "x11.h"
#include "xutil.h"
#include "3dview.h"
#include "nleg.h"
#include "buttons.h"

/* Some window sizes */
#define EWIDTH  	200
#define EHEIGHT 	  0
#define LDHEIGHT  	  0
#define LEGHEIGHT 	 60

typedef enum { eOSingle, eOBond, eOHBond, eONR } eObject;

typedef enum { eVNormal, eVSpecial, eVHidden, evNR } eVisible;

enum { eBThin, eBFat, eBVeryFat, eBSpheres, eBNR };

enum { esbNone, esbRect, esbTri, esbTrunc, esbNR };

typedef struct {
  t_windata wd;			/* Mol window structure			*/
  bool      bShowHydrogen;	/* Show Hydrogens?			*/
  int       bond_type;		/* Show one of the above bondtypes      */
  int       boxtype;            /* Rectangular, Tric, TruncOct (display)*/
  int       realbox;            /* Property of the real box             */
} t_molwin;

typedef struct {
  eObject 	eO;		/* The type of object			*/
  eVisible	eV;		/* Visibility status of the object	*/
  unsigned long   	color;		/* The color (only when eV==evSpecial) 	*/
  atom_id 	ai,aj;		/* The atom_id for i (and j if bond)	*/
  real    	z;		/* The Z-coordinate for depht cueing	*/
} t_object;

typedef struct {
  t_block *grps;		/* Blocks with atom numbers		*/
  char    **grpnames;		/* The names of the groups		*/
  bool    *bDisable;            /* Group indexes out of natoms in TRX   */
  bool    *bShow;		/* Show a group ?			*/
} t_filter;

/*
 * t_manager structure:
 *
 * This structure manages the display area for the gmx program.
 * It reads the status file and sends messages when windows need to
 * be updated.
 *
 */
typedef struct {
  int       status;
  char      *trajfile;
  int       natom;		/* The number of atoms			*/
  t_topology top;               /* topology                             */
  rvec      box_size;
  int       step;               /* The actual step number               */
  real      time;               /* The actual time                      */
  rvec      *x;			/* The coordinates			*/
  iv2       *ix;		/* The coordinates after projection	*/
  real      *zz;                /* Z-coords                             */
  matrix    box;		/* The box				*/
  int       nobj;		/* The number of objects		*/
  t_object  *obj;		/* The objects on screen		*/
  bool      *bHydro;		/* TRUE for hydrogen atoms		*/
  bool      *bLabel;            /* Show a label on atom i?              */
  char      **szLab;            /* Array of pointers to labels          */
  unsigned long *col;		/* The colour of the atoms		*/
  int       *size;		/* The size of the atoms		*/
  real      *vdw;		/* The VDWaals radius of the atoms	*/
  bool      *bVis;              /* visibility of atoms                  */
  bool      bPbc;               /* Remove Periodic boundary             */
  bool      bAnimate;		/* Animation going on?			*/
  bool      bEof;               /* End of file reached?                 */
  bool      bStop;              /* Stopped by user?                     */
  bool      bSort;		/* Sort the coordinates			*/
  bool      bPlus;		/* Draw plus for single atom		*/
  int       nSkip;		/* Skip n steps after each frame	*/
  int       nWait;		/* Wait n ms after each frame           */

  t_windata   wd;               /* The manager subwindow                */
  t_windata   title;		/* Title window				*/
  t_3dview    *view;            /* The 3d struct                        */
  t_molwin    *molw;		/* The molecule window			*/
  t_butbox    *vbox;		/* The video box			*/
  t_butbox    *bbox;		/* The button box			*/
  t_legendwin *legw;		/* The legend window			*/
} t_manager;

extern t_manager *init_man(t_x11 *x11,Window Parent,
			   int x,int y,int width,int height,
			   unsigned long fg,unsigned long bg,matrix box);
/* Initiate the display manager */

extern void move_man(t_x11 *x11,t_manager *man,int width,int height);
/* Set the right size for this window */

extern void step_message(t_x11 *x11,t_manager *man);
/* Send a message to the manager */

extern void set_file(t_x11 *x11,t_manager *man,char *trajectory,char *status);
/* Read a new trajectory and topology */

extern void map_man(t_x11 *x11,t_manager *man);

extern void move_man(t_x11 *x11,t_manager *man,int width,int height);

extern bool toggle_animate (t_x11 *x11,t_manager *man);

extern bool toggle_pbc (t_manager *man);

extern void no_labels(t_x11 *x11,t_manager *man);
/* Turn off all labels */

extern void done_man(t_x11 *x11,t_manager *man);
/* Clean up man struct */

extern void draw_mol(t_x11 *x11,t_manager *man);

extern void create_visibility(t_manager *man);

extern void do_filter(t_x11 *x11,t_manager *man,t_filter *filter);

#endif
