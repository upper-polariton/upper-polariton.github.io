/*
 * $Id: gbutil.h,v 1.12 2002/02/28 21:55:49 spoel Exp $
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
 * Grunge ROck MAChoS
 */
static char *SRCID_gbutil_h = "$Id: gbutil.h,v 1.12 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

extern void rotate_conf(int natom,rvec *x,rvec *v,real alfa, real beta,real gamma);
/*rotate() rotates a configuration alfa degrees around the x_axis and beta degrees around the y_axis, *v can be NULL */

extern void orient(int natom,rvec *x,rvec *v, rvec angle,matrix box);
/*orient() rotates a configuration until the largest atom-atom distance is 
 *placed along the z-axis and the second largest distance is placed along
 *the y-axis. Finally the third longest distance is placed along the x-axis
 */

extern void genconf(t_atoms *atoms,rvec *x,rvec *v,real *r,matrix box,ivec n_box);
/*genconf() generates a new configuration by adding boxes*/
extern void gen_box(int NTB,int natoms,rvec *x, matrix box,rvec box_space,
		    bool bCenter);
/* gen_box() generates a box around a configuration, box_space is optional 
 * extra space around it. If NTB = 1 then a truncated octahedon will be 
 * generated (don't!) if bCenter then coordinates will be centered in the 
 * genereated box
 */
