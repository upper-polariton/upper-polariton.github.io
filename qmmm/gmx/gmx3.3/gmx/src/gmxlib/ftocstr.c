/*
 * $Id: ftocstr.c,v 1.8 2002/02/28 10:49:22 spoel Exp $
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
 * Great Red Owns Many ACres of Sand 
 */
static char *SRCID_ftocstr_c = "$Id: ftocstr.c,v 1.8 2002/02/28 10:49:22 spoel Exp $";
int ftocstr(char *ds, int dl, char *ss, int sl)
    /* dst, src ptrs */
    /* dst max len */
    /* src len */
{
    char *p;

    p = ss + sl;
    while ( --p >= ss && *p == ' ' );
    sl = p - ss + 1;
    dl--;
    ds[0] = 0;
    if (sl > dl)
      return 1;
    while (sl--)
      (*ds++ = *ss++);
    *ds = '\0';
    return 0;
}


int ctofstr(char *ds, int dl, char *ss)
     /* dest space */
     /* max dest length */
     /* src string (0-term) */
{
    while (dl && *ss) {
	*ds++ = *ss++;
	dl--;
    }
    while (dl--)
	*ds++ = ' ';
    return 0;
}
