/*
 * $Id: luck.c,v 1.8 2002/02/28 10:54:43 spoel Exp $
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
 * GROningen Mixture of Alchemy and Childrens' Stories
 */
static char *SRCID_luck_c = "$Id: luck.c,v 1.8 2002/02/28 10:54:43 spoel Exp $";
#include <stdio.h>
#include "copyrite.h"
#include "string.h"
#include "statutil.h"

int main(int argc,char *argv[])
{

  /* Necessary to find the library directory before installation */
  set_program_name(argv[0]);

  if ( argc == 2 ) 
    if ( strcmp(argv[1],"-c")==0)
      CopyRight(stdout,argv[0]);
  printf("%s\n",cool_quote());
  return 0;
}
 
