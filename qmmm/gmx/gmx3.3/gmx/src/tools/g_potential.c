/*
 * $Id: g_potential.c,v 1.18 2002/02/28 11:00:27 spoel Exp $
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
 * GROtesk MACabre and Sinister
 */
static char *SRCID_g_potential_c = "$Id: g_potential.c,v 1.18 2002/02/28 11:00:27 spoel Exp $";
#include <math.h>
#include <ctype.h>
#include "sysstuff.h"
#include "string.h"
#include "typedefs.h"
#include "smalloc.h"
#include "macros.h"
#include "princ.h"
#include "rmpbc.h"
#include "vec.h"
#include "xvgr.h"
#include "pbc.h"
#include "copyrite.h"
#include "futil.h"
#include "statutil.h"
#include "rdgroup.h"

#define EPS0 8.85419E-12
#define ELC 1.60219E-19

/****************************************************************************/
/* This program calculates the electrostatic potential across the box by    */
/* determining the charge density in slices of the box and integrating these*/
/* twice.                                                                   */
/* Peter Tieleman, April 1995                                               */
/* It now also calculates electrostatic potential in spherical micelles,    */
/* using \frac{1}{r}\frac{d^2r\Psi}{r^2} = - \frac{\rho}{\epsilon_0}        */
/* This probably sucks but it seems to work.                                */
/****************************************************************************/

static int ce=0, cb=0;

/* this routine integrates the array data and returns the resulting array */
/* routine uses simple trapezoid rule                                     */
void p_integrate(real *result, real data[], int ndata, real slWidth)
{
  int i, slice;
  real sum;
  
  if (ndata <= 2) 
    fprintf(stderr,"Warning: nr of slices very small. This will result"
	    "in nonsense.\n");

  fprintf(stderr,"Integrating from slice %d to slice %d\n",cb, ndata-ce);

  for (slice = cb; slice < (ndata-ce); slice ++)
  {
    sum = 0;
    for (i = cb; i < slice; i++)
      sum += slWidth * (data[i] + 0.5 * (data[i+1] - data[i]));
    result[slice] = sum;
  }
  return;
}

void calc_potential(char *fn, atom_id **index, int gnx[], 
		    real ***slPotential, real ***slCharge, 
		    real ***slField, int *nslices, 
		    t_topology *top, int axis, int nr_grps, real *slWidth,
		    real fudge_z, bool bSpherical)
{
  rvec *x0;              /* coordinates without pbc */
  matrix box;            /* box (3x3) */
  int natoms,            /* nr. atoms in trj */
      status,
      **slCount,         /* nr. of atoms in one slice for a group */
      i,j,n,             /* loop indices */
      teller = 0,      
      ax1=0, ax2=0,
      nr_frames = 0,     /* number of frames */
      slice;             /* current slice */
  real slVolume;         /* volume of slice for spherical averaging */
  real t, z;
  rvec xcm;

  switch(axis)
  {
  case 0:
    ax1 = 1; ax2 = 2;
    break;
  case 1:
    ax1 = 0; ax2 = 2;
    break;
  case 2:
    ax1 = 0; ax2 = 1;
    break;
  default:
    fatal_error(0,"Invalid axes. Terminating\n");
  }

  if ((natoms = read_first_x(&status,fn,&t,&x0,box)) == 0)
    fatal_error(0,"Could not read coordinates from statusfile\n");

  if (! *nslices)
    *nslices = (int)(box[axis][axis] * 10); /* default value */

  fprintf(stderr,"\nDividing the box in %d slices\n",*nslices);

  snew(*slField, nr_grps);
  snew(*slCharge, nr_grps);
  snew(*slPotential, nr_grps);

  for (i = 0; i < nr_grps; i++)
  {
    snew((*slField)[i], *nslices);
    snew((*slCharge)[i], *nslices);
    snew((*slPotential)[i], *nslices);
  }

  /*********** Start processing trajectory ***********/
  do 
  {
    *slWidth = box[axis][axis]/(*nslices);
    teller++;
    
    rm_pbc(&(top->idef),top->atoms.nr,box,x0,x0);

    /* calculate position of center of mass based on group 1 */
    calc_xcm(x0, gnx[0], index[0], top->atoms.atom, xcm, FALSE);
    svmul(-1,xcm,xcm);
	  
    for (n = 0; n < nr_grps; n++)
    {      
      for (i = 0; i < gnx[n]; i++)   /* loop over all atoms in index file */
      {
	if (bSpherical)
	{
	  rvec_add(x0[index[n][i]], xcm, x0[index[n][i]]);
	  /* only distance from origin counts, not sign */
	  slice = norm(x0[index[n][i]])/(*slWidth);
	  
	  /* this is a nice check for spherical groups but not for
	     all water in a cubic box since a lot will fall outside
	     the sphere
 	    if (slice > (*nslices)) 
	    {
	     fprintf(stderr,"Warning: slice = %d\n",slice);
	    }
	  */
	  (*slCharge)[n][slice] += top->atoms.atom[index[n][i]].q;
	}
	else
	{
	  z = x0[index[n][i]][axis];
	  z = z + fudge_z;
	  if (z < 0) 
	    z += box[axis][axis];
	  if (z > box[axis][axis])
	    z -= box[axis][axis];
	  /* determine which slice atom is in */
	  slice = (z / (*slWidth)); 
	  (*slCharge)[n][slice] += top->atoms.atom[index[n][i]].q;
	}
      }
    }
    nr_frames++;
  } while (read_next_x(status,&t,natoms,x0,box));
  
  /*********** done with status file **********/
  close_trj(status);
  
  /* slCharge now contains the total charge per slice, summed over all
     frames. Now divide by nr_frames and integrate twice 
   */
  
  if (bSpherical)
    fprintf(stderr,"\n\nRead %d frames from trajectory. Calculating potential"
	    "in spherical coordinates\n", nr_frames);
  else
    fprintf(stderr,"\n\nRead %d frames from trajectory. Calculating potential\n",
	    nr_frames);

  for (n =0; n < nr_grps; n++)
  {
    for (i = 0; i < *nslices; i++)
    {
      if (bSpherical)
      {
	/* charge per volume is now the summed charge, divided by the nr
	   of frames and by the volume of the slice it's in, 4pi r^2 dr
	   */
	slVolume = 4*M_PI * sqr(i) * sqr(*slWidth) * *slWidth;
	if (slVolume == 0)
	{
	  (*slCharge)[n][i] = 0;
	}
	else
	{
	  (*slCharge)[n][i] = (*slCharge)[n][i] / (nr_frames * slVolume);
	}
      }
      else
      {
	/* get charge per volume */
	(*slCharge)[n][i] = (*slCharge)[n][i] * (*nslices) /
	  (nr_frames * box[axis][axis] * box[ax1][ax1] * box[ax2][ax2]);
      }
    }
    /* Now we have charge densities */
   
    /* integrate twice to get field and potential */
    p_integrate((*slField)[n], (*slCharge)[n], *nslices, *slWidth);
    p_integrate((*slPotential)[n],(*slField)[n], *nslices, *slWidth);
  }
  
  /* Now correct for eps0 and in spherical case for r*/
  for (n = 0; n < nr_grps; n++)
    for (i = 0; i < *nslices; i++)
    {
      if (bSpherical)
      {
	(*slPotential)[n][i] = ELC * (*slPotential)[n][i] * -1.0E9 / 
	  (EPS0 * i * (*slWidth));
	(*slField)[n][i] = ELC * (*slField)[n][i] * 1E18 / 
	  (EPS0 * i * (*slWidth));
      }
      else 
      {
	(*slPotential)[n][i] = ELC * (*slPotential)[n][i] * -1.0E9 / EPS0  ;
	(*slField)[n][i] = ELC * (*slField)[n][i] * 1E18 / EPS0;
      }
    }
  
  sfree(x0);  /* free memory used by coordinate array */
}

void plot_potential(real *potential[], real *charge[], real *field[], 
		    char *afile, char *bfile, char *cfile, int nslices,
		    int nr_grps, char *grpname[], real slWidth)
{
  FILE       *pot,     /* xvgr file with potential */
             *cha,     /* xvgr file with charges   */
             *fie;     /* xvgr files with fields   */
  char       buf[256]; /* for xvgr title */
  int        slice, n;

  sprintf(buf,"Electrostatic Potential");
  pot = xvgropen(afile, buf, "Box (nm)","Potential (V)");
  xvgr_legend(pot,nr_grps,grpname);

  sprintf(buf,"Charge Distribution");
  cha = xvgropen(bfile, buf, "Box (nm)", "Charge density (q/nm\\S3\\N)");
  xvgr_legend(cha,nr_grps,grpname);

  sprintf(buf, "Electric Field");
  fie = xvgropen(cfile, buf, "Box (nm)", "Field (V/nm)");
  xvgr_legend(fie,nr_grps,grpname);

  for (slice = cb; slice < (nslices - ce); slice++)
  { 
    fprintf(pot,"%12g  ", slice * slWidth);
    fprintf(cha,"%12g  ", slice * slWidth);
    fprintf(fie,"%12g  ", slice * slWidth);
    for (n = 0; n < nr_grps; n++)
    {
      fprintf(pot,"   %12g", potential[n][slice]);
      fprintf(fie,"   %12g", field[n][slice]);
      fprintf(cha,"   %12g", charge[n][slice]);
    }
    fprintf(pot,"\n");
    fprintf(cha,"\n");
    fprintf(fie,"\n");
  }

  fclose(pot);
  fclose(cha);
  fclose(fie);
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "Compute the electrostatical potential across the box. The potential is"
    "calculated by first summing the charges per slice and then integrating"
    "twice of this charge distribution. Periodic boundaries are not taken  "
    "into account. Reference of potential is taken to be the left side of"
    "the box. It's also possible to calculate the potential in spherical"
    "coordinates as function of r by calculating a charge distribution in"
    "spherical slices and twice integrating them. epsilon_r is taken as 1,"
    "2 is more appropriate in many cases"
  };
  static int  axis = 2;                      /* normal to memb. default z  */
  static char *axtitle="Z"; 
  static int  nslices = 10;                  /* nr of slices defined       */
  static bool bSpherical = FALSE;            /* default is bilayer types   */
  static real fudge_z = 0;                    /* translate coordinates      */
  t_pargs pa [] = {
    { "-d",   FALSE, etSTR, {&axtitle}, 
      "Take the normal on the membrane in direction X, Y or Z." },
    { "-sl",  FALSE, etINT, {&nslices},
      "Calculate potential as function of boxlength, dividing the box"
      " in #nr slices." } ,
    { "-cb",  FALSE, etINT, {&cb},
      "Discard first #nr slices of box for integration" },
    { "-ce",  FALSE, etINT, {&ce},
      "Discard last #nr slices of box for integration" },
    { "-tz",  FALSE, etREAL, {&fudge_z},
      "Translate all coordinates <distance> in the direction of the box" },
    { "-spherical", FALSE, etBOOL, {&bSpherical},
      "Calculate spherical thingie" },
  };
  static char *bugs[] = {
    "Discarding slices for integration should not be necessary."
  };

  real     **potential,                    /* potential per slice        */
            **charge,                       /* total charge per slice     */
            **field,                        /* field per slice            */
            slWidth;                        /* width of one slice         */
  char      **grpname;            	    /* groupnames                 */
  int       ngrps = 0,                      /* nr. of groups              */
            *ngx;                           /* sizes of groups            */
  t_topology *top;                	    /* topology 		  */ 
  atom_id   **index;             	    /* indices for all groups     */
  t_filenm  fnm[] = {             	    /* files for g_order 	  */
    { efTRX, "-f", NULL,  ffREAD },    	    /* trajectory file 	          */
    { efNDX, NULL, NULL,  ffREAD },    	    /* index file 		  */
    { efTPX, NULL, NULL,  ffREAD },    	    /* topology file           	  */
    { efXVG,"-o","potential", ffWRITE },    /* xvgr output file 	  */
    { efXVG,"-oc","charge", ffWRITE }, 	    /* xvgr output file 	  */
    { efXVG,"-of","field", ffWRITE }, 	    /* xvgr output file 	  */
  };

#define NFILE asize(fnm)

  CopyRight(stderr,argv[0]);
  parse_common_args(&argc, argv, PCA_CAN_VIEW | PCA_CAN_TIME | PCA_BE_NICE,
		    NFILE,fnm,asize(pa),pa,asize(desc),desc,asize(bugs),bugs);

  /* Calculate axis */
  axis = toupper(axtitle[0]) - 'X';
  
  top = read_top(ftp2fn(efTPX,NFILE,fnm));     /* read topology file */

  printf("How many groups? ");
  do { scanf("%d",&ngrps); } while (ngrps <= 0);
  
  snew(grpname,ngrps);
  snew(index,ngrps);
  snew(ngx,ngrps);
 
  rd_index(ftp2fn(efNDX,NFILE,fnm),ngrps,ngx,index,grpname); 

  
  calc_potential(ftp2fn(efTRX,NFILE,fnm), index, ngx, 
		 &potential, &charge, &field,
		 &nslices, top, axis, ngrps, &slWidth, fudge_z,
		 bSpherical); 

  plot_potential(potential, charge, field, opt2fn("-o",NFILE,fnm),
		 opt2fn("-oc",NFILE,fnm), opt2fn("-of",NFILE,fnm),
		 nslices, ngrps, grpname, slWidth);

  do_view(opt2fn("-o",NFILE,fnm), NULL);       /* view xvgr file */
  do_view(opt2fn("-oc",NFILE,fnm), NULL);      /* view xvgr file */  
  do_view(opt2fn("-of",NFILE,fnm), NULL);      /* view xvgr file */

  thanx(stderr);
  return 0;
}









