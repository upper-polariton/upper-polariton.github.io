/*
 * $Id: do_shift.c,v 1.5 2002/02/28 11:14:16 spoel Exp $
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
static char *SRCID_do_shift_c = "$Id: do_shift.c,v 1.5 2002/02/28 11:14:16 spoel Exp $";
#include <stdlib.h>
#include "errno.h"
#include "sysstuff.h"
#include "typedefs.h"
#include "string2.h"
#include "strdb.h"
#include "macros.h"
#include "smalloc.h"
#include "mshift.h"
#include "statutil.h"
#include "copyrite.h"
#include "confio.h"
#include "fatal.h"
#include "xvgr.h"
#include "gstat.h"
#include "rdgroup.h"
#include "pdbio.h"

void cat(FILE *out,char *fn,real t)
{
  FILE *in;
  char *ptr,buf[256];
  int    anr,rnr;
  char   anm[24],rnm[24];
  double f1,f2,f3,f4,f5,f6;
   
  in=ffopen(fn,"r");
  while ((ptr=fgets2(buf,255,in)) != NULL) {
    sscanf(buf,"%d%d%s%s%lf%lf%lf%lf%lf%lf",
	   &anr,&rnr,rnm,anm,&f1,&f2,&f3,&f4,&f5,&f6);
    fprintf(out,"%8g  %10g  %10g  %10g  %10g  %10g  %10g  %s%d-%s%d\n",
	    t,f6,f1,f2,f3,f4,f5,rnm,rnr,anm,anr);
  }
  /*if ((int)strlen(buf) > 0) 
    fprintf(out,"%s\n",buf);*/
  fflush(out);
  fclose(in);
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "do_shift reads a trajectory file and computes the chemical",
    "shift for each time frame (or every [BB]dt[bb] ps) by",
    "calling the 'total' program. If you do not have the total program,",
    "get it. do_shift assumes that the total executable is in",
    "/home/mdgroup/total/total. If that is not the case, then you should",
    "set an environment variable [BB]TOTAL[bb] as in: [PAR]",
    "[TT]setenv TOTAL /usr/local/bin/total[tt][PAR]",
    "where the right hand side should point to the total executable.[PAR]",
    "Output is printed in files shift.out where t is the time of the frame.[PAR]",
    "The program also needs an input file called [BB]random.dat[bb] which",
    "contains the random coil chemical shifts of all protons."
  };
  static real dt=0.0;
  t_pargs pa[] = {
    { "-dt", FALSE, etREAL, { &dt }, "Time interval between frames." }
  };
  static char *bugs[] = {
    "The program is very slow"
  };
  static     char *OXYGEN="O";
  FILE       *out,*tot,*fp;
  t_topology *top;
  t_atoms    *atoms;
  int        status,nres;
  real       t,nt;
  int        i,natoms,nframe=0;
  matrix     box;
  int        gnx;
  char       *grpnm,*randf;
  atom_id    *index;
  rvec       *x,*x_s;
  char       pdbfile[32],tmpfile[32];
  char       total[256],*dptr;
  t_filenm   fnm[] = {
    { efTRX, "-f",   NULL,     ffREAD },
    { efTPX, NULL,   NULL,     ffREAD },
    { efNDX, NULL,   NULL,     ffREAD },
    { efOUT, "-o",   "shift",  ffWRITE },
    { efDAT, "-d",   "random", ffREAD }
  };
  char *leg[] = { "shift","ring","anisCO","anisCN","sigmaE","sum" };
#define NFILE asize(fnm)

  CopyRight(stdout,argv[0]);
  parse_common_args(&argc,argv,PCA_CAN_TIME | PCA_BE_NICE ,NFILE,fnm,
		    asize(pa),pa,asize(desc),desc,asize(bugs),bugs);
		    
  top=read_top(ftp2fn(efTPX,NFILE,fnm));
  atoms=&(top->atoms);
  nres=atoms->nres;
  for(i=0; (i<atoms->nr); i++)
    if ((strcmp(*atoms->atomname[i],"O1") == 0) ||
	(strcmp(*atoms->atomname[i],"O2") == 0) ||
	(strcmp(*atoms->atomname[i],"OXT") == 0) ||
	(strcmp(*atoms->atomname[i],"OT") == 0))
      atoms->atomname[i]=&OXYGEN;
  rd_index(ftp2fn(efNDX,NFILE,fnm),1,&gnx,&index,&grpnm);
  
  snew(x_s,atoms->nr);

  strcpy(pdbfile,"dsXXXXXX");
  gmx_tmpnam(pdbfile);
  strcpy(tmpfile,"dsXXXXXX");
  gmx_tmpnam(tmpfile);
  fprintf(stderr,"pdbfile = %s\ntmpfile = %s\n",pdbfile,tmpfile);
  
  if ((dptr=getenv("TOTAL")) == NULL)
    dptr="/home/mdgroup/total/total";
  sprintf(total,"%s > /dev/null",dptr);
  fprintf(stderr,"total cmd='%s'\n",total);
  randf=ftp2fn(efDAT,NFILE,fnm);
  
  natoms=read_first_x(&status,ftp2fn(efTRX,NFILE,fnm),&t,&x,box);
  if (natoms != atoms->nr) 
    fatal_error(0,"Trajectory does not match topology!");
  out=ftp2FILE(efOUT,NFILE,fnm,"w");
  xvgr_legend(out,asize(leg),leg);
  nt=t;
  do {
    if (t >= nt) {
      rm_pbc(&(top->idef),top->atoms.nr,box,x,x_s);
      fp=ffopen(pdbfile,"w");
      write_pdbfile_indexed(fp,"Generated by do_shift",
			    atoms,x_s,box,0,-1,gnx,index);
      fclose(fp);
      
      if ((tot=popen(total,"w")) == NULL)
	perror("opening pipe to total");
      fprintf(tot,"%s\n",pdbfile);
      fprintf(tot,"%s\n",tmpfile);
      fprintf(tot,"3\n");
      fprintf(tot,"N\n");
      fprintf(tot,"%s\n",randf);
      fprintf(tot,"N\n");
      fprintf(tot,"N\n");
      if (pclose(tot) != 0)
	perror("closing pipe to total");
      cat(out,tmpfile,t);
      remove(pdbfile);
      remove(tmpfile);
      nt+=dt;
      nframe++;
    }
  } while(read_next_x(status,&t,natoms,x,box));
  close_trj(status);
  fclose(out);
  
  thanx(stderr);
  
  return 0;
}
