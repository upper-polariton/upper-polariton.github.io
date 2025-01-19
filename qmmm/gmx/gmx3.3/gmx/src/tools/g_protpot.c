/*
 * $Id: protpot.c,v 1.4 2001/07/23 15:28:29 lindahl Exp $
 * 
 *                This source code is part of
 * 
 *                 G   R   O   M   A   C   S
 * 
 *          GROningen MAchine for Chemical Simulations
 * 
 *                        VERSION 3.0
 * 
 * Copyright (c) 1991-2001
 * BIOSON Research Institute, Dept. of Biophysical Chemistry
 * University of Groningen, The Netherlands
 * 
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
 * Do check out http://www.gromacs.org , or mail us at gromacs@gromacs.org .
 * 
 * And Hey:
 * Gyas ROwers Mature At Cryogenic Speed
 */

/* This line is only for CVS version info */
static char *SRCID_template_c = "$Id: template.c,v 1.4 2001/07/23 15:28:29 lindahl Exp $";

#include "statutil.h"
#include "typedefs.h"
#include "smalloc.h"
#include "vec.h"
#include "copyrite.h"
#include "statutil.h"
#include "tpxio.h"
#include "math.h"

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "this is a small test program meant to serve as a template ",
    "when writing your own analysis tools. The advantage of ",
    "using gromacs for this is that you have access to all ",
    "information in the topology, and your program will be ",
    "able to handle all types of coordinates and trajectory ",
    "files supported by gromacs. Go ahead and try it! ",
    "This test version just writes the coordinates of an ",
    "arbitrary atom to standard out for each frame. You can ",
    "select which atom you want to examine with the -n argument."
  };
  
  static int n=1;

  /* Extra arguments - but note how you always get the begin/end
   * options when running the program, without mentioning them here!
   */
  
  t_pargs pa[] = {
    { "-n", FALSE, etINT, {&n},
      "Plot data for atom number n (starting on 1)"
    }
  };
  
  t_topology top;
  char       title[STRLEN],out_file1[256],out_file2[256];
  t_trxframe fr,frout1,frout2;
  rvec       *xtop;
  matrix     box;
  int        status;
  int        flags = TRX_READ_X;
  int        trx_after=NOTSET,trx_before=NOTSET;
  int        i,j;
  real       r,rr=0;
  rvec       Hx,Hv,Hf;
  t_filenm fnm[] = {
    { efTPS,  NULL,  NULL, ffREAD },   /* this is for the topology */
    { efTRX, "-f", NULL, ffREAD }      /* and this for the trajectory */
  };
  
#define NFILE asize(fnm)

  CopyRight(stderr,argv[0]);
  sprintf(out_file2,"after.xtc");
  sprintf(out_file1,"before.xtc");
  /* This is the routine responsible for adding default options,
   * calling the X/motif interface, etc. */
  parse_common_args(&argc,argv,PCA_CAN_TIME | PCA_CAN_VIEW,
		    NFILE,fnm,asize(pa),pa,asize(desc),desc,0,NULL);

  /* We don't need any topology information to write the coordinates,
   * but to show how it works we start by writing the name and
   * charge of the selected atom. It returns a boolean telling us
   * whether the topology was found and could be read
   */
  
  read_tps_conf(ftp2fn(efTPS,NFILE,fnm),title,&top,&xtop,NULL,box,TRUE);
  sfree(xtop);

  

  trx_after = open_trx(out_file2,"w");
  trx_before= open_trx(out_file1,"w");
  /* The first time we read data is a little special */
  read_first_frame(&status,ftp2fn(efTRX,NFILE,fnm),&fr,flags);

  frout1=fr;
  frout2=fr;

  frout2.natoms+=2;  
  frout1.natoms+=2;
  srenew(frout1.x,frout1.natoms);
  srenew(frout1.x,frout2.natoms);
  if(fr.bV){
    srenew(frout1.v,frout1.natoms);
    srenew(frout2.v,frout2.natoms);
  }
  if(fr.bF){
    srenew(frout1.f,frout1.natoms);
    srenew(frout2.f,frout2.natoms);
  }


  /* This is the main loop over frames */

  /* HACK MAAR RAAK */

  do {

    /* manipulate the position of the proton, add the link atoms and
     * reshuffle the order in the xtc file
     */

    /* add Link atoms */
    for(i=fr.natoms-1;i>=1272;i--){
      for(j=0;j<DIM;j++){
	frout1.x[i+2][j]=fr.x[i][j];
	if(fr.bV)
	  frout1.v[i+2][j]=fr.v[i][j];
	if(fr.bF)
	  frout1.f[i+2][j]=fr.f[i][j];
      }
    }
    for(i=1272;i<=1273;i++){
      for(j=0;j<DIM;j++){
	frout1.x[i][j]=0.0;
	if(fr.bV)
	  frout1.v[i][j]=0.0;
	if(fr.bF)
	  frout1.f[i][j]=0.0;
      }
    }
    write_trxframe(trx_before,&frout1);

    /* transfer Proton from Glu to PCA and write result to trx_after */

    rr=0.0;
    for(j=0;j<DIM;j++)
      rr+=(fr.x[420][j]-fr.x[663][j])*(fr.x[420][j]-fr.x[663][j]);
    r=sqrt(rr);

    for(j=0;j<DIM;j++){
      Hx[j]=fr.x[663][j]+(fr.x[420][j]-fr.x[663][j])/r*0.095;
      if(fr.bV)
	Hv[j]=fr.v[421][j];
      if(fr.bF)
	Hf[j]=fr.f[421][j];
    }
    for(i=421;i<664;i++){
      for(j=0;j<DIM;j++){
	frout1.x[i][j]=frout1.x[i+1][j];
	if(fr.bV)
	  frout1.v[i][j]=frout1.v[i+1][j];
	if(fr.bF)
	  frout1.f[i][j]=frout1.f[i+1][j];
      } 
    }
    for(j=0;j<DIM;j++){
      frout1.x[663][j]=Hx[j];
      if(fr.bV)
	frout1.v[663][j]=Hv[j];
      if(fr.bF)
	frout1.f[663][j]=Hf[j];
    } 

    /* write another xtc file with a transferred proton */

    write_trxframe(trx_after,&frout1);
    
    /* coordinates are available in the vector fr.x
     * you can find this and all other structures in
     * the types directory under the gromacs include dir.
     * Note how flags determines wheter to read x/v/f!  */
    /*    printf("Coordinates at t=%8.3f : %8.5f %8.5f %8.5f\n",fr.time,fr.x[n][XX],fr.x[n][YY],fr.x[n][ZZ]);*/
  } while(read_next_frame(status,&fr));
  close_trx(trx_before);
  close_trx(trx_after);
  thanx(stderr);
  
  return 0;
}

