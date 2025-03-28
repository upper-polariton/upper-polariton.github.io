/*
 * $Id: g_anaeig.c,v 1.53 2002/05/08 09:58:10 hess Exp $
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
 * Gromacs Runs One Microsecond At Cannonball Speeds
 */
static char *SRCID_g_anaeig_c = "$Id: g_anaeig.c,v 1.53 2002/05/08 09:58:10 hess Exp $";
#include <math.h>
#include <string.h>
#include "statutil.h"
#include "sysstuff.h"
#include "typedefs.h"
#include "smalloc.h"
#include "macros.h"
#include "fatal.h"
#include "vec.h"
#include "pbc.h"
#include "copyrite.h"
#include "futil.h"
#include "statutil.h"
#include "rdgroup.h"
#include "pdbio.h"
#include "confio.h"
#include "tpxio.h"
#include "matio.h"
#include "mshift.h"
#include "xvgr.h"
#include "do_fit.h"
#include "rmpbc.h"
#include "txtdump.h"
#include "eigio.h"

char *proj_unit;

static real tick_spacing(real range,int minticks)
{
  real sp;

  sp = 0.2*exp(log(10)*ceil(log(range)/log(10)));
  while (range/sp < minticks-1)
    sp = sp/2;

  return sp;
}

static void write_xvgr_graphs(char *file,int ngraphs,
			      char *title,char *xlabel,char **ylabel,
			      int n,real *x, real **y,
			      real scale_x,bool bZero, bool bSplit)
{
  FILE *out;
  int g,i;
  real min,max,xsp,ysp;
  
  out=ffopen(file,"w"); 
  for(g=0; g<ngraphs; g++) {
    min=y[g][0];
    max=y[g][0];
    for(i=0; i<n; i++) {
      if (y[g][i]<min) min=y[g][i];
      if (y[g][i]>max) max=y[g][i];
    }
    if (bZero)
      min=0;
    else
      min=min-0.1*(max-min);
    max=max+0.1*(max-min);
    xsp=tick_spacing((x[n-1]-x[0])*scale_x,4);
    ysp=tick_spacing(max-min,3);
    fprintf(out,"@ with g%d\n@ g%d on\n",g,g);
    fprintf(out,"@ g%d autoscale type AUTO\n",g);
    if (g==0)
      fprintf(out,"@ title \"%s\"\n",title);
    if (g==ngraphs-1)
      fprintf(out,"@ xaxis  label \"%s\"\n",xlabel);
    else 
      fprintf(out,"@ xaxis  ticklabel off\n");
    fprintf(out,"@ world xmin %g\n",x[0]*scale_x);
    fprintf(out,"@ world xmax %g\n",x[n-1]*scale_x);
    fprintf(out,"@ world ymin %g\n",min);
    fprintf(out,"@ world ymax %g\n",max);
    fprintf(out,"@ view xmin 0.15\n");
    fprintf(out,"@ view xmax 0.85\n");
    fprintf(out,"@ view ymin %g\n",0.15+(ngraphs-1-g)*0.7/ngraphs);
    fprintf(out,"@ view ymax %g\n",0.15+(ngraphs-g)*0.7/ngraphs);
    fprintf(out,"@ yaxis  label \"%s\"\n",ylabel[g]);
    fprintf(out,"@ xaxis tick major %g\n",xsp);
    fprintf(out,"@ xaxis tick minor %g\n",xsp/2);
    fprintf(out,"@ xaxis ticklabel start type spec\n");
    fprintf(out,"@ xaxis ticklabel start %g\n",ceil(min/xsp)*xsp);
    fprintf(out,"@ yaxis tick major %g\n",ysp);
    fprintf(out,"@ yaxis tick minor %g\n",ysp/2);
    fprintf(out,"@ yaxis ticklabel start type spec\n");
    fprintf(out,"@ yaxis ticklabel start %g\n",ceil(min/ysp)*ysp);
    if ((min<0) && (max>0)) {
      fprintf(out,"@ zeroxaxis bar on\n");
      fprintf(out,"@ zeroxaxis bar linestyle 3\n");
    }
    for(i=0; i<n; i++) {
      if ( bSplit && i>0 && abs(x[i])<1e-5 )
	fprintf(out,"&\n");
      fprintf(out,"%10.4f %10.5f\n",x[i]*scale_x,y[g][i]);
    }
    fprintf(out,"&\n");
  }
  fclose(out);
}

static void compare(int natoms,int n1,rvec **eigvec1,int n2,rvec **eigvec2,
		    char *Eig1File,char *Eig2File)
{
  int    n,neig1,neig2,nrow;
  real   **eig1,**eig2;
  int    i,j,k;
  double sum1,sum2,trace1,trace2,sab,samsb2,tmp,ip;  

  n = min(n1,n2);

  neig1 = read_xvg(Eig1File,&eig1,&nrow);
  neig2 = read_xvg(Eig2File,&eig2,&nrow);
  fprintf(stdout,"\nRead %d and %d eigenvalues\n",neig1,neig2);
  n = min(n,min(neig1,neig2));
  fprintf(stdout,"Will compare the covariance matrices using %d dimensions\n",
	  n);

  sum1 = 0;
  for(i=0; i<n; i++) {
    if (eig1[1][i] < 0)
      eig1[1][i] = 0;
    sum1 += eig1[1][i];
    eig1[1][i] = sqrt(eig1[1][i]);
  }
  trace1 = sum1;
  for(i=n; i<neig1; i++)
    trace1 += eig1[1][i];
  sum2 = 0;
  for(i=0; i<n; i++) {
    if (eig2[1][i] < 0)
      eig2[1][i] = 0;
    sum2 += eig2[1][i];
    eig2[1][i] = sqrt(eig2[1][i]);
  }
  trace2 = sum2;
  for(i=n; i<neig2; i++)
    trace2 += eig1[1][i];

  fprintf(stdout,"Trace of the two matrices: %g and %g\n",sum1,sum2);
  if (neig1!=n || neig2!=n)
    fprintf(stdout,"this is %d%% and %d%% of the total trace\n",
	    (int)(100*sum1/trace1+0.5),(int)(100*sum2/trace2+0.5));
  fprintf(stdout,"Square root of the traces: %g and %g\n",
	  sqrt(sum1),sqrt(sum2));

  sab = 0;
  for(i=0; i<n; i++) {
    tmp = 0;
    for(j=0; j<n; j++) {
      ip = 0;
      for(k=0; k<natoms; k++)
	ip += iprod(eigvec1[i][k],eigvec2[j][k]);
      tmp += eig2[1][j]*ip*ip;
    }
    sab += eig1[1][i]*tmp;
  }

  samsb2 = sum1+sum2-2*sab;
  if (samsb2 < 0)
    samsb2 = 0;

  fprintf(stdout,"The overlap of the covariance matrices:\n");
  fprintf(stdout,"  normalized:  %.3f\n",1-sqrt(samsb2/(sum1+sum2)));
  tmp = 1-sab/sqrt(sum1*sum2);
  if (tmp < 0)
    tmp = 0;
  fprintf(stdout,"       shape:  %.3f\n",1-sqrt(tmp));
}

static void inprod_matrix(char *matfile,int natoms,
			  int nvec1,int *eignr1,rvec **eigvec1,
			  int nvec2,int *eignr2,rvec **eigvec2,
			  bool bSelect,int noutvec,int *outvec)
{
  FILE  *out;
  real  **mat;
  int   i,x1,y1,x,y,nlevels;
  int   nx,ny;
  real  inp,*t_x,*t_y,max;
  t_rgb rlo,rhi;

  snew(t_y,nvec2);
  if (bSelect) {
    nx = noutvec;
    ny = 0;
    for(y1=0; y1<nx; y1++)
      if (outvec[y1] < nvec2) {
	t_y[ny] = eignr2[outvec[y1]]+1;
	ny++;
      }
  } else {
    nx = nvec1;
    ny = nvec2;
    for(y=0; y<ny; y++)
      t_y[y] = eignr2[y]+1;
  }
  
  fprintf(stderr,"Calculating inner-product matrix of %dx%d eigenvectors\n",
	  nx,nvec2);
  
  snew(mat,nx);
  snew(t_x,nx);
  max = 0;
  for(x1=0; x1<nx; x1++) {
    snew(mat[x1],ny);
    if (bSelect)
      x = outvec[x1];
    else
      x = x1;
    t_x[x1] = eignr1[x]+1;
    fprintf(stderr," %d",eignr1[x]+1);
    for(y1=0; y1<ny; y1++) {
      inp = 0;
      if (bSelect) {
	while (outvec[y1] >= nvec2)
	  y1++;
	y= outvec[y1];
      } else
	y = y1;
      for(i=0; i<natoms; i++)
	inp += iprod(eigvec1[x][i],eigvec2[y][i]);
      mat[x1][y1] = fabs(inp);
      if (mat[x1][y1]>max)
	max = mat[x1][y1];
    }
  }
  fprintf(stderr,"\n");
  rlo.r = 1; rlo.g = 1; rlo.b = 1;
  rhi.r = 0; rhi.g = 0; rhi.b = 0;
  nlevels = 41;
  out = ffopen(matfile,"w");
  write_xpm(out,"Eigenvector inner-products","in.prod.","run 1","run 2",
	    nx,ny,t_x,t_y,mat,0.0,max,rlo,rhi,&nlevels);
  fclose(out);
}

static void overlap(char *outfile,int natoms,
		    rvec **eigvec1,
		    int nvec2,int *eignr2,rvec **eigvec2,
		    int noutvec,int *outvec)
{
  FILE *out;
  int i,v,vec,x;
  real overlap,inp;

  fprintf(stderr,"Calculating overlap between eigenvectors of set 2 with eigenvectors\n");
  for(i=0; i<noutvec; i++)
    fprintf(stderr,"%d ",outvec[i]+1);
  fprintf(stderr,"\n");

  out=xvgropen(outfile,"Subspace overlap",
	       "Eigenvectors of trajectory 2","Overlap");
  fprintf(out,"@ subtitle \"using %d eigenvectors of trajectory 1\"\n",
	  noutvec);
  overlap=0;
  for(x=0; x<nvec2; x++) {
    for(v=0; v<noutvec; v++) {
      vec=outvec[v];
      inp=0;
      for(i=0; i<natoms; i++)
	inp+=iprod(eigvec1[vec][i],eigvec2[x][i]);
      overlap+=sqr(inp);
    }
    fprintf(out,"%5d  %5.3f\n",eignr2[x]+1,overlap/noutvec);
  }

  fclose(out);
}

static void project(char *trajfile,t_topology *top,matrix topbox,rvec *xtop,
		    char *projfile,char *twodplotfile,char *threedplotfile,
		    char *filterfile,int skip,
		    char *extremefile,bool bExtrAll,real extreme,int nextr,
		    t_atoms *atoms,int natoms,atom_id *index,
		    bool bFit,rvec *xref,int nfit,atom_id *ifit,real *w_rls,
		    real *sqrtm,rvec *xav,
		    int *eignr,rvec **eigvec,
		    int noutvec,int *outvec, bool bSplit)
{
  FILE    *xvgrout=NULL;
  int     status,out=0,nat,i,j,d,v,vec,nfr,nframes=0,snew_size,frame;
  int     noutvec_extr,*imin,*imax;
  atom_id *all_at;
  matrix  box;
  rvec    *xread,*x;
  real    t,inp,**inprod=NULL,min=0,max=0;
  char    str[STRLEN],str2[STRLEN],**ylabel,*c;
  
  snew(x,natoms);
  
  if (bExtrAll)
    noutvec_extr=noutvec;
  else
    noutvec_extr=1;
  

  if (trajfile) {
    snew(inprod,noutvec+1);
    
    if (filterfile) {
      fprintf(stderr,"Writing a filtered trajectory to %s using eigenvectors\n",
	      filterfile);
      for(i=0; i<noutvec; i++)
	fprintf(stderr,"%d ",outvec[i]+1);
      fprintf(stderr,"\n");
      out=open_trx(filterfile,"w");
    }
    snew_size=0;
    nfr=0;
    nframes=0;
    nat=read_first_x(&status,trajfile,&t,&xread,box);
    if (nat>atoms->nr)
      fatal_error(0,"the number of atoms in your trajectory (%d) is larger than the number of atoms in your structure file (%d)",nat,atoms->nr); 
    snew(all_at,nat);
    for(i=0; i<nat; i++)
      all_at[i]=i;
    do {
      if (nfr % skip == 0) {
	if (top)
	  rm_pbc(&(top->idef),nat,box,xread,xread);
	if (nframes>=snew_size) {
	  snew_size+=100;
	  for(i=0; i<noutvec+1; i++)
	    srenew(inprod[i],snew_size);
	}
	inprod[noutvec][nframes]=t;
	/* calculate x: a fitted struture of the selected atoms */
	if (bFit && (xref==NULL)) {
	  reset_x(nfit,ifit,nat,NULL,xread,w_rls);
	  do_fit(nat,w_rls,xtop,xread);
	}
	for (i=0; i<natoms; i++)
	  copy_rvec(xread[index[i]],x[i]);
	if (bFit && xref) {
	  reset_x(natoms,all_at,natoms,NULL,x,w_rls);
	  do_fit(natoms,w_rls,xref,x);
	}

	for(v=0; v<noutvec; v++) {
	  vec=outvec[v];
	  /* calculate (mass-weighted) projection */
	  inp=0;
	  for (i=0; i<natoms; i++) {
	    inp+=(eigvec[vec][i][0]*(x[i][0]-xav[i][0])+
	    eigvec[vec][i][1]*(x[i][1]-xav[i][1])+
	    eigvec[vec][i][2]*(x[i][2]-xav[i][2]))*sqrtm[i];
	  }
	  inprod[v][nframes]=inp;
	}
	if (filterfile) {
	  for(i=0; i<natoms; i++)
	    for(d=0; d<DIM; d++) {
	      /* misuse xread for output */
	      xread[index[i]][d] = xav[i][d];
	      for(v=0; v<noutvec; v++)
		xread[index[i]][d] +=
		  inprod[v][nframes]*eigvec[outvec[v]][i][d]/sqrtm[i];
	    }
	  write_trx(out,natoms,index,atoms,0,t,box,xread,NULL);
	}
	nframes++;
      }
      nfr++;
    } while (read_next_x(status,&t,nat,xread,box));
    close_trj(status);
     sfree(x);
     if (filterfile)
       close_trx(out);
  }
  else
    snew(xread,atoms->nr);
  

  if (projfile) {
    snew(ylabel,noutvec);
    for(v=0; v<noutvec; v++) {
      sprintf(str,"vec %d",eignr[outvec[v]]+1);
      ylabel[v]=strdup(str);
    }
    sprintf(str,"projection on eigenvectors (%s)",proj_unit);
    write_xvgr_graphs(projfile, noutvec, str, xvgr_tlabel(),
		      ylabel, nframes, inprod[noutvec], inprod,
		      time_factor(), FALSE, bSplit);
  }
  
  if (twodplotfile) {
    sprintf(str,"projection on eigenvector %d (%s)",
	    eignr[outvec[0]]+1,proj_unit);
    sprintf(str2,"projection on eigenvector %d (%s)",
	    eignr[outvec[noutvec-1]]+1,proj_unit); 
    xvgrout=xvgropen(twodplotfile,"2D projection of trajectory",str,str2);
    for(i=0; i<nframes; i++) {
      if ( bSplit && i>0 && abs(inprod[noutvec][i])<1e-5 ) 
	fprintf(xvgrout,"&\n");
      fprintf(xvgrout,"%10.5f %10.5f\n",inprod[0][i],inprod[noutvec-1][i]);
    }
    fclose(xvgrout);
  }
  
  if (threedplotfile) {
    t_atoms atoms;
    rvec    *x;
    real    *b=NULL;
    matrix  box;
    char    *resnm,*atnm, pdbform[STRLEN];
    bool    bPDB, b4D;
    FILE    *out;
    
    if (noutvec < 3)
      fatal_error(0,"You have selected less than 3 eigenvectors");  
      
    /* initialize */
    bPDB = fn2ftp(threedplotfile)==efPDB;
    clear_mat(box);
    box[XX][XX] = box[YY][YY] = box[ZZ][ZZ] = 1;
    
    b4D = bPDB && (noutvec >= 4);
    if (b4D) {
      fprintf(stderr, "You have selected four or more eigenvectors:\n"
	      "fourth eigenvector will be plotted "
	      "in bfactor field of pdb file\n");
      sprintf(str,"4D proj. of traj. on eigenv. %d, %d, %d and %d",
	      eignr[outvec[0]]+1,eignr[outvec[1]]+1,
	      eignr[outvec[2]]+1,eignr[outvec[3]]+1);
    } else {
    sprintf(str,"3D proj. of traj. on eigenv. %d, %d and %d",
	    eignr[outvec[0]]+1,eignr[outvec[1]]+1,eignr[outvec[2]]+1);
    }
    init_t_atoms(&atoms,nframes,FALSE);
    snew(x,nframes);
    snew(b,nframes);
    atnm=strdup("C");
    resnm=strdup("PRJ");
    for(i=0; i<nframes; i++) {
      atoms.resname[i]=&resnm;
      atoms.atomname[i]=&atnm;
      atoms.atom[i].resnr=i;
      x[i][XX]=inprod[0][i];
      x[i][YY]=inprod[1][i];
      x[i][ZZ]=inprod[2][i];
      if (b4D)
	b[i]  =inprod[3][i];
    }
    if ( ( b4D || bSplit ) && bPDB ) {
      strcpy(pdbform,pdbformat);
      strcat(pdbform,"%8.4f%8.4f\n");
      
      out=ffopen(threedplotfile,"w");
      fprintf(out,"HEADER    %s\n",str);
      if ( b4D )
	fprintf(out,"REMARK    %s\n","fourth dimension plotted as B-factor");
      j=0;
      for(i=0; i<atoms.nr; i++) {
	if ( j>0 && bSplit && abs(inprod[noutvec][i])<1e-5 ) {
	  fprintf(out,"TER\n");
	  j=0;
	}
	fprintf(out,pdbform,"ATOM",i+1,"C","PRJ",' ',j+1,
		PR_VEC(10*x[i]), 1.0, 10*b[i]);
	if (j>0)
	  fprintf(out,"CONECT%5d%5d\n", i, i+1);
	j++;
      }
      fprintf(out,"TER\n");
      fclose(out);
    } else
      write_sto_conf(threedplotfile,str,&atoms,x,NULL,box); 
    free_t_atoms(&atoms);
  }
  
  if (extremefile) {
    if (extreme==0) {
      fprintf(stderr,"%11s %17s %17s\n","eigenvector","Minimum","Maximum");
      fprintf(stderr,
	      "%11s %10s %10s %10s %10s\n","","value","time","value","time");
      snew(imin,noutvec_extr);
      snew(imax,noutvec_extr);
      for(v=0; v<noutvec_extr; v++) {
	for(i=0; i<nframes; i++) {
	  if (inprod[v][i]<inprod[v][imin[v]])
	    imin[v]=i;
	  if (inprod[v][i]>inprod[v][imax[v]])
	    imax[v]=i;
	}
	min=inprod[v][imin[v]];
	max=inprod[v][imax[v]];
	fprintf(stderr,"%7d     %10.6f %10.1f %10.6f %10.1f\n",
		eignr[outvec[v]]+1,
		min,inprod[noutvec][imin[v]],max,inprod[noutvec][imax[v]]); 
      }
    }
    else {
      min=-extreme;
      max=+extreme;
    }
    /* build format string for filename: */
    strcpy(str,extremefile);/* copy filename */
    c=strrchr(str,'.'); /* find where extention begins */
    strcpy(str2,c); /* get extention */
    sprintf(c,"%%d%s",str2); /* append '%s' and extention to filename */
    for(v=0; v<noutvec_extr; v++) {
      /* make filename using format string */
      if (noutvec_extr==1)
	strcpy(str2,extremefile);
      else
	sprintf(str2,str,eignr[outvec[v]]+1);
      fprintf(stderr,"Writing %d frames along eigenvector %d to %s\n",
	      nextr,outvec[v]+1,str2);
      out=open_trx(str2,"w");
      for(frame=0; frame<nextr; frame++) {
	if ((extreme==0) && (nextr<=3))
	  for(i=0; i<natoms; i++)
	    atoms->atom[index[i]].chain='A'+frame;
	for(i=0; i<natoms; i++)
	  for(d=0; d<DIM; d++) 
	    xread[index[i]][d] = 
	      (xav[i][d] + (min*(nextr-frame-1)+max*frame)/(nextr-1)
	      *eigvec[outvec[v]][i][d]/sqrtm[i]);
	write_trx(out,natoms,index,atoms,0,frame,topbox,xread,NULL);
      }
      close_trx(out);
    }
  }
  fprintf(stderr,"\n");
}

static void components(char *outfile,int natoms,real *sqrtm,
		       int *eignr,rvec **eigvec,
		       int noutvec,int *outvec)
{
  int g,v,i;
  real *x,**y;
  char str[STRLEN],**ylabel;

  fprintf(stderr,"Writing atom displacements to %s\n",outfile);

  snew(ylabel,noutvec);
  snew(y,noutvec);
  snew(x,natoms);
  for(i=0; i<natoms; i++)
    x[i]=i+1;
  for(g=0; g<noutvec; g++) {
    v=outvec[g];
    sprintf(str,"vec %d",eignr[v]+1);
    ylabel[g]=strdup(str);
    snew(y[g],natoms);
    for(i=0; i<natoms; i++)
      y[g][i] = norm(eigvec[v][i])/sqrtm[i];
  }
  write_xvgr_graphs(outfile,noutvec,"Atom displacements (nm)","Atom number",
		    ylabel,natoms,x,y,1,TRUE,FALSE);
  fprintf(stderr,"\n");
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "[TT]g_anaeig[tt] analyzes eigenvectors. The eigenvectors can be of a",
    "covariance matrix ([TT]g_covar[tt]) or of a Normal Modes anaysis",
    "([TT]g_nmeig[tt]).[PAR]",
    
    "When a trajectory is projected on eigenvectors, all structures are",
    "fitted to the structure in the eigenvector file, if present, otherwise",
    "to the structure in the structure file. When no run input file is",
    "supplied, periodicity will not be taken into account. Most analyses",
    "are performed on eigenvectors [TT]-first[tt] to [TT]-last[tt], but when",
    "[TT]-first[tt] is set to -1 you will be prompted for a selection.[PAR]",
    
    "[TT]-disp[tt]: plot all atom displacements of eigenvectors",
    "[TT]-first[tt] to [TT]-last[tt].[PAR]",
    
    "[TT]-proj[tt]: calculate projections of a trajectory on eigenvectors",
    "[TT]-first[tt] to [TT]-last[tt].",
    "The projections of a trajectory on the eigenvectors of its",
    "covariance matrix are called principal components (pc's).",
    "It is often useful to check the cosine content the pc's,",
    "since the pc's of random diffusion are cosines with the number",
    "of periods equal to half the pc index.",
    "The cosine content of the pc's can be calculated with the program",
    "[TT]g_analyze[tt].[PAR]",
    
    "[TT]-2d[tt]: calculate a 2d projection of a trajectory on eigenvectors",
    "[TT]-first[tt] and [TT]-last[tt].[PAR]",
    
    "[TT]-3d[tt]: calculate a 3d projection of a trajectory on the first",
    "three selected eigenvectors.[PAR]",
    
    "[TT]-filt[tt]: filter the trajectory to show only the motion along",
    "eigenvectors [TT]-first[tt] to [TT]-last[tt].[PAR]",
    
    "[TT]-extr[tt]: calculate the two extreme projections along a trajectory",
    "on the average structure and interpolate [TT]-nframes[tt] frames",
    "between them, or set your own extremes with [TT]-max[tt]. The",
    "eigenvector [TT]-first[tt] will be written unless [TT]-first[tt] and",
    "[TT]-last[tt] have been set explicitly, in which case all eigenvectors",
    "will be written to separate files. Chain identifiers will be added",
    "when writing a [TT].pdb[tt] file with two or three structures (you",
    "can use [TT]rasmol -nmrpdb[tt] to view such a pdb file).[PAR]",
    
    "  Overlap calculations between covariance analysis:[BR]",
    "  NOTE: the analysis should use the same fitting structure[PAR]",
    
    "[TT]-over[tt]: calculate the subspace overlap of the eigenvectors in",
    "file [TT]-v2[tt] with eigenvectors [TT]-first[tt] to [TT]-last[tt]",
    "in file [TT]-v[tt].[PAR]",
    
    "[TT]-inpr[tt]: calculate a matrix of inner-products between",
    "eigenvectors in files [TT]-v[tt] and [TT]-v2[tt]. All eigenvectors",
    "of both files will be used unless [TT]-first[tt] and [TT]-last[tt]",
    "have been set explicitly.[PAR]",
    
    "When [TT]-v[tt], [TT]-eig1[tt], [TT]-v2[tt] and [TT]-eig2[tt] are given,",
    "a single number for the overlap between the covariance matrices is",
    "generated. The formulas are:[BR]",
    "        difference = sqrt(tr((sqrt(M1) - sqrt(M2))^2))[BR]",
    "normalized overlap = 1 - difference/sqrt(tr(M1) + tr(M2))[BR]",
    "     shape overlap = 1 - sqrt(tr((sqrt(M1/tr(M1)) - sqrt(M2/tr(M2)))^2))[BR]",
    "where M1 and M2 are the two covariance matrices and tr is the trace",
    "of a matrix. The numbers are proportional to the overlap of the square",
    "root of the fluctuations. The normalized overlap is the most useful",
    "number, it is 1 for identical matrices and 0 when the sampled",
    "subspaces are orthogonal."
  };
  static int  first=1,last=8,skip=1,nextr=2;
  static real max=0.0;
  static bool bSplit=FALSE;
  t_pargs pa[] = {
    { "-first", FALSE, etINT, {&first},     
      "First eigenvector for analysis (-1 is select)" },
    { "-last",  FALSE, etINT, {&last}, 
      "Last eigenvector for analysis (-1 is till the last)" },
    { "-skip",  FALSE, etINT, {&skip},
      "Only analyse every nr-th frame" },
    { "-max",  FALSE, etREAL, {&max}, 
      "Maximum for projection of the eigenvector on the average structure, "
      "max=0 gives the extremes" },
    { "-nframes",  FALSE, etINT, {&nextr}, 
      "Number of frames for the extremes output" },
    { "-split", FALSE, etBOOL, {&bSplit},
      "Split eigenvector projections where time is zero" }
  };
#define NPA asize(pa)
  
  FILE       *out;
  int        status,trjout;
  t_topology top;
  t_atoms    *atoms=NULL;
  rvec       *xtop,*xref1,*xref2;
  bool       bDMR1,bDMA1,bDMR2,bDMA2;
  int        nvec1,nvec2,*eignr1=NULL,*eignr2=NULL;
  rvec       *x,*xread,*xav1,*xav2,**eigvec1=NULL,**eigvec2=NULL;
  matrix     topbox;
  real       xid,totmass,*sqrtm,*w_rls,t,lambda;
  int        natoms,step;
  char       *grpname,*indexfile,title[STRLEN];
  int        i,j,d;
  int        nout,*iout,noutvec,*outvec,nfit;
  atom_id    *index,*ifit;
  char       *Vec2File,*topfile;
  char       *Eig1File,*Eig2File;
  char       *CompFile,*ProjOnVecFile;
  char       *TwoDPlotFile,*ThreeDPlotFile;
  char       *FilterFile,*ExtremeFile;
  char       *OverlapFile,*InpMatFile;
  bool       bFit1,bFit2,bM,bIndex,bTPS,bTop,bVec2,bProj;
  bool       bFirstToLast,bFirstLastSet,bTraj,bCompare,bPDB3D;

  t_filenm fnm[] = { 
    { efTRN, "-v",    "eigenvec",    ffREAD  },
    { efTRN, "-v2",   "eigenvec2",   ffOPTRD },
    { efTRX, "-f",    NULL,          ffOPTRD }, 
    { efTPS, NULL,    NULL,          ffOPTRD },
    { efNDX, NULL,    NULL,          ffOPTRD },
    { efXVG, "-eig1", "eigenval1",   ffOPTRD },
    { efXVG, "-eig2", "eigenval2",   ffOPTRD },
    { efXVG, "-disp", "eigdisp",     ffOPTWR },
    { efXVG, "-proj", "proj",        ffOPTWR },
    { efXVG, "-2d",   "2dproj",      ffOPTWR },
    { efSTO, "-3d",   "3dproj.pdb",  ffOPTWR },
    { efTRX, "-filt", "filtered",    ffOPTWR },
    { efTRX, "-extr", "extreme.pdb", ffOPTWR },
    { efXVG, "-over", "overlap",     ffOPTWR },
    { efXPM, "-inpr", "inprod",      ffOPTWR }
  }; 
#define NFILE asize(fnm) 

  CopyRight(stderr,argv[0]); 
  parse_common_args(&argc,argv,PCA_CAN_TIME | PCA_TIME_UNIT | PCA_CAN_VIEW | PCA_BE_NICE ,
		    NFILE,fnm,NPA,pa,asize(desc),desc,0,NULL); 

  indexfile=ftp2fn_null(efNDX,NFILE,fnm);

  Vec2File        = opt2fn_null("-v2",NFILE,fnm);
  topfile         = ftp2fn(efTPS,NFILE,fnm); 
  Eig1File        = opt2fn_null("-eig1",NFILE,fnm);
  Eig2File        = opt2fn_null("-eig2",NFILE,fnm);
  CompFile        = opt2fn_null("-disp",NFILE,fnm);
  ProjOnVecFile   = opt2fn_null("-proj",NFILE,fnm);
  TwoDPlotFile    = opt2fn_null("-2d",NFILE,fnm);
  ThreeDPlotFile  = opt2fn_null("-3d",NFILE,fnm);
  FilterFile      = opt2fn_null("-filt",NFILE,fnm);
  ExtremeFile     = opt2fn_null("-extr",NFILE,fnm);
  OverlapFile     = opt2fn_null("-over",NFILE,fnm);
  InpMatFile      = ftp2fn_null(efXPM,NFILE,fnm);
  bTop   = fn2bTPX(topfile);
  bProj  = ProjOnVecFile || TwoDPlotFile || ThreeDPlotFile 
    || FilterFile || ExtremeFile;
  bFirstLastSet  = 
    opt2parg_bSet("-first",NPA,pa) && opt2parg_bSet("-last",NPA,pa);
  bFirstToLast = CompFile || ProjOnVecFile || FilterFile || OverlapFile || 
    ((ExtremeFile || InpMatFile) && bFirstLastSet);
  bVec2  = Vec2File || OverlapFile || InpMatFile;
  bM     = CompFile || bProj;
  bTraj  = ProjOnVecFile || FilterFile || (ExtremeFile && (max==0))
    || TwoDPlotFile || ThreeDPlotFile;
  bIndex = bM || bProj;
  bTPS   = ftp2bSet(efTPS,NFILE,fnm) || bM || bTraj ||
    FilterFile  || (bIndex && indexfile);
  bCompare = Vec2File && Eig1File && Eig2File;
  bPDB3D = fn2ftp(ThreeDPlotFile)==efPDB;

  read_eigenvectors(opt2fn("-v",NFILE,fnm),&natoms,&bFit1,
		    &xref1,&bDMR1,&xav1,&bDMA1,&nvec1,&eignr1,&eigvec1);
  if (bVec2) {
    read_eigenvectors(Vec2File,&i,&bFit2,
		      &xref2,&bDMR2,&xav2,&bDMA2,&nvec2,&eignr2,&eigvec2);
    if (i!=natoms)
      fatal_error(0,"Dimensions in the eigenvector files don't match");
  }

  if ((!bFit1 || xref1) && !bDMR1 && !bDMA1) 
    bM=FALSE;
  if ((xref1==NULL) && (bM || bTraj))
    bTPS=TRUE;
    
  xtop=NULL;
  nfit=0;
  ifit=NULL;
  w_rls=NULL;
  if (!bTPS)
    bTop=FALSE;
  else {
    bTop=read_tps_conf(ftp2fn(efTPS,NFILE,fnm),
		       title,&top,&xtop,NULL,topbox,bM);
    atoms=&top.atoms;
    rm_pbc(&(top.idef),atoms->nr,topbox,xtop,xtop);
    /* Fitting is only needed when we need to read a trajectory */ 
    if (bTraj) {
      if ((xref1==NULL) || (bM && bDMR1)) {
	if (bFit1) {
	  printf("\nNote: the structure in %s should be the same\n"
		 "      as the one used for the fit in g_covar\n",topfile);
	  printf("\nSelect the index group that was used for the least squares fit in g_covar\n");
	  get_index(atoms,indexfile,1,&nfit,&ifit,&grpname);
	  snew(w_rls,atoms->nr);
	  for(i=0; (i<nfit); i++)
	    if (bM && bDMR1)
	      w_rls[ifit[i]]=atoms->atom[ifit[i]].m;
	    else
	      w_rls[ifit[i]]=1.0;
	}
	else {
	  /* make the fit index in xref instead of xtop */
	  nfit=natoms;
	  snew(ifit,natoms);
	  snew(w_rls,nfit);
	  for(i=0; (i<nfit); i++) {
	    ifit[i]=i;
	    w_rls[i]=atoms->atom[ifit[i]].m;
	  }
	}
      }
      else {
	/* make the fit non mass weighted on xref */
	nfit=natoms;
	snew(ifit,nfit);
	snew(w_rls,nfit);
	for(i=0; i<nfit; i++) {
	  ifit[i]=i;
	  w_rls[i]=1.0;
	}
      }
    }
  }

  if (bIndex) {
    printf("\nSelect an index group of %d elements that corresponds to the eigenvectors\n",natoms);
    get_index(atoms,indexfile,1,&i,&index,&grpname);
    if (i!=natoms)
      fatal_error(0,"you selected a group with %d elements instead of %d",
		  i,natoms);
    printf("\n");
  }

  snew(sqrtm,natoms);
  if (bM && bDMA1) {
    proj_unit="u\\S1/2\\Nnm";
    for(i=0; (i<natoms); i++)
      sqrtm[i]=sqrt(atoms->atom[index[i]].m);
  } else {
    proj_unit="nm";
    for(i=0; (i<natoms); i++)
      sqrtm[i]=1.0;
  }
  
  if (bVec2) {
    t=0;
    totmass=0;
    for(i=0; (i<natoms); i++)
      for(d=0;(d<DIM);d++) {
	t+=sqr((xav1[i][d]-xav2[i][d])*sqrtm[i]);
	totmass+=sqr(sqrtm[i]);
      }
    fprintf(stdout,"RMSD (without fit) between the two average structures:"
	    " %.3f (nm)\n\n",sqrt(t/totmass));
  }
  
  if (last==-1)
    last=natoms*DIM;
  if (first>-1) {
    if (bFirstToLast) {
      /* make an index from first to last */
      nout=last-first+1;
      snew(iout,nout);
      for(i=0; i<nout; i++)
	iout[i]=first-1+i;
    } else if (ThreeDPlotFile) {
      /* make an index of first+(0,1,2) and last */
      nout = bPDB3D ? 4 : 3;
      nout = min(last-first+1, nout);
      snew(iout,nout);
      iout[0]=first-1;
      iout[1]=first;
      if (nout>3)
	iout[2]=first+1;
      iout[nout-1]=last-1;
    } else {
      /* make an index of first and last */
      nout=2;
      snew(iout,nout);
      iout[0]=first-1;
      iout[1]=last-1;
    }
  }
  else {
    printf("Select eigenvectors for output, end your selection with 0\n");
    nout=-1;
    iout=NULL;
    do {
      nout++;
      srenew(iout,nout+1);
      scanf("%d",&iout[nout]);
      iout[nout]--;
    } while (iout[nout]>=0);
    printf("\n");
  }
  /* make an index of the eigenvectors which are present */
  snew(outvec,nout);
  noutvec=0;
  for(i=0; i<nout; i++) {
    j=0;
    while ((j<nvec1) && (eignr1[j]!=iout[i]))
      j++;
    if ((j<nvec1) && (eignr1[j]==iout[i])) {
      outvec[noutvec]=j;
      noutvec++;
    }
  }
  fprintf(stderr,"%d eigenvectors selected for output",noutvec);
  if (noutvec <= 100) {
    fprintf(stderr,":");
    for(j=0; j<noutvec; j++)
      fprintf(stderr," %d",eignr1[outvec[j]]+1);
  }
  fprintf(stderr,"\n");
  
  if (CompFile)
    components(CompFile,natoms,sqrtm,eignr1,eigvec1,noutvec,outvec);
  
  if (bProj)
    project(bTraj ? opt2fn("-f",NFILE,fnm) : NULL,
	    bTop ? &top : NULL,topbox,xtop,
	    ProjOnVecFile,TwoDPlotFile,ThreeDPlotFile,FilterFile,skip,
	    ExtremeFile,bFirstLastSet,max,nextr,atoms,natoms,index,
	    bFit1,xref1,nfit,ifit,w_rls,
	    sqrtm,xav1,eignr1,eigvec1,noutvec,outvec,bSplit);
  
  if (OverlapFile)
    overlap(OverlapFile,natoms,
	    eigvec1,nvec2,eignr2,eigvec2,noutvec,outvec);
  
  if (InpMatFile)
    inprod_matrix(InpMatFile,natoms,
		  nvec1,eignr1,eigvec1,nvec2,eignr2,eigvec2,
		  bFirstLastSet,noutvec,outvec);

  if (bCompare)
    compare(natoms,nvec1,eigvec1,nvec2,eigvec2,Eig1File,Eig2File);

  if (!CompFile && !bProj && !OverlapFile && !InpMatFile && !bCompare)
    fprintf(stderr,"\nIf you want some output,"
	    " set one (or two or ...) of the output file options\n");

  view_all(NFILE, fnm);

  return 0;
}
  
