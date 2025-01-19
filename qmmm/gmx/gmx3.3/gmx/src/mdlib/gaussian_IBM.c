static char *SRCID_gaussian_c = "$Id: gaussian.c,v 1.69 2002/02/28 10:32:04 spoel Exp $";

#include <math.h>
#include "sysstuff.h"
#include "typedefs.h"
#include "macros.h"
#include "smalloc.h"
#include "assert.h"
#include "physics.h"
#include "macros.h"
#include "vec.h"
#include "force.h"
#include "invblock.h"
#include "confio.h"
#include "nsb.h"
#include "names.h"
#include "network.h"
#include "wnblist.h"
#include "pbc.h"
#include "ns.h"
#include "nrnb.h"
#include "bondf.h"
#include "mshift.h"
#include "txtdump.h"
#include "ewald_util.h"
#include "shift_util.h"
#include "pppm.h"
#include "poisson.h"
#include "ewald.h"
#include "pme.h"
#include "copyrite.h"
#include "qmmm.h"
#include <stdio.h>
#include <string.h>
#include "fatal.h"
#include "typedefs.h"
#include <stdlib.h>

/* Gaussian interface routines */

void init_gaussian(t_QMMMrec *qr)
{
  ivec
    basissets[eQMbasisNR]={{0,3,0},
			   {5,0,0},
			   {5,0,1},
			   {5,0,11},
			   {5,6,0},
			   {1,6,0},
			   {1,6,1},
			   {1,6,11},
			   {4,6,0}};
  char
    *buf;
  int
    i;
  
  /* using the ivec above to convert the basis read form the mdp file
   * in a human readable format into some numbers for the gaussian
   * route. This is necessary as we are using non standard routes to
   * do SH.
   */
  for(i=0;i<DIM;i++)
    qr->SHbasis[i]=basissets[qr->QMbasis][i];

  /* init gradually switching on of the SA */
  qr->SAstep = 0;
  /* we read the number of cpus and environment from the environment
   * if set.  
   */
  snew(buf,20);
  buf = getenv("NCPUS");
  if (buf)
    sscanf(buf,"%d",&qr->nQMcpus);
  else
    qr->nQMcpus=1;
  fprintf(stderr,"number of CPUs for gaussian = %d\n",qr->nQMcpus);
  snew(buf,50);
  buf = getenv("MEM");
  if (buf)
    sscanf(buf,"%d",&qr->QMmem);
  else
    qr->QMmem=50000000;
  fprintf(stderr,"memory for gaussian = %d\n",qr->QMmem);
  snew(buf,30);
  buf = getenv("ACC");
  if (buf)
    sscanf(buf,"%d",&qr->accuracy);
  else
    qr->accuracy=8;  
  fprintf(stderr,"accuracy in l510 = %d\n",qr->accuracy); 
  snew(buf,30);
  buf = getenv("CPMCSCF");
  if (buf)
    sscanf(buf,"%d",&qr->cpmcscf);
  else
    qr->cpmcscf=0;
  if (qr->cpmcscf)
    fprintf(stderr,"using cp-mcscf in l1003\n");
  else
    fprintf(stderr,"NOT using cp-mcscf in l1003\n"); 
  snew(buf,50);
  buf = getenv("SASTEP");
  if (buf)
    sscanf(buf,"%d",&qr->SAstep);
  else
    /* init gradually switching on of the SA */
    qr->SAstep = 0;
  /* we read the number of cpus and environment from the environment
   * if set.  
   */
  fprintf(stderr,"Level of SA at start = %d\n",qr->SAstep);

}  



void write_gaussian_SH_input(int step,bool swap,t_forcerec *fr)
{
  int
    i;
  bool
    bSA;
  FILE
    *out;
  t_QMMMrec
    *QMMMrec;

  QMMMrec = fr->qr;
  bSA = (QMMMrec->SAstep>0);

  out = fopen("input.com","w");
  /* write the route */
  fprintf(out,"%s","%scr=input\n");
  fprintf(out,"%s","%rwf=input\n");
  fprintf(out,"%s","%int=input\n");
  fprintf(out,"%s","%d2e=input\n");
/*  if(step)
 *   fprintf(out,"%s","%nosave\n");
 */
  fprintf(out,"%s","%chk=input\n");
  fprintf(out,"%s%d\n","%mem=",QMMMrec->QMmem);
  fprintf(out,"%s%3d\n","%nprocshare=",QMMMrec->nQMcpus);
  /* use the version of l7xx that can do gradients on point charges */
  fprintf(out,"%s","%subst l405 /gaussian01b1/g01b1test/l405\n");
  fprintf(out,"%s","%subst l510 /local/gerrit_devel/l510\n");
  if(fr->bRF && QMMMrec->QMMMscheme!=eQMMMschemeoniom){
    fprintf(out,"%s","%subst l301 /local/gerrit_devel/l301\n");
    fprintf(out,"%s","%subst l302 /local/gerrit_devel/l302\n");
    fprintf(out,"%s","%subst l701 /local/gerrit_devel/l701\n");
  } else {
    fprintf(out,"%s","%subst l301 /local/gerrit_devel/l301_no_rf\n");
    fprintf(out,"%s","%subst l701 /local/gerrit_devel/l701_no_rf\n");
  }
  fprintf(out,"%s","%subst l1003 /local/gerrit_devel/l1003\n");
  fprintf(out,"%s","%subst l9999 /local/gerrit_devel/l9999\n");
  fprintf(out,"%s",
	  "#P nonstd\n 1/18=10,20=1,38=1/1;\n");
  fprintf(out,"%s",
	  " 2/9=110,15=1,17=6,18=5,40=1/2;\n");
  if(QMMMrec->nrMMatoms)
    fprintf(out,
	    " 3/5=%d,6=%d,7=%d,25=1,32=1,43=1,94=-2/1,2,3;\n",
	    QMMMrec->SHbasis[0],
	    QMMMrec->SHbasis[1],
	    QMMMrec->SHbasis[2]); /*basisset stuff */
  else
    fprintf(out,
	    " 3/5=%d,6=%d,7=%d,25=1,32=1,43=0,94=-2/1,2,3;\n",
	    QMMMrec->SHbasis[0],
	    QMMMrec->SHbasis[1],
	    QMMMrec->SHbasis[2]); /*basisset stuff */
  /* development */
  if (step+1) /* fetch initial guess from check point file */
    /* hack, to alyays read from chk file!!!!! */
    fprintf(out,"%s%d,%s%d%s"," 4/5=1,7=6,17=",
	    QMMMrec->CASelectrons,
	    "18=",QMMMrec->CASorbitals,"/1,5;\n");
  else /* generate the first checkpoint file */
    fprintf(out,"%s%d,%s%d%s"," 4/5=0,7=6,17=",
	    QMMMrec->CASelectrons,
	    "18=",QMMMrec->CASorbitals,"/1,5;\n");
  /* the rest of the input depends on where the system is on the PES 
   */
  if(swap && bSA){ /* make a slide to the other surface */
    if(QMMMrec->CASorbitals>6){  /* use direct and no full diag */
      fprintf(out," 5/5=2,16=-2,17=10000000,28=2,32=2,38=6,97=100/10;\n");
    } 
    else {
      if(QMMMrec->cpmcscf){
	fprintf(out," 5/5=2,6=%d,17=31000200,28=2,32=2,38=6,97=100/10;\n",
		QMMMrec->accuracy);
	if(QMMMrec->nrMMatoms>0)
	  fprintf(out," 7/7=1,16=-2,30=1/1;\n");
	fprintf(out," 11/31=1,42=1,45=1/1;\n");
	fprintf(out," 10/6=1,10=700006,28=2,29=1,31=1,97=100/3;\n");
	fprintf(out," 7/30=1/16;\n 99/10=4/99;\n");
      }
      else{
	fprintf(out," 5/5=2,6=%d,17=11000000,28=2,32=2,38=6,97=100/10;\n",
		QMMMrec->accuracy);
	fprintf(out," 7/7=1,16=-2,30=1/1,2,3,16;\n 99/10=4/99;\n");
      }
    }

    /* modified 28-07-02!
     *    if(QMMMrec->CASorbitals>6)  use direct and no full diag 
     * fprintf(out," 5/5=2,16=-2,17=30000200,28=2,32=2,38=6,97=100/10;\n");
     * else
     * fprintf(out," 5/5=2,17=31000200,28=2,32=2,38=6,97=100/10;\n");
     *if(QMMMrec->nrMMatoms>0)
     * fprintf(out," 7/7=1,16=-2,30=1/1;\n");
     *    fprintf(out," 8/6=4,10=90,11=11/1;\n");
     *    fprintf(out," 11/31=1,42=1,45=1/1;\n");
     *fprintf(out," 10/6=1,10=700005,28=2,29=1,31=1,97=100/3;\n");
     *    fprintf(out," 6/7=3,28=1/1;\n");
     *    fprintf(out," 7/30=1/16;\n 99/10=4/99;\n");
     */
   }
  else if(bSA){ /* do a "state-averaged" CAS calculation */
    if(QMMMrec->CASorbitals>6){ /* no full diag */ 
      fprintf(out," 5/5=2,16=-2,17=10000000,28=2,32=2,38=6/10;\n");
    } 
    else {
      if(QMMMrec->cpmcscf){
	fprintf(out," 5/5=2,6=%d,17=31000200,28=2,32=2,38=6/10;\n",
		QMMMrec->accuracy);
	if(QMMMrec->nrMMatoms>0)
	  fprintf(out," 7/7=1,16=-2,30=1/1;\n");
	fprintf(out," 11/31=1,42=1,45=1/1;\n");
	fprintf(out," 10/6=1,10=700006,28=2,29=1,31=1/3;\n");
	fprintf(out," 7/30=1/16;\n 99/10=4/99;\n");
      }
      else{
      	fprintf(out," 5/5=2,6=%d,17=11000000,28=2,32=2,38=6/10;\n",
		QMMMrec->accuracy);
	fprintf(out," 7/7=1,16=-2,30=1/1,2,3,16;\n 99/10=4/99;\n");
      }
    }
    /* modified at 28-07-02!
     *
     * Moreover, we use the gradients from l1003, as they are needed
     * to describe the surface near the intersection seam more
     * accurately. Without these the SH does not even take place.....  
     *
     *if(QMMMrec->CASorbitals>6)
     * no full diag 
     *fprintf(out," 5/5=2,16=-2,17=30000200,28=2,32=2,38=6/10;\n");
     *else
     *fprintf(out," 5/5=2,17=31000200,28=2,32=2,38=6/10;\n");
     *if(QMMMrec->nrMMatoms>0) 
     *fprintf(out," 7/7=1,16=-2,30=1/1;\n");
     *    fprintf(out," 8/6=4,10=90,11=11/1;\n");
     *    fprintf(out," 11/31=1,42=1,45=1/1;\n");
     * fprintf(out," 10/6=1,10=700005,28=2,29=1,31=1/3;\n"); 
     *    fprintf(out," 6/7=3,28=1/1;\n");
     *    fprintf(out," 7/30=1/16;\n 99/10=4/99;\n");
     */
  }
  else if(swap){/* do a "swapped" CAS calculation */
    if(QMMMrec->CASorbitals>6)
      fprintf(out," 5/5=2,16=-2,17=0,28=2,32=2,38=6,97=100/10;\n");
    else
      fprintf(out," 5/5=2,6=%d,17=1000000,28=2,32=2,38=6,97=100/10;\n",
	      QMMMrec->accuracy);
    /*    fprintf(out," 6/7=2,8=2,9=2,10=2,28=1/1;\n");*/
    fprintf(out," 7/7=1,16=-2,30=1/1,2,3,16;\n 99/10=4/99;\n");
  }
  else {/* do a "normal" CAS calculation */
    if(QMMMrec->CASorbitals>6)
      fprintf(out," 5/5=2,16=-2,17=0,28=2,32=2,38=6/10;\n");
    else
      fprintf(out," 5/5=2,6=%d,17=1000000,28=2,32=2,38=6/10;\n",
	      QMMMrec->accuracy);
    /*    fprintf(out," 6/7=2,8=2,9=2,10=2,28=1/1;\n");*/
    fprintf(out," 7/7=1,16=-2,30=1/1,2,3,16;\n 99/10=4/99;\n");
  }
  fprintf(out, "\ninput-file generated by gromacs\n\n");
  fprintf(out,"%2d%2d\n",QMMMrec->QMcharge,QMMMrec->multiplicity);
  for (i=0;i<QMMMrec->nrQMatoms;i++){
#ifdef DOUBLE
    fprintf(out,"%3d %10.7lf  %10.7lf  %10.7lf\n",
	    QMMMrec->atomicnumberQM[i],
	    QMMMrec->xQM[i][XX]/BORH2NM,
	    QMMMrec->xQM[i][YY]/BORH2NM,
	    QMMMrec->xQM[i][ZZ]/BORH2NM);
#else
    fprintf(out,"%3d %10.7f  %10.7f  %10.7f\n",
	    QMMMrec->atomicnumberQM[i],
	    QMMMrec->xQM[i][XX]/BORH2NM,
	    QMMMrec->xQM[i][YY]/BORH2NM,
	    QMMMrec->xQM[i][ZZ]/BORH2NM);
#endif
  }
  /* MM point charge data */
  if(QMMMrec->QMMMscheme!=eQMMMschemeoniom && QMMMrec->nrMMatoms){
    fprintf(out,"\n");
    for(i=0;i<QMMMrec->nrMMatoms;i++){
#ifdef DOUBLE
      fprintf(out,"%10.7lf  %10.7lf  %10.7lf %8.4lf\n",
	      QMMMrec->xMM[i][XX]/BORH2NM,
	      QMMMrec->xMM[i][YY]/BORH2NM,
	      QMMMrec->xMM[i][ZZ]/BORH2NM,
	      QMMMrec->MMcharges[i]);
#else
      fprintf(out,"%10.7f  %10.7f  %10.7f %8.4f\n",
	      QMMMrec->xMM[i][XX]/BORH2NM,
	      QMMMrec->xMM[i][YY]/BORH2NM,
	      QMMMrec->xMM[i][ZZ]/BORH2NM,
	      QMMMrec->MMcharges[i]);
#endif
    }
  }
  if(bSA) {/* put the SA coefficients at the end of the file */
    fprintf(out,"\n%10.8lf %10.8lf\n",
	    QMMMrec->SAstep*0.5/QMMMrec->SAsteps,
	    1-QMMMrec->SAstep*0.5/QMMMrec->SAsteps);
    fprintf(stderr,"State Averaging level = %d/%d\n",QMMMrec->SAstep,QMMMrec->SAsteps);
  }
  fprintf(out,"\n");
  fclose(out);
}  /* write_gaussian_SH_input */

void write_gaussian_input(int step,t_QMMMrec *QMMMrec,t_forcerec *fr)
{
  int
    i;
  FILE
    *out;

  out = fopen("input.com","w");
  /* write the route */
  /*  fprintf(out,"%s","%scr=input\n");*/
  /*fprintf(out,"%s","%rwf=input\n");*/
  /*fprintf(out,"%s","%int=input\n");*/
  /*fprintf(out,"%s","%d2e=input\n");*/
/*  if(step)
 *   fprintf(out,"%s","%nosave\n");
 */
  fprintf(out,"%s","%chk=input\n");
  /*  fprintf(out,"%s%d\n","%mem=",QMMMrec->QMmem);*/
  fprintf(out,"%s%3d\n","%nprocshare=",QMMMrec->nQMcpus);
  /* use the version of l7xx that can do gradients on point charges */

  if(!fr->bRF){
    fprintf(out,"%s","%subst l701 /home/groenhof/Gaussian/code/g01/l701\n");
    fprintf(out,"%s","%subst l301 /home/groenhof/Gaussian/code/g01/l301\n");
  } else{
    fprintf(out,"%s","%subst l701 /home/groenhof/Gaussian/rf/l701\n");
    fprintf(out,"%s","%subst l301 /home/groenhof/Gaussian/rf/l301\n");
    fprintf(out,"%s","%subst l302 /home/groenhof/Gaussian/rf/l302\n");
  }
  fprintf(out,"%s","%subst l9999 /home/groenhof/Gaussian/code/g01/l9999\n");
  


  /*  fprintf(out,"%s","%subst l9999 /local/gerrit_devel/l9999\n");*/
  fprintf(out,"%s","#P "); 
  fprintf(out," %s/%s FORCE ",
   	  eQMmethod_names[QMMMrec->QMmethod],
	  eQMbasis_names[QMMMrec->QMbasis]);
  if(QMMMrec->QMMMscheme==eQMMMschemenormal)
    fprintf(out,"%s","Charge ");
  if (step) /* fetch guess from checkpoint file */
    fprintf(out,"%s"," guess=read");
  fprintf(out,"\nNosymm Punch=(Derivatives) units=bohr\n");
  fprintf(out,"iop(3/33=1)\n\n");
  fprintf(out, "input-file generated by gromacs\n\n");
  fprintf(out,"%2d%2d\n",QMMMrec->QMcharge,QMMMrec->multiplicity);
  for (i=0;i<QMMMrec->nrQMatoms;i++){
#ifdef DOUBLE
    fprintf(out,"%3d %10.7lf  %10.7lf  %10.7lf\n",
	    QMMMrec->atomicnumberQM[i],
	    QMMMrec->xQM[i][XX]/BORH2NM,
	    QMMMrec->xQM[i][YY]/BORH2NM,
	    QMMMrec->xQM[i][ZZ]/BORH2NM);
#else
    fprintf(out,"%3d %10.7f  %10.7f  %10.7f\n",
	    QMMMrec->atomicnumberQM[i],
	    QMMMrec->xQM[i][XX]/BORH2NM,
	    QMMMrec->xQM[i][YY]/BORH2NM,
	    QMMMrec->xQM[i][ZZ]/BORH2NM);
#endif
  }
  /* MM point charge data */
  if(QMMMrec->QMMMscheme!=eQMMMschemeoniom && QMMMrec->nrMMatoms){
    fprintf(out,"\n");
    for(i=0;i<QMMMrec->nrMMatoms;i++){
#ifdef DOUBLE
      fprintf(out,"%10.7lf  %10.7lf  %10.7lf %8.4lf\n",
	      QMMMrec->xMM[i][XX]/BORH2NM,
	      QMMMrec->xMM[i][YY]/BORH2NM,
	      QMMMrec->xMM[i][ZZ]/BORH2NM,
	      QMMMrec->MMcharges[i]);
#else
      fprintf(out,"%10.7f  %10.7f  %10.7f %8.4f\n",
	      QMMMrec->xMM[i][XX]/BORH2NM,
	      QMMMrec->xMM[i][YY]/BORH2NM,
	      QMMMrec->xMM[i][ZZ]/BORH2NM,
	      QMMMrec->MMcharges[i]);
#endif
    }
  }
  fprintf(out,"\n");
  fclose(out);
}  /* write_gaussian_input */

real read_gaussian_output(rvec QMgrad[],rvec MMgrad[],int step,
			  t_QMMMrec *QMMMrec)
{
  int
    i;
  char
    buf[300];
  real
    QMener;
  FILE
    *in;
  
  in=fopen("fort.7","r");
  /* first line is the energy and in the case of CAS, the energy
   * difference between the two states.
   */
  fgets(buf,300,in);
#ifdef DOUBLE
  sscanf(buf,"%lf\n",&QMener);
#else
  sscanf(buf,"%f\n", &QMener);
#endif
  /* next lines contain the gradients of the QM atoms */
  for(i=0;i<QMMMrec->nrQMatoms;i++){
    fgets(buf,300,in);
#ifdef DOUBLE
    sscanf(buf,"%lf %lf %lf\n",
	   &QMgrad[i][XX],
	   &QMgrad[i][YY],
	   &QMgrad[i][ZZ]);
#else
    sscanf(buf,"%f %f %f\n",
	   &QMgrad[i][XX],
	   &QMgrad[i][YY],
	   &QMgrad[i][ZZ]);
#endif     
  }
  /* the next lines, are the gradients of the MM atoms */
  
  if(QMMMrec->QMMMscheme!=eQMMMschemeoniom){
    for(i=0;i<QMMMrec->nrMMatoms;i++){
      fgets(buf,300,in);
#ifdef DOUBLE
      sscanf(buf,"%lf %lf %lf\n",
	     &MMgrad[i][XX],
	     &MMgrad[i][YY],
	     &MMgrad[i][ZZ]);
#else
      sscanf(buf,"%f %f %f\n",
	     &MMgrad[i][XX],
	     &MMgrad[i][YY],
	     &MMgrad[i][ZZ]);
#endif	
    }
  }
  fclose(in);
  return(QMener);  
}

real read_gaussian_SH_output(rvec QMgrad[],rvec MMgrad[],int step,
			  bool swapped,t_QMMMrec *QMMMrec)
{
  int
    i;
  char
    buf[300];
  real
    QMener,DeltaE;
  FILE
    *in;
  
  in=fopen("fort.7","r");
  /* first line is the energy and in the case of CAS, the energy
   * difference between the two states.
   */
  fgets(buf,300,in);
#ifdef DOUBLE
  sscanf(buf,"%lf %lf\n",&QMener,&DeltaE);
#else
  sscanf(buf,"%f %f\n",  &QMener,&DeltaE);
#endif

  /* switch on/off the State Averaging */
  
  if(DeltaE > QMMMrec->SAoff){
    if (QMMMrec->SAstep > 0){
      QMMMrec->SAstep--;
    }
  }
  else if (DeltaE < QMMMrec->SAon || (QMMMrec->SAstep > 0)){
    if (QMMMrec->SAstep < QMMMrec->SAsteps){
      QMMMrec->SAstep++;
    }
  }
  
  /* for debugging: */
  fprintf(stderr,"Gap = %5f,SA = %3d\n",DeltaE,(QMMMrec->SAstep>0));
  /* next lines contain the gradients of the QM atoms */
  for(i=0;i<QMMMrec->nrQMatoms;i++){
    fgets(buf,300,in);
#ifdef DOUBLE
    sscanf(buf,"%lf %lf %lf\n",
	   &QMgrad[i][XX],
	   &QMgrad[i][YY],
	   &QMgrad[i][ZZ]);
#else
    sscanf(buf,"%f %f %f\n",
	   &QMgrad[i][XX],
	   &QMgrad[i][YY],
	   &QMgrad[i][ZZ]);
#endif     
  }
  /* the next lines, are the gradients of the MM atoms */
  
  if(QMMMrec->QMMMscheme!=eQMMMschemeoniom){
    for(i=0;i<QMMMrec->nrMMatoms;i++){
      fgets(buf,300,in);
#ifdef DOUBLE
      sscanf(buf,"%lf %lf %lf\n",
	     &MMgrad[i][XX],
	     &MMgrad[i][YY],
	     &MMgrad[i][ZZ]);
#else
      sscanf(buf,"%f %f %f\n",
	     &MMgrad[i][XX],
	     &MMgrad[i][YY],
	     &MMgrad[i][ZZ]);
#endif	
    }
  }
  /* the next line contains the two CI eigenvector elements */
  fgets(buf,300,in);
  if(!step){
    sscanf(buf,"%d",&QMMMrec->CIdim);
    snew(QMMMrec->CIvec1,QMMMrec->CIdim);
    snew(QMMMrec->CIvec1old,QMMMrec->CIdim);
    snew(QMMMrec->CIvec2,QMMMrec->CIdim);
    snew(QMMMrec->CIvec2old,QMMMrec->CIdim);
  } else {
    /* before reading in the new current CI vectors, copy the current
     * CI vector into the old one.
     */
    for(i=0;i<QMMMrec->CIdim;i++){
      QMMMrec->CIvec1old[i] = QMMMrec->CIvec1[i];
      QMMMrec->CIvec2old[i] = QMMMrec->CIvec2[i];
    }
  }
  /* first vector */
  for(i=0;i<QMMMrec->CIdim;i++){
    fgets(buf,300,in);
#ifdef DOUBLE
    sscanf(buf,"%lf\n",&QMMMrec->CIvec1[i]);
#else
    sscanf(buf,"%f\n", &QMMMrec->CIvec1[i]);   
#endif
  }
  /* second vector */
  for(i=0;i<QMMMrec->CIdim;i++){
    fgets(buf,300,in);
#ifdef DOUBLE
    sscanf(buf,"%lf\n",&QMMMrec->CIvec2[i]);
#else
    sscanf(buf,"%f\n", &QMMMrec->CIvec2[i]);   
#endif
  }
  fclose(in);
  return(QMener);  
}

real inproduct(real *a, real *b, int n)
{
  int
    i;
  real
    dot=0.0;

  /* computes the inner product between two vectors (a.b), both of
   * which have length n.
   */  
  for(i=0;i<n;i++){
    dot+=a[i]*b[i];
  }
  return(dot);
}

int hop(int step, t_QMMMrec *qr)
{
  int
    swap = 0;
  real
    d11=0.0,d12=0.0,d21=0.0,d22=0.0;
  
  /* calculates the inproduct between the current Ci vector and the
   * previous CI vector. A diabatic hop will be made if d12 and d21
   * are much bigger than d11 and d22. In that case hop returns true,
   * otherwise it returns false.
   */  
  if(step){ /* only go on if more than one step has been done */
    d11 = inproduct(qr->CIvec1,qr->CIvec1old,qr->CIdim);
    d12 = inproduct(qr->CIvec1,qr->CIvec2old,qr->CIdim);
    d21 = inproduct(qr->CIvec2,qr->CIvec1old,qr->CIdim);
    d22 = inproduct(qr->CIvec2,qr->CIvec2old,qr->CIdim);
  }
  fprintf(stderr,"-------------------\n");
  fprintf(stderr,"d11 = %13.8f\n",d11);
  fprintf(stderr,"d12 = %13.8f\n",d12);
  fprintf(stderr,"d21 = %13.8f\n",d21);
  fprintf(stderr,"d22 = %13.8f\n",d22);
  fprintf(stderr,"-------------------\n");

  if((fabs(d12)>0.5)&&(fabs(d21)>0.5))
    swap = 1;
  
  return(swap);
}

void do_gaussian(int step)
{
  char
    buf[100];

  /* make the call to the gaussian binary through system()
   * The location of the binary will be picked up from the 
   * environment using getenv().
   */
  if(step) /* hack to prevent long inputfiles */
    sprintf(buf,"%s < %s > %s",
	    "/gaussian01b1/g01/g01",
	    "input.com","input.log");
  else
    sprintf(buf,"%s < %s > %s",
            "/home/groenhof/Gaussian/g01/g01",
            "input.com","input.log");
  fprintf(stderr,"Calling '%s'\n",buf);
  if ( system(buf) != 0 )
    fatal_error(0,"Call to '%s' failed\n",buf);
   fprintf(stderr,"Returned from system call\n");
}

real call_gaussian(t_commrec *cr, rvec f[], t_forcerec *fr)
{
  /* normal gaussian jobs */
  static int
    step=0;
  int
    i,j;
  real
    QMener=0.0;
  rvec
    *QMgrad,*MMgrad;
  t_QMMMrec
    *qr;
  /* copy the QMMMrec pointer */
  qr = fr->qr;

  snew(QMgrad,qr->nrQMatoms);
  snew(MMgrad,qr->nrMMatoms);

  write_gaussian_input(step,qr,fr);
  do_gaussian(step);
  QMener = read_gaussian_output(QMgrad,MMgrad,step,qr);
  /* add the QMMM forces to the gmx force array and to the fr->fsift
   */
  for(i=0;i<qr->nrQMatoms;i++){
    for(j=0;j<DIM;j++){
      f[qr->indexQM[i]][j]          -= HARTREE_BOHR2MD*QMgrad[i][j];
      fr->fshift[qr->shiftQM[i]][j] += HARTREE_BOHR2MD*QMgrad[i][j];
    }
  }
  for(i=0;i<qr->nrMMatoms;i++){
    for(j=0;j<DIM;j++){
      f[qr->indexMM[i]][j]          -= HARTREE_BOHR2MD*MMgrad[i][j];      
      fr->fshift[qr->shiftMM[i]][j] += HARTREE_BOHR2MD*MMgrad[i][j];
    }
  }
  QMener = QMener*HARTREE2KJ*AVOGADRO;
  step++;
  return(QMener);

} /* call_gaussian */

real call_gaussian_SH(t_commrec *cr, rvec f[], t_forcerec *fr)
{ 
  /* a gaussian call routine intended for doing diabatic surface
   * "sliding". See the manual for the theoretical background of this
   * TSH method.  
   */
  static int
    step=0;
  int
    state,i,j;
  real
    QMener=0.0;
  static  bool
    swapped=FALSE; /* handle for identifying the current PES */
  bool
    swap=FALSE; /* the actual swap */
  rvec
    *QMgrad,*MMgrad;
  t_QMMMrec
    *qr;
  char
    *buf;

  /* hack to do ground state simulations */
  if(!step){
    snew(buf,20);
    buf = getenv("STATE");
    if (buf)
      sscanf(buf,"%d",&state);
    else
      state=2;
    if(state==1)
      swapped=TRUE;
    free(buf);
  }
  /* end of hack */


  /* copy the QMMMrec pointer */
  qr = fr->qr;
  snew(QMgrad,qr->nrQMatoms);
  snew(MMgrad,qr->nrMMatoms);
  /* at step 0 there should be no SA */
  /*  if(!step)
   * qr->bSA=FALSE;*/
  /* temporray set to step + 1, since there is a chk start */
  write_gaussian_SH_input(step,swapped,fr);

  do_gaussian(step);
  QMener = read_gaussian_SH_output(QMgrad,MMgrad,step,swapped,qr);

  /* check for a surface hop. Only possible if we were already state
   * averaging.
   */
  if(qr->SAstep>0){
    if(!swapped){
      swap    = (step && hop(step,qr));
      swapped = swap;
    } 
    else { /* already on the other surface, so check if we go back */
      swap    = (step && hop(step,qr));
      swapped =!swap; /* so swapped shoud be false again */
    }
    if (swap){/* change surface, so do another call */
      write_gaussian_SH_input(step,swapped,fr);
      do_gaussian(step);
      QMener = read_gaussian_SH_output(QMgrad,MMgrad,step,swapped,qr);
    }
  }
  /* add the QMMM forces to the gmx force array and fshift
   */
  for(i=0;i<qr->nrQMatoms;i++){
    for(j=0;j<DIM;j++){
      f[qr->indexQM[i]][j]          -= HARTREE_BOHR2MD*QMgrad[i][j];
      fr->fshift[qr->shiftQM[i]][j] += HARTREE_BOHR2MD*QMgrad[i][j];
    }
  }
  for(i=0;i<qr->nrMMatoms;i++){
    for(j=0;j<DIM;j++){
      f[qr->indexMM[i]][j]          -= HARTREE_BOHR2MD*MMgrad[i][j];
      fr->fshift[qr->shiftMM[i]][j] += HARTREE_BOHR2MD*MMgrad[i][j];
    }
  }
  QMener = QMener*HARTREE2KJ*AVOGADRO;
  fprintf(stderr,"step %5d, SA = %5d, swap = %5d\n",
	  step,(qr->SAstep>0),swapped);
  step++;
  return(QMener);

} /* call_gaussian_SH */
    
/* end of gaussian sub routines */
