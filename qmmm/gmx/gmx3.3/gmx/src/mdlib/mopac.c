static char *SRCID_mopac_c = "$Id: mopac.c,v 1.69 2002/02/28 10:32:04 spoel Exp $";

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
#include "copyrite.h"
#include "qmmm.h"
#include <stdio.h>
#include <string.h>
#include "fatal.h"
#include "typedefs.h"
#include <stdlib.h>


/* mopac interface routines */

#ifdef USE_MOPAC
void domldt_(int *nrqmat, int labels[], char keywords[]);

void domop_(int *nrqmat,double qmcrd[],int *nrmmat,
	    double mmchrg[],double mmcrd[],double qmgrad[],
	    double mmgrad[], double *energy,double qmcharges[]);
#else
void domldt_(int *nrqmat, int labels[], char keywords[])
{}

void domop_(int *nrqmat,double qmcrd[],int *nrmmat,
	    double mmchrg[],double mmcrd[],double qmgrad[],
  double mmgrad[], double *energy,double qmcharges[])
{}
#endif

void init_mopac(t_commrec *cr, t_QMrec *qm, t_MMrec *mm)
{
  /* initializes the mopac routines ans sets up the semiempirical
   * computation by calling moldat(). The inline mopac routines can
   * only perform gradient operations. If one would like to optimize a
   * structure or find a transition state at PM3 level, gaussian is
   * used instead.
   */
  char 
    *keywords;
  
  snew(keywords,240);
  
  if(!qm->bSH){    /* if rerun then grad should not be done! */
    sprintf(keywords,"PRECISE GEO-OK CHARGE=%d GRAD MMOK ANALYT %s\n",
	    qm->QMcharge,
	    eQMmethod_names[qm->QMmethod]);
  }
  else
    sprintf(keywords,"PRECISE GEO-OK CHARGE=%d SINGLET GRAD %s C.I.=(%d,%d) root=2 MECI \n",
	    qm->QMcharge,
	    eQMmethod_names[qm->QMmethod],
	    qm->CASorbitals,qm->CASelectrons/2);
  domldt_(&qm->nrQMatoms,qm->atomicnumberQM,keywords);
  fprintf(stderr,"keywords are: %s\n",keywords);
  free(keywords);
  
} /* init_mopac */

real call_mopac(t_commrec *cr, t_forcerec *fr, t_QMrec *qm, t_MMrec *mm, 
		rvec f[], rvec fshift[])
{
  /* do the actual QMMM calculation using directly linked mopac subroutines 
   */
  double /* always double as the MOPAC routines are always compiled in
	    double precission! */
    *qmcrd=NULL,*qmchrg=NULL,*mmcrd=NULL,*mmchrg=NULL,
    *qmgrad,*mmgrad=NULL,energy;
  int
    i,j;
  real
    QMener=0.0;
  snew(qmcrd, 3*(qm->nrQMatoms));
  snew(qmgrad,3*(qm->nrQMatoms));
  /* copy the data from qr into the arrays that are going to be used
   * in the fortran routines of MOPAC
   */
  for(i=0;i<qm->nrQMatoms;i++){
    for (j=0;j<DIM;j++){
      qmcrd[3*i+j] = (double)qm->xQM[i][j]*10;
    }
  }
  if(mm->nrMMatoms){
    /* later we will add the point charges here. There are some
     * conceptual problems with semi-empirical QM in combination with
     * point charges that we need to solve first....  
     */
    fatal_error(0,"At present only ONIOM is allowed in combination"
		" with MOPAC QM subroutines\n");
  }
  else {
    /* now compute the energy and the gradients.
     */
      
    snew(qmchrg,qm->nrQMatoms);    
    domop_(&qm->nrQMatoms,qmcrd,&mm->nrMMatoms,
	   mmchrg,mmcrd,qmgrad,mmgrad,&energy,qmchrg);
    /* add the gradients to the f[] array, and also to the fshift[].
     * the mopac gradients are in kCal/angstrom.
     */
    for(i=0;i<qm->nrQMatoms;i++){
      for(j=0;j<DIM;j++){
	f[i][j]       = (real)10*CAL2JOULE*qmgrad[3*i+j];
	fshift[i][j]  = (real)10*CAL2JOULE*qmgrad[3*i+j];
      }
    }
    QMener = (real)CAL2JOULE*energy;
    /* do we do something with the mulliken charges?? */

    free(qmchrg);
}
  free(qmgrad);
  free(qmcrd);
  return (QMener);
}

real call_mopac_SH(t_commrec *cr, t_forcerec *fr, t_QMrec *qm, t_MMrec *mm, 
		   rvec f[], rvec fshift[])
{
  /* do the actual SH QMMM calculation using directly linked mopac
   subroutines */

  double /* always double as the MOPAC routines are always compiled in
	    double precission! */
    *qmcrd=NULL,*qmchrg=NULL,*mmcrd=NULL,*mmchrg=NULL,
    *qmgrad,*mmgrad=NULL,energy;
  int
    i,j;
  real
    QMener=0.0;

  snew(qmcrd, 3*(qm->nrQMatoms));
  snew(qmgrad,3*(qm->nrQMatoms));
  /* copy the data from qr into the arrays that are going to be used
   * in the fortran routines of MOPAC
   */
  for(i=0;i<qm->nrQMatoms;i++){
    for (j=0;j<DIM;j++){
      qmcrd[3*i+j] = (double)qm->xQM[i][j]*10;
    }
  }
  if(mm->nrMMatoms){
    /* later we will add the point charges here. There are some
     * conceptual problems with semi-empirical QM in combination with
     * point charges that we need to solve first....  
     */
    fatal_error(0,"At present only ONIOM is allowed in combination with MOPAC\n");
  }
  else {
    /* now compute the energy and the gradients.
     */
    snew(qmchrg,qm->nrQMatoms);    

    domop_(&qm->nrQMatoms,qmcrd,&mm->nrMMatoms,
	   mmchrg,mmcrd,qmgrad,mmgrad,&energy,qmchrg);
    /* add the gradients to the f[] array, and also to the fshift[].
     * the mopac gradients are in kCal/angstrom.
     */
    for(i=0;i<qm->nrQMatoms;i++){
      for(j=0;j<DIM;j++){
	f[i][j]      = (real)10*CAL2JOULE*qmgrad[3*i+j];
	fshift[i][j] = (real)10*CAL2JOULE*qmgrad[3*i+j];
      }
    }
    QMener = (real)CAL2JOULE*energy;
  }
  free(qmgrad);
  free(qmcrd);
  return (QMener);
} /* call_mopac_SH */

