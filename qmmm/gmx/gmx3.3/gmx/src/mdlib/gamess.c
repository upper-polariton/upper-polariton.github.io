static char *SRCID_qmmm_c = "$Id: qmmm.c,v 1.69 2002/02/28 10:32:04 spoel Exp $";

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


/* QMMM sub routines */
/* mopac interface routines */

#ifdef USE_GAMESS
void inigms_(void);

void endgms_(void);

void grads_(int *nrqmat,real *qmcrd,int *nrmmat, real *mmchrg, 
	    real *mmcrd, real *qmgrad,real *mmgrad, real *energy);
#else
void inigms_(void)
{}
void endgms_(void)
{}
void grads_(int *nrqmat,real *qmcrd,int *nrmmat, real *mmchrg, 
		   real *mmcrd, real *qmgrad,real *mmgrad, real *energy)
{}
#endif

void init_gamess(t_commrec *cr,t_QMrec *qm, t_MMrec *mm){
  /* it works hopelessly complicated :-)
   * first a file is written. Then the standard gamess input/output
   * routine is called (no system()!) to set up all fortran arrays. 
   * this routine writes a punch file, like in a normal gamess run.
   * via this punch file the other games routines, needed for gradient
   * and energy evaluations are called. This setup works fine for 
   * dynamics simulations. 7-6-2002 (London)
   */
  int 
    i,j,rank;
  FILE
    *out;
  char
    periodic_system[37][3]={"XX","H ","He","Li","Be","B ","C ","N ",
			    "O ","F ","Ne","Na","Mg","Al","Si","P ",
			    "S ","Cl","Ar","K ","Ca","Sc","Ti","V ",
			    "Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga",
			    "Ge","As","Se","Br","Kr"};
  
  if MASTER(cr){
    out=fopen("FOR009","w");
    /* of these options I am not completely sure....  the overall
     * preformance on more than 4 cpu's is rather poor at the moment.  
     */
    fprintf(out,"memory 48000000\nPARALLEL IOMODE SCREENED\n");
    fprintf(out,"ELEC %d\nMULT %d\nSUPER ON\nNOSYM\nGEOMETRY ANGSTROM\n",
	    qm->nelectrons,qm->multiplicity);
    for (i=0;i<qm->nrQMatoms;i++){
#ifdef DOUBLE
      fprintf(out,"%10.7lf  %10.7lf  %10.7lf  %5.3lf  %2s\n",
	      i/2.,
	      i/3.,
	      i/4.,  
	      qm->atomicnumberQM[i]*1.0,
	      periodic_system[qm->atomicnumberQM[i]]);
#else
      fprintf(out,"%10.7f  %10.7f  %10.7f  %5.3f  %2s\n",
	      i/2.,
	      i/3.,
	      i/4.,  
	      qm->atomicnumberQM[i]*1.0,
	      periodic_system[qm->atomicnumberQM[i]]);
#endif
    }
    if(mm->nrMMatoms){
      for (j=i;j<i+2;j++){
#ifdef DOUBLE
	fprintf(out,"%10.7lf  %10.7lf  %10.7lf  %5.3lf  BQ\n",
		j/5.,
		j/6.,
		j/7.,
		1.0);  
#else
	fprintf(out,"%10.7f  %10.7f  %10.7f  %5.3f  BQ\n",
		j/5.,
		j/6.,
		j/7.,
		2.0);  
#endif
      }
    }
    if(!qm->bTS)
      fprintf(out,"END\nBASIS %s\nRUNTYPE GRADIENT\nSCFTYPE %s\n",
	      eQMbasis_names[qm->QMbasis],
	      eQMmethod_names[qm->QMmethod]); /* see enum.h */
    else
      fprintf(out,"END\nBASIS %s\nRUNTYPE SADDLE\nSCFTYPE %s\n",
	      eQMbasis_names[qm->QMbasis],
	      eQMmethod_names[qm->QMmethod]); /* see enum.h */
    fclose(out);
  }
  gmx_sync_ring(cr->nodeid,cr->nnodes,cr->left,cr->right);
  inigms_();
}

real call_gamess(t_commrec *cr, t_forcerec *fr, t_QMrec *qm, t_MMrec *mm, 
		 rvec f[], rvec fshift[])
{
  /* do the actual QMMM calculation using GAMESS-UK. In this
   * implementation (3-2001) a system call is made to the GAMESS-UK
   * binary. Now we are working to get the electron integral, SCF, and
   * gradient routines linked directly 
   */
  int 
    i,j,rank;
  real
    QMener=0.0,*qmgrad,*mmgrad,*mmcrd,*qmcrd,energy;
  t_QMMMrec
    *qr;

  /* copy the QMMMrec pointer */
  qr = fr->qr;
  snew(qmcrd, 3*(qm->nrQMatoms));
  snew(mmcrd,3*(mm->nrMMatoms));
  snew(qmgrad,3*(qm->nrQMatoms));
  snew(mmgrad,3*(mm->nrMMatoms));
  
  /* copy the data from qr into the arrays that are going to be used
   * in the fortran routines of gamess
   */
  for(i=0;i<qm->nrQMatoms;i++){
    for (j=0;j<DIM;j++){
      qmcrd[DIM*i+j] = 1/BORH2NM*qm->xQM[i][j];
    }
  }
  for(i=0;i<mm->nrMMatoms;i++){
    for (j=0;j<DIM;j++){
      mmcrd[DIM*i+j] = 1/BORH2NM*mm->xMM[i][j];
    }
  }
  grads_(&qm->nrQMatoms,qmcrd,&mm->nrMMatoms,mm->MMcharges,
	 mmcrd,qmgrad,mmgrad,&energy);

  for(i=0;i<qm->nrQMatoms;i++){
    for(j=0;j<DIM;j++){
      f[i][j]      = HARTREE_BOHR2MD*qmgrad[3*i+j];
      fshift[i][j] = HARTREE_BOHR2MD*qmgrad[3*i+j];
    }
  }
  for(i=0;i<mm->nrMMatoms;i++){
    for(j=0;j<DIM;j++){
      f[i][j]      = HARTREE_BOHR2MD*mmgrad[3*i+j];
      fshift[i][j] = HARTREE_BOHR2MD*mmgrad[3*i+j];
    }
  }
  /* convert a.u to kJ/mol */
  QMener=energy*HARTREE2KJ*AVOGADRO;
  return(QMener);
}

