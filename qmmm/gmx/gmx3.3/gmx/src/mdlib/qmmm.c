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

/* f2c/g2c hack */
int MAIN__()
{
    return(0);
} /* MAIN__ */

/* QMMM sub routines */

/* declarations of the interfaces to the QM packages. The _SH indicate
 * the QM interfaces can be used for Surface Hopping simulations 
 */
extern void init_gamess(t_commrec *cr, t_QMrec *qm, t_MMrec *mm);

extern void init_mopac(t_commrec *cr, t_QMrec *qm, t_MMrec *mm);

extern void init_gaussian(t_commrec *cr ,t_QMrec *qm, t_MMrec *mm);

extern real call_gamess(t_commrec *cr,t_forcerec *fr,
			t_QMrec *qm, t_MMrec *mm,rvec f[], rvec fshift[]);

extern real call_gaussian_SH(t_commrec *cr,t_forcerec *fr,t_QMrec *qm, 
			     t_MMrec *mm,rvec f[], rvec fshift[]);

extern real call_gaussian(t_commrec *cr,t_forcerec *fr, t_QMrec *qm,
			  t_MMrec *mm,rvec f[], rvec fshift[]);

extern real call_mopac(t_commrec *cr,t_forcerec *fr, t_QMrec *qm, 
		       t_MMrec *mm,rvec f[], rvec fshift[]);

extern real call_mopac_SH(t_commrec *cr,t_forcerec *fr,t_QMrec *qm, 
			  t_MMrec *mm,rvec f[], rvec fshift[]);

/* this struct and these comparison functions are needed for creating
 * a QMMM input for the QM routines from the QMMM neighbor list.  
 */

typedef struct {
  int      j;
  int      shift;
} t_j_particle;

static int struct_comp(const void *a, const void *b){

  return (int)(((t_j_particle *)a)->j)-(int)(((t_j_particle *)b)->j);
  
} /* struct_comp */

static int int_comp(const void *a,const void *b){
  
  return (*(int *)a) - (*(int *)b);
  
} /* int_comp */

static int QMlayer_comp(const void *a, const void *b){
  
  return (int)(((t_QMrec *)a)->nrQMatoms)-(int)(((t_QMrec *)b)->nrQMatoms);
  
} /* QMlayer_comp */

void sort_QMlayers(t_QMMMrec *qr){
  /* sorts QM layers from small to big */
  qsort(qr->qm,qr->nrQMlayers,
	(size_t)sizeof(qr->qm[0]),
	QMlayer_comp);
} /* sort_QMlayers */


real call_QMroutine(t_commrec *cr, t_forcerec *fr, t_QMrec *qm, 
		    t_MMrec *mm, rvec f[], rvec fshift[])
{
  /* makes a call to the requested QM routine (qm->QMmethod) 
   */
  real
    QMener=0.0;

    /* do a semi-empiprical calculation */

#ifdef USE_MOPAC
  if (qm->QMmethod<eQMmethodRHF && !(mm->nrMMatoms)){
    if (qm->bSH)
      QMener = call_mopac_SH(cr,fr,qm,mm,f,fshift);
    else
      QMener = call_mopac(cr,fr,qm,mm,f,fshift);
  }
  else
#endif
    {
      /* do an ab-initio calculation */
      if (qm->bSH){
	QMener = call_gaussian_SH(cr,fr,qm,mm,f,fshift);
      }
      else{
#ifdef USE_GAMESS
	QMener = call_gamess(cr,fr,qm,mm,f,fshift);
#else
	QMener = call_gaussian(cr,fr,qm,mm,f,fshift);
#endif
      }
    }
  return (QMener);
}

void init_QMroutine(t_commrec *cr, t_QMrec *qm, t_MMrec *mm)
{
  /* makes a call to the requested QM routine (qm->QMmethod) 
   */
#ifdef USE_MOPAC
  if (qm->QMmethod<eQMmethodRHF){
    /* do a semi-empiprical calculation */
    init_mopac(cr,qm,mm);
  }
  else 
#endif
    {
    /* do an ab-initio calculation */
#ifdef USE_GAMESS
    init_gamess(cr,qm,mm);
#else
    init_gaussian(cr,qm,mm);
#endif
    }
}

void atomic_number(int nr, char ***atomtype, int *nucnum)
{
  /* retrievs the atomic numbers of the atoms and puts them in the
   * array nucnum. On exit atomic_number returns the sum of the atomic
   * numbers. Caution: this function oly works for the ffG43
   * forcefield. Each forcefield requires its own atomic_number
   * function.....  Maybe later the .atp file will also contain teh
   * atomic numbner, but this requires some thought, as it would make
   * this version of Gromacs incompatible with previous versions
   * 1-1-2002 */
  
  int 
    i;
  
  for (i=0;i<nr;i++){
    if(!strcmp((*(atomtype[i])),"O"))      
      nucnum[i]=8;
    else if(!strcmp((*(atomtype[i])),"OM"))
      nucnum[i]=8;
    else if(!strcmp((*(atomtype[i])),"OA"))
      nucnum[i]=8;
    else if(!strcmp((*(atomtype[i])),"OW"))
      nucnum[i]=8;      
    else if(!strcmp((*(atomtype[i])),"N"))
      nucnum[i]=7;
    else if(!strcmp((*(atomtype[i])),"NT"))
      nucnum[i]=7;    
    else if(!strcmp((*(atomtype[i])),"NL"))
      nucnum[i]=7;    /* and so on..... still needs to be completed! */
    else if(!strcmp((*(atomtype[i])),"NR"))
      nucnum[i]=7;
    else if(!strcmp((*(atomtype[i])),"NZ"))
      nucnum[i]=7;    
    else if(!strcmp((*(atomtype[i])),"NE"))
      nucnum[i]=7;
    else if(!strcmp((*(atomtype[i])),"C"))
      nucnum[i]=6;
    else if(!strcmp((*(atomtype[i])),"CH1"))
      nucnum[i]=6;
    else if(!strcmp((*(atomtype[i])),"CH2"))
      nucnum[i]=6;
    else if(!strcmp((*(atomtype[i])),"CH3"))
      nucnum[i]=6;
    else if(!strcmp((*(atomtype[i])),"CR1"))
      nucnum[i]=6;
    else if(!strcmp((*(atomtype[i])),"HC"))
      nucnum[i]=1;
    else if(!strcmp((*(atomtype[i])),"H"))
      nucnum[i]=1;
    else if(!strcmp((*(atomtype[i])),"HW"))
      nucnum[i]=1;
    else if(!strcmp((*(atomtype[i])),"S"))
      nucnum[i]=16;
    else if(!strcmp((*(atomtype[i])),"CU1+"))
      nucnum[i]=7;    
    else if(!strcmp((*(atomtype[i])),"NA+"))
      nucnum[i]=11;
    else if(!strcmp((*(atomtype[i])),"LA"))
      nucnum[i]=1; 
    else if(!strcmp((*(atomtype[i])),"P"))
      nucnum[i]=15;
    else if(!strcmp((*(atomtype[i])),"CL"))
          nucnum[i]=17;
    else if(!strcmp((*(atomtype[i])),"CL-"))
          nucnum[i]=17;
    else if(!strcmp((*(atomtype[i])),"XX"))
          nucnum[i]=0;
    else if(!strcmp((*(atomtype[i])),"OP"))
                    nucnum[i]=8;
    else
      fatal_error(0,"atomtype %s does not exist in ffG43a2!\n",
		  ((*(atomtype[i])))); 
  }
} /* atomic_number */ 

void update_QMMM_coord(rvec x[],t_forcerec *fr, t_QMrec *qm, t_MMrec *mm)
{
  /* shifts the QM and MM particles into the central box and stores
   * these shifted coordinates in the coordinate arrays of the
   * QMMMrec. These coordinates are passed on the QM subroutines.
   */
  int
    i;

  /* shift the QM atoms into the central box 
   */
  for(i=0;i<qm->nrQMatoms;i++)
    rvec_sub(x[qm->indexQM[i]],fr->shift_vec[qm->shiftQM[i]],qm->xQM[i]);
  
  /* also shift the MM atoms into the central box, if any 
   */
  for(i=0;i<mm->nrMMatoms;i++)
    rvec_sub(x[mm->indexMM[i]],fr->shift_vec[mm->shiftMM[i]],mm->xMM[i]);   

} /* update_QMMM_coord */

/* end of QMMM subroutines */

/* QMMM core routines */

t_QMrec *mk_QMrec(void){
  t_QMrec *qm;
  snew(qm,1);
  return qm;
} /* mk_QMrec */

t_MMrec *mk_MMrec(void){
  t_MMrec *mm;
  snew(mm,1);
  return mm;
} /* mk_MMrec */

void init_QMrec(int grpnr, t_QMrec *qm,int nr, int *atomarray, 
		t_mdatoms *md, t_inputrec *ir){
  /* fills the t_QMrec struct of QM group grpnr 
   */
  int
    i;


  qm->nrQMatoms = nr;
  snew(qm->xQM,nr);
  snew(qm->indexQM,nr);
  snew(qm->shiftQM,nr); /* the shifts */
  for(i=0;i<nr;i++){
    qm->indexQM[i]=atomarray[i];
  }

  snew(qm->atomicnumberQM,nr);
  for (i=0;i<qm->nrQMatoms;i++){
    qm->nelectrons       += md->nucnum[qm->indexQM[i]];  
    qm->atomicnumberQM[i] = md->nucnum[qm->indexQM[i]]; 
  }
  qm->QMcharge       = ir->opts.QMcharge[grpnr];
  qm->multiplicity   = ir->opts.QMmult[grpnr];
  qm->nelectrons    -= ir->opts.QMcharge[grpnr];

  qm->QMmethod       = ir->opts.QMmethod[grpnr];
  qm->QMbasis        = ir->opts.QMbasis[grpnr];
  /* trajectory surface hopping setup (Gaussian only) */
  qm->bSH            = ir->opts.bSH[grpnr];
  qm->CASorbitals    = ir->opts.CASorbitals[grpnr];
  qm->CASelectrons   = ir->opts.CASelectrons[grpnr];
  qm->SAsteps        = ir->opts.SAsteps[grpnr];
  qm->SAon           = ir->opts.SAon[grpnr];
  qm->SAoff          = ir->opts.SAoff[grpnr];
  /* hack to prevent gaussian from reinitializing all the time */
  qm->nQMcpus        = 0; /* number of CPU's to be used by g01, is set
			   * upon initializing gaussian
			   * (init_gaussian() 
			   */
  qm->bTS            = (ir->eI==eiTS);
  qm->bOPT           = (ir->eI==eiOPT);
  /* print the current layer to allow users to check their input */
  fprintf(stderr,"Layer %d\nnr of QM atoms %d\n",grpnr,nr);
  fprintf(stderr,"QMlevel: %s/%s\n\n",
	  eQMmethod_names[qm->QMmethod],eQMbasis_names[qm->QMbasis]);
  
} /* init_QMrec */  

t_QMrec *copy_QMrec(t_QMrec *qm)
{
  /* copies the contents of qm into a new t_QMrec struct */
  t_QMrec
    *qmcopy;
  int
    i;
  
  qmcopy = mk_QMrec();
  qmcopy->nrQMatoms = qm->nrQMatoms;
  snew(qmcopy->xQM,qmcopy->nrQMatoms);
  snew(qmcopy->indexQM,qmcopy->nrQMatoms);
  snew(qmcopy->atomicnumberQM,qm->nrQMatoms);
  snew(qmcopy->shiftQM,qmcopy->nrQMatoms); /* the shifts */
  for (i=0;i<qmcopy->nrQMatoms;i++){
    qmcopy->shiftQM[i]        = qm->shiftQM[i];
    qmcopy->indexQM[i]        = qm->indexQM[i];
    qmcopy->atomicnumberQM[i] = qm->atomicnumberQM[i];
  }
  qmcopy->nelectrons   = qm->nelectrons;
  qmcopy->multiplicity = qm->multiplicity;
  qmcopy->QMcharge     = qm->QMcharge;
  qmcopy->nelectrons   = qm->nelectrons;
  qmcopy->QMmethod     = qm->QMmethod; 
  qmcopy->QMbasis      = qm->QMbasis;  
  /* trajectory surface hopping setup (Gaussian only) */
  qmcopy->bSH          = qm->bSH;
  qmcopy->CASorbitals  = qm->CASorbitals;
  qmcopy->CASelectrons = qm->CASelectrons;
  qmcopy->SAsteps      = qm->SAsteps;
  qmcopy->SAon         = qm->SAon;
  qmcopy->SAoff        = qm->SAoff;
  /* Gaussian init. variables */
  qmcopy->nQMcpus      = qm->nQMcpus;
  for(i=0;i<DIM;i++)
    qmcopy->SHbasis[i] = qm->SHbasis[i];
  qmcopy->QMmem        = qm->QMmem;
  qmcopy->accuracy     = qm->accuracy;
  qmcopy->cpmcscf      = qm->cpmcscf;
  qmcopy->SAstep       = qm->SAstep;

  return(qmcopy);

} /*copy_QMrec */

t_QMMMrec *mk_QMMMrec(void)
{

  t_QMMMrec *qr;

  snew(qr,1);

  return qr;

} /* mk_QMMMrec */

void init_QMMMrec(t_commrec *cr,
		  t_mdatoms *md,
		  matrix box,
		  t_topology *top,
		  t_inputrec *ir,
		  t_QMMMrec *qr)
{
  /* we put the atomsnumbers of atoms that belong to the QMMM group in
   * an array that will be copied later to QMMMrec->indexQM[..]. Also
   * it will be used to create an QMMMrec->bQMMM index array that
   * simply contains true/false for QM and MM (the other) atoms.
   */

  atom_id 
    *qm_arr=NULL,dummy,ai,aj;
  int     
    qm_max=0,qm_nr=0,i,j,jmax,k,l,nrdum=0;
  t_MMrec 
    *mm;
  t_iatom 
    *iatoms;

  /* make a local copy of the DUMMY2 iatoms array 
   */
  iatoms = top->idef.il[F_DUMMY2].iatoms;
  nrdum  = top->idef.il[F_DUMMY2].nr;
  /* bQMMM[..] is an array containing TRUE/FALSE for atoms that are
   * QM/not QM. We first set all elemenst at false. Afterwards we use
   * the qm_arr (=MMrec->indexQM) to changes the elements
   * corresponding to the QM atoms at TRUE.  */
  snew(qr->bQMMM,md->nr);
  for (i=0;i<md->nr;i++)
    qr->bQMMM[i]=FALSE;

  qr->QMMMscheme     = ir->QMMMscheme;

  /* we take the possibility into account that a user has
   * defined more than one QM group:
   */
  /* an ugly work-around in case there is only one group In this case
   * the whole system is treated as QM. Otherwise the second group is
   * always the rest of the total system and is treated as MM.  
   */

  /* small problem if there is only QM.... so no MM */
  
  jmax = ir->opts.ngQM;

  if(qr->QMMMscheme==eQMMMschemeoniom)
    qr->nrQMlayers = jmax;
  else
    qr->nrQMlayers = 1; 

  /* there are jmax groups of QM atoms. In case of multiple QM groups
   * I assume that the users wants to do ONIOM. However, maybe it
   * should also be possible to define more than one QM subsystem with
   * independent neighbourlists. I have to think about
   * that.. 11-11-2003 
   */
  snew(qr->qm,jmax);
  for(j=0;j<jmax;j++){
    /* new layer */
    for (i=0;i<md->nr;i++){
      if(qm_nr >= qm_max){
	qm_max += 1000;
	srenew(qm_arr,qm_max);
      }
      if(md->cQMMM[i]==j){
	qm_arr[qm_nr++]=i;
      }
    }
    if(qr->QMMMscheme==eQMMMschemeoniom){
      /* add the atoms to the bQMMM array
       */

      /* I assume that users specify the QM groups from small to
       * big(ger) in thge mdp file 
       */
      for (k=0;k<qm_nr;k++)
	qr->bQMMM[qm_arr[k]]=TRUE;
      
      qr->qm[j] = mk_QMrec(); 
      /* we need to throw out link atoms that in the previous layer
       * existed to separate this QMlayer from the previous
       * QMlayer. We use the iatoms array in the idef for that
       * purpose. If all atoms defining the current Link Atom (Dummy2)
       * are part of the current QM layer it needs to be removed from
       * qm_arr[].  */
      
      for(k=0;k<nrdum;k+=4){
	dummy=iatoms[k+1]; /* the dummy */
	ai=iatoms[k+2]; /* the atoms that construct */
	aj=iatoms[k+3]; /* the dummy */
	if(qr->bQMMM[dummy] && qr->bQMMM[aj] && qr->bQMMM[ai]){
	  /* this dummy link atom needs to be removed from the qm_arr
	   * before making the QMrec of this layer!  
	   */
	  for(i=0;i<qm_nr;i++){
	    if(qm_arr[i]==dummy){
	      /* drop the element */
	      for(l=i;l<qm_nr;l++){
		qm_arr[l]=qm_arr[l+1];
	      }
	      qm_nr--;
			
	    }
	  }
	}	
      }  
      /* store QM atoms in this layer in the QMrec and initialise layer 
       */
      init_QMrec(j,qr->qm[j],qm_nr,qm_arr,md,ir);
      

    
      
      /* reset counter for next layer
       */
      /*      qm_nr = 0;*/
    }
  }
  if(qr->QMMMscheme!=eQMMMschemeoniom){

    /* standard QMMM, all layers are merged together so there is one QM 
     * subsystem and one MM subsystem. 
     * Also we set the charges to zero in the md->charge arrays to prevent 
     * the innerloops from doubly counting the electostatic QM MM interaction
     */
    for (k=0;k<qm_nr;k++){
      qr->bQMMM[qm_arr[k]]=TRUE;
      md->chargeT[qm_arr[k]]=0.0;
      md->chargeA[qm_arr[k]]=0.0;
      md->chargeB[qm_arr[k]]=0.0;
    } 
    qr->qm[0] = mk_QMrec();
    /* store QM atoms in the QMrec and initialise
     */
    init_QMrec(0,qr->qm[0],qm_nr,qm_arr,md,ir);



    /* MM rec creation */
    mm               = mk_MMrec(); 
    mm->scalefactor  = ir->scalefactor;
    mm->nrMMatoms    = md->nr-qr->qm[0]->nrQMatoms; /* rest of the atoms */
    qr->mm           = mm;
  } else {/* ONIOM */
    /* MM rec creation */    
    mm               = mk_MMrec(); 
    mm->scalefactor  = ir->scalefactor;
    mm->nrMMatoms    = 0;
    qr->mm           = mm;
  }
  
  /* these variables get updated in the update QMMMrec */

  if(qr->nrQMlayers==1){
    /* with only one layer there is only one initialisation
     * needed. Multilayer is a bit more complicated as it requires
     * re-initialisation at every step of the simulation. This is due
     * to the use of COMMON blocks in the fortran QM subroutines.  
     */
#ifdef USE_MOPAC
    if (qr->qm[0]->QMmethod<eQMmethodRHF){
      /* semi-empiprical 1-layer ONIOM calculation requested (mopac93) 
       */
      init_mopac(cr,qr->qm[0],qr->mm);
    }
    else 
#endif
      { 
      /* ab initio calculation requested (gamess/gaussian) 
       */
#ifdef USE_GAMESS
	init_gamess(cr,qr->qm[0],qr->mm);
#else
	init_gaussian(cr,qr->qm[0],qr->mm);
#endif
      }
    
  }
} /* init_QMMMrec */

void update_QMMMrec(t_commrec *cr,
		    t_forcerec *fr,
		    rvec x[],
		    t_mdatoms *md,
		    matrix box,
		    t_topology *top)
{
  /* updates the coordinates of but QM atoms and MM atoms and stores
   * them in the QMMMrec.  
   *
   * NOTE: is NOT yet working if there are no PBC. Also in ns.c, simple
   * ns needs to be fixed!  
   */
  int 
    mm_max=0,mm_nr=0,mm_nr_new,i,j,is;
  t_j_particle 
    *mm_j_particles=NULL,*qm_i_particles=NULL;
  t_QMMMrec 
    *qr; 
  t_nblist 
    QMMMlist_sr,QMMMlist_lr;
  rvec
    dx,crd;
  int
    *MMatoms;
  t_QMrec
    *qm;
  t_MMrec
    *mm;
  int  
    *parallelMMarray=NULL;
  /* every cpu has this array. On every processor we fill this array
   * with 1's and 0's. 1's indicate the atoms is a QM atom on the
   * current cpu in a later stage these arrays are all summed. indexes
   * > 0 indicate the atom is a QM atom. Every node therefore knows
   * whcih atoms are part of the QM subsystem.  
   */
  /* copy some pointers */
  qr          = fr->qr;
  mm          = qr->mm;
  QMMMlist_sr = fr->QMMMlist_sr;
  QMMMlist_lr = fr->QMMMlist_lr;

  

  init_pbc(box); /* needs to be called first, see pbc.h */
  
  /* only in standard (normal) QMMM we need the neighbouring MM
   * particles to provide a electric field of point charges for the QM
   * atoms.  
   */
  if(qr->QMMMscheme==eQMMMschemenormal){ /* also implies 1 QM-layer */
    /* we NOW create/update a number of QMMMrec entries:
     *
     * 1) the shiftQM, containing the shifts of the QM atoms
     *
     * 2) the indexMM array, containing the index of the MM atoms
     * 
     * 3) the shiftMM, containing the shifts of the MM atoms
     *
     * 4) the shifted coordinates of the MM atoms
     *
     * the shifts are used for computing virial of the QM/MM particles.
     */
    qm = qr->qm[0]; /* in case of normal QMMM, there is only one group */
    snew(qm_i_particles,QMMMlist_sr.nri);
    if(QMMMlist_sr.nri){
      qm_i_particles[0].shift = XYZ2IS(0,0,0);
      for(i=0;i<QMMMlist_sr.nri;i++){
	qm_i_particles[i].j     = QMMMlist_sr.iinr[i];
	if(i){
	  qm_i_particles[i].shift = pbc_dx(x[QMMMlist_sr.iinr[0]],
					   x[QMMMlist_sr.iinr[i]],dx);

	}
	/* However, since nri >= nrQMatoms, we do a quicksort, and throw
	 * out double, triple, etc. entries later, as we do for the MM
	 * list too.  
	 */
	
	/* compute the shift for the MM j-particles with respect to
	 * the QM i-particle and store them. 
	 */
	
	crd[0] = IS2X(QMMMlist_sr.shift[i]) + IS2X(qm_i_particles[i].shift);
	crd[1] = IS2Y(QMMMlist_sr.shift[i]) + IS2Y(qm_i_particles[i].shift);
	crd[2] = IS2Z(QMMMlist_sr.shift[i]) + IS2Z(qm_i_particles[i].shift);
	is = XYZ2IS(crd[0],crd[1],crd[2]); 
	for(j=QMMMlist_sr.jindex[i];
	    j<QMMMlist_sr.jindex[i+1];
	    j++){
	  if(mm_nr >= mm_max){
	    mm_max += 1000;
	    srenew(mm_j_particles,mm_max);
	  }	  
	  
	  mm_j_particles[mm_nr].j = QMMMlist_sr.jjnr[j];
	  mm_j_particles[mm_nr].shift = is;
	  mm_nr++;
	}
      }
      
      /* quicksort QM and MM shift arrays and throw away multiple entries */
      
      qsort(qm_i_particles,QMMMlist_sr.nri,
	    (size_t)sizeof(qm_i_particles[0]),
	    struct_comp);
      qsort(mm_j_particles,mm_nr,
	    (size_t)sizeof(mm_j_particles[0]),
	    struct_comp);

      /* remove multiples in the QM shift array, since in init_QMMM() we
       * went through the atom numbers from 0 to md.nr, the order sorted
       * here matches the one of QMindex already.
       */
      j=0;
      for(i=0;i<QMMMlist_sr.nri;i++){
	if (i==0 || qm_i_particles[i].j!=qm_i_particles[i-1].j){
	  qm_i_particles[j++] = qm_i_particles[i];
	}
      }
      /* we also remove mm atoms that have no charges! 
       * actually this is already done in the ns.c  
       */
      mm_nr_new = 0;
      for(i=0;i<mm_nr;i++){
	if((i==0 || mm_j_particles[i].j!=mm_j_particles[i-1].j)
	   && !qr->bQMMM[mm_j_particles[i].j] 
	   && md->chargeT[mm_j_particles[i].j]){
	  mm_j_particles[mm_nr_new++] = mm_j_particles[i];
	}
      }
      mm_nr = mm_nr_new;
      
      /* store the data retrieved above into the QMMMrec
       */    
      for(i=0;i<qm->nrQMatoms;i++){
	qm->shiftQM[i]=qm_i_particles[i].shift;
      }
    }
    /* parallel excecution */
    if(PAR(cr)){
      snew(parallelMMarray,2*(md->nr)); 
      /* only MM particles have a 1 at their atomnumber. The second part
       * of the array contains the shifts. Thus:
       * p[i]=1/0 depending on wether atomnumber i is a MM particle in the QM
       * step or not. p[i+md->nr] is the shift of atomnumber i.
       */
      for(i=0;i<2*(md->nr);i++){
	parallelMMarray[i]=0;
      }
      
      for(i=0;i<mm_nr;i++){
	parallelMMarray[mm_j_particles[i].j]=1;
	parallelMMarray[mm_j_particles[i].j+(md->nr)]=mm_j_particles[i].shift;
      }
      gmx_sumi(md->nr,parallelMMarray,cr);
      mm_nr=0;
      
      mm_max = 0;
      for(i=0;i<md->nr;i++){
	if(parallelMMarray[i]){
	  if(mm_nr >= mm_max){
	    mm_max += 1000;
	    srenew(mm->indexMM,mm_max);
	    srenew(mm->shiftMM,mm_max);
	  }
	  mm->indexMM[mm_nr]  = i;
	  mm->shiftMM[mm_nr++]= parallelMMarray[i+md->nr]/parallelMMarray[i];
	}
      }
      mm->nrMMatoms=mm_nr;
      free(parallelMMarray);
    }
    /* serial execution */
    else{
      mm->nrMMatoms = mm_nr;
      srenew(mm->shiftMM,mm_nr);
      srenew(mm->indexMM,mm_nr);
      for(i=0;i<mm_nr;i++){
	mm->indexMM[i]=mm_j_particles[i].j;
	mm->shiftMM[i]=mm_j_particles[i].shift;
      }
      
    }
    /* (re) allocate memory for the MM coordiate array. The QM
     * coordinate array was already allocated in init_QMMM, and is
     * only (re)filled in the update_QMMM_coordinates routine 
     */
    srenew(mm->xMM,mm->nrMMatoms);
    
    /* now we (re) fill the array that contains the MM charges with
     * the forcefield charges. If requested, these charges will be
     * scaled by a factor 
     */
    srenew(mm->MMcharges,mm->nrMMatoms);
    for(i=0;i<mm->nrMMatoms;i++)/* no free energy yet */
      mm->MMcharges[i]=md->chargeT[mm->indexMM[i]]*mm->scalefactor; 
    
    /* the next routine fills the coordinate fields in the QMMM rec of
     * both the qunatum atoms and the MM atoms, using the shifts
     * calculated above.  
     */
    update_QMMM_coord(x,fr,qr->qm[0],qr->mm);
    free(qm_i_particles);
    free(mm_j_particles);
  } 
  else { /* ONIOM */ /* ????? */
    mm->nrMMatoms=0;
    /* do for each layer */
    for (j=0;j<qr->nrQMlayers;j++){
      qm = qr->qm[j];
      qm->shiftQM[0]=XYZ2IS(0,0,0);
      for(i=1;i<qm->nrQMatoms;i++){
	qm->shiftQM[i] = pbc_dx(x[qm->indexQM[0]],x[qm->indexQM[i]],dx);
      }
      update_QMMM_coord(x,fr,qm,mm);    
    }
  }
} /* update_QMMM_rec */


real calculate_QMMM(t_commrec *cr,
		    rvec x[],rvec f[],
		    t_forcerec *fr,
		    t_mdatoms *md)
{
  real
    QMener=0.0;
  /* a selection for the QM package depending on which is requested
   * (Gaussian, GAMESS-UK or MOPAC) needs to be implemented here. Now
   * it works through defines.... Not so nice yet 
   */
  t_QMMMrec
    *qr;
  t_QMrec
    *qm,*qm2;
  t_MMrec
    *mm=NULL;
  rvec 
    *forces=NULL,*fshift=NULL,    
    *forces2=NULL, *fshift2=NULL; /* needed for multilayer ONIOM */
  int
    i,j,k;
  /* make a local copy the QMMMrec pointer 
   */
  qr = fr->qr;
  mm = qr->mm;

  /* now different procedures are carried out for one layer ONION and
   * normal QMMM on one hand and multilayer oniom on the other
   */
  if(qr->QMMMscheme==eQMMMschemenormal || qr->nrQMlayers==1){
    qm = qr->qm[0];
    snew(forces,(qm->nrQMatoms+mm->nrMMatoms));
    snew(fshift,(qm->nrQMatoms+mm->nrMMatoms));
    QMener = call_QMroutine(cr,fr,qm,mm,forces,fshift);
    for(i=0;i<qm->nrQMatoms;i++){
      for(j=0;j<DIM;j++){
	f[qm->indexQM[i]][j]          -= forces[i][j];
	fr->fshift[qm->shiftQM[i]][j] += fshift[i][j];
      }
    }
    for(i=0;i<mm->nrMMatoms;i++){
      for(j=0;j<DIM;j++){
	f[mm->indexMM[i]][j]          -= forces[qm->nrQMatoms+i][j];
	fr->fshift[mm->shiftMM[i]][j] += fshift[qm->nrQMatoms+i][j];
      }
      
    }
  }
  else{ /* Multi-layer ONIOM */
    for(i=0;i<qr->nrQMlayers-1;i++){ /* last layer is special */
      qm  = qr->qm[i];
      qm2 = copy_QMrec(qr->qm[i+1]);

      qm2->nrQMatoms = qm->nrQMatoms;
    
      for(j=0;j<qm2->nrQMatoms;j++){
	for(k=0;k<DIM;k++)
	  qm2->xQM[j][k]       = qm->xQM[j][k];
	qm2->indexQM[j]        = qm->indexQM[j];
	qm2->atomicnumberQM[j] = qm->atomicnumberQM[j];
	qm2->shiftQM[j]        = qm->shiftQM[j];
      }

      qm2->QMcharge = qm->QMcharge;
      /* this layer at the higher level of theory */
      srenew(forces,qm->nrQMatoms);
      srenew(fshift,qm->nrQMatoms);
      /* we need to re-initialize the QMroutine every step... */
      init_QMroutine(cr,qm,mm);
      QMener += call_QMroutine(cr,fr,qm,mm,forces,fshift);

      /* this layer at the lower level of theory */
      srenew(forces2,qm->nrQMatoms);
      srenew(fshift2,qm->nrQMatoms);
      init_QMroutine(cr,qm2,mm);
      QMener -= call_QMroutine(cr,fr,qm2,mm,forces2,fshift2);
      /* E = E1high-E1low The next layer includes the current layer at
       * the lower level of theory, which provides + E2low
       * this is similar for gradients
       */
      for(i=0;i<qm->nrQMatoms;i++){
	for(j=0;j<DIM;j++){
	  f[qm->indexQM[i]][j]          -= (forces[i][j]-forces2[i][j]);
	  fr->fshift[qm->shiftQM[i]][j] += (fshift[i][j]-fshift2[i][j]);
	}
      }
      free(qm2);
    }
    /* now the last layer still needs to be done: */
    qm      = qr->qm[qr->nrQMlayers-1]; /* C counts from 0 */
    init_QMroutine(cr,qm,mm);
    srenew(forces,qm->nrQMatoms);
    srenew(fshift,qm->nrQMatoms);
    QMener += call_QMroutine(cr,fr,qm,mm,forces,fshift);
    for(i=0;i<qm->nrQMatoms;i++){
      for(j=0;j<DIM;j++){
	f[qm->indexQM[i]][j]          -= forces[i][j];
	fr->fshift[qm->shiftQM[i]][j] += fshift[i][j];
      }
    }
  }
  if(qm->bTS||qm->bOPT){
    /* qm still contains the largest ONIOM QM subsystem 
     * we taket the optimized coordiantes and put the in x[]
     */
    for(i=0;i<qm->nrQMatoms;i++){
      for(j=0;j<DIM;j++){
	x[qm->indexQM[i]][j] = qm->xQM[i][j];
      }
    }
  }
  return(QMener);
} /* calculate_QMMM */

/* end of QMMM core routines */
