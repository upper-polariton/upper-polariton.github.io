      subroutine gmx
      implicit REAL  (a-h,p-w),integer   (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
INCLUDE(common/segm)
INCLUDE(common/vcore)
INCLUDE(common/sizes)
INCLUDE(common/maxlen)
INCLUDE(common/errcodes)

_IF(chemshell,charmm)
INCLUDE(common/iofile)
INCLUDE(common/work)
INCLUDE(common/chemshell)
INCLUDE(common/junkc)
_ENDIF
_IF(linux)
      common/handle/handle,idum,ioff
_ENDIF
_IF(charmm)
INCLUDE(common/chmgms)
      logical qinigm2
_ENDIF
_IF(drf)
cdrf------------------------
INCLUDE(../drf/comdrf/sizesrf)
INCLUDE(../drf/comdrf/darw)
      common/nottwi/obeen,obeen2,obeen3,obeen4
INCLUDE(../drf/comdrf/rfene)
INCLUDE(../drf/comdrf/enrgci)
INCLUDE(../drf/comdrf/drfzfa)
INCLUDE(../drf/comdrf/runpar)
INCLUDE(../drf/comdrf/drfbem)
      common/bufb/kstart(7,mxshel),nshell,nuc,norb,ispace
cdrf-------------------------
_ENDIF
INCLUDE(common/drfopt)
INCLUDE(common/zjorgs)
      common/jorgs/maxj,irecal,opowel,obfgs,obfgx,onrfo,ocut,
     *outjor,outdeb,maxsta
INCLUDE(common/modj)
INCLUDE(common/modmin)
c ...
      common/restrl/ociopt(2),omp2,ogr(13),
     + oforce,oci,ocart(32)
INCLUDE(common/cndx40)
INCLUDE(common/cndx41)
INCLUDE(common/iofile)
INCLUDE(common/dump3)
INCLUDE(common/utilc)
INCLUDE(common/restar)
INCLUDE(common/restrj)
INCLUDE(common/statis)
INCLUDE(common/machin)
INCLUDE(common/runlab)
      common/restrz/ztttt(11),zzzzzz,zzspac(38)
INCLUDE(common/direc)
INCLUDE(common/timez)
INCLUDE(common/jinfo)
INCLUDE(common/work)
INCLUDE(common/prnprn)
INCLUDE(common/infoa)
INCLUDE(common/atmol3)

_IF(ccpdft)
INCLUDE(common/ccpdft.hf77)
INCLUDE(common/blur)
_ENDIF

_IF(charmm)
INCLUDE(common/chmgms)
_ENDIF

      dimension zcas(2)
_IF(drf)
cdrf------------------------
      dimension dumm(mxgran), dumx(mxgrpar)
cdrf------------------------
_ENDIF
c     hack
      integer ioff, handle, idum
      
      data zprop/'prop'/
      data zgame/' gamess'/ , zcas/'casscf','mcscf'/
c     starten maar:

      write(6,*)'calling gamess'
      call gmxgms

c     setup the gradient job:

      call cpuwal(tstart,estart)
c
_IF(drf)
      eci(1) = 0.d00
c
_ENDIF
      zcom(1) = zanam
      zcom(2) = zdate
      zcom(3) = ztime
      zcom(4) = zgame
      zcom(6) = zaccno
c
      call inpa4(ytext)
c
      if (ytext.eq.'util' .or. ytext.eq.'serv') then
         call gserv(qq(2+ioff),oquit)
         if (oquit) go to 275
      end if
      jrec = 0
      oresti = .false.
      oterm = .false.
      ofirt = .true.
      oprop = .false.
c
c     ----- start- read in basis set and get initial mo*s -----
c
 20   call start(qq(2+ioff),oresti,ostop,ostopr)
c
c punch requests table in output file
c
      call blktab(ostopr)
c
c write initial data to punchfile
c
      call blk11(qq(2+ioff))
c
      if (ostop) then
c
c  end of this run
c
         if (ostopr) then
c
c user requested stop ... punch any available information
c on the dumpfile
c
            call blk(qq(2+ioff))
 
            write (iwr,7000)
 7000       format (/' program termination requested'/)
 
         endif
 
         call closda

         go to 275
      end if
c
c      ------ check system is small enough to treat
c
      call chksiz(.false.)
c
_IF(ccpdft)
c
c initialise CCP1 DFT module
c
      idum = CD_init(qq(2+ioff),iwr)
_ENDIF

_IF(charmm)
c
c Initialise external charge code, this 
c defines the geometry and AO basis
c
      idum =  gden_init(qq(2+ioff),iwr)
c
c Store external charge distribution
c
      call gden_add_gauss(nat,iwr)

_ELSEIF(ccpdft)
      if(oblur)then
c
c set up the external gaussian distribution
c
         if(nbltyp .ne. 0)then
c
c input via atom labels (blur directive)
c            
            do i = 1, nat
               blexpo(i) = -1.0d0
               blwght(i) =  0.0d0
               do j = 1, nbltyp
                  if(zaname(i) .eq. ztagbl(j))then
                     blexpo(i) = blexpo2(j)
                     blwght(i) = blwght2(j)
                  endif
               enddo
            enddo
         else
c
c input via blur keyword on geometry block
c
            do i = 1, nat
               write(6,*)'atom',i,'blur', blexpo(i), blwght(i)
            enddo
         endif
         idum = gden_init(qq(2+ioff),iwr)
         call gden_add_gauss(nat,iwr)
      endif
_ENDIF


c
c     ----- get initial m.o.s
c
      if (zruntp.ne.zrunt(10)) then
 
        call mogues(qq(2+ioff))
        write(6,*)'ostopm = ',ostopm
        if (ostopm) go to 260

c
c     === attempt to allocate memory for integral storage
c
_IFN1(i)        if(omem(3).and.ofirt) call ed2mem(num,nat)
_IF1(i)        if(omem(3).and.ofirt) 
_IF1(i)     + call caserr('in-core integral option not available')
      endif
      irun = locatc(zrunt,mxtask,zruntp)
c
      osymm = .true.
      omcscf = zscftp.eq.zcas(1) .or. zscftp.eq.zcas(2)
      oprop = .false.
      
      write(6,*)'irun is: ',irun
      icount = 1

c     optimize molecular geometry (optxyz)
c     single point gradient evaluation
c
      goto 275
      oprop = iprop.lt.0
 250  if (.not.odirpa) call closbf
      go to 270
 260  if (zscftp.eq.zcas(1) .or. zscftp.eq.zcas(2)) go to 250
 270  oresti = .true.
      itask(mtask) = irest
      call revise
c
c  flush incore files to disk (GA ed3 only at present)
      call ioupdate
c
      go to 20

c     here gmx takes over!
 275  return
      end



c     the gmxopt code
      subroutine grdgms(core,nrqmat,qmcrd,nrmmat,mmchrg,mmcrd,qmgrad,
     +                  mmgrad,qmmmen)
c
c     minimization of the energy by varying the geometry
c     of the molecule using murtagh and sargent algorithm
c
c     this is an updating routine which successively
c     generates the hessian matrix (i.e. the inverse of the
c     matrix of second derivatives) after a successful one
c     dimensional search.
c
      implicit REAL  (a-h,p-w),integer   (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      real qmcrd,mmchrg,mmcrd,qmgrad,mmgrad,qmmmen
      integer nrqmat,nrmmat
      dimension qmcrd(*),mmchrg(*),mmcrd(*),qmgrad(*),mmgrad(*)      

INCLUDE(common/sizes)
INCLUDE(common/machin)
INCLUDE(common/cntl1)
INCLUDE(common/iofile)
INCLUDE(common/dump3)
INCLUDE(common/restar)
INCLUDE(common/restri)
INCLUDE(common/runlab)
INCLUDE(common/infoa)
      common/miscop/p(maxat*3),g(maxat*3),dx(maxat*3),func,
     +              alpha,cz(maxat*3),q(maxat*3),cc,gnrm
INCLUDE(common/funct)
INCLUDE(common/seerch)
INCLUDE(common/timez)
INCLUDE(common/copt)
INCLUDE(common/prnprn)
      common/runopt/zopti(maxat)
_IF(drf)
      common/nottwi/obeen,obeen2,obeen3,obeen4
INCLUDE(../drf/comdrf/drfpar)
INCLUDE(common/drfopt)
_ENDIF
      dimension core(*)
      data done/1.0d0/
      data m17/17/
      data zgrad/'gradient'/
      data zirc /'irc'/

      orset = .false.
      nprino = nprint
      tolg = rcvopt

      nat = nrqmat+nrmmat
      if(nat.gt.maxat) then
      write(6,*) 'too many particles'
      return
      endif
      do iat=1,nrqmat
      do igg=1,3
      c(igg,iat)=qmcrd(3*(iat-1)+igg)
      enddo
      enddo
      do iat=nrqmat+1,nat
      do igg=1,3
      c(igg,iat)=mmcrd(3*(iat-nrqmat-1)+igg)
      enddo
      czan(iat)=mmchrg(iat-nrqmat)
      symz(iat)=mmchrg(iat-nrqmat)
      nuct(iat)=3
      imass(iat)=0
      amasold(iat)=0.0
      ipseud(iat)=0
      zaname(iat)(1:8)='       '
      zaname(iat)(1:2)='bq'
      enddo    

      bigg0 = 0.0d0
      bigg = 0.0d0
      npts = -1


c
c ----- allocate gradient section on dumpfile
c
       write (6,*)'number of atoms (nat) = ',nat
       ncoord = 3*nat
       nc2 = ncoord*ncoord
      isize = lensec(nc2) + lensec(mach(7))
      call secput(isect(495),m17,isize,ibl3g)
      ibl3hs = ibl3g + lensec(mach(7))
c
c     ----- check for a restart run -----
c
c      write (6,*)'orstrt is ',orstrt
c
c      write(6,*)'modified atoms in optx:'
c      do 24 iat=1,nat
c	   write (6,*)'---------'
c      write (6,*)'atom ',iat,' ',(c(igg,iat),igg=1,3),' ',(czan(iat))
c 24   continue

c      nserch = 0
      enrgy = 0.0d0
      func = 0.0d0


      call vclr(dx,1,ncoord)
      call vclr(egrad,1,ncoord)
      call dcopy(ncoord,c(1,1),1,p,1)

      call vclr(g,1,ncoord)
c      gs0 = 0.0d0
      call valopt(core,qmener)
      write(6,*)'qmener in gmx = ',qmener

c      gnrm = dnrm2(ncoord,g,1)
c      alpha = 0.0d0
c      iupdat = 0
c      icode = 2
      icode = 3
c_IF1(v)      bigg = 0.0d0
c_IF1(v)      do 1 i = 1,ncoord
c_IF1(v)      dum =  dabs(g(i))
c_IF1(v)      if (dum .gt. bigg) bigg = dum
c_IF1(v)    1 continue
c_IF1(u)      bigg=absmax(ncoord,0.0d0,g)
c_IF1(f)      call maxmgv(g,1,bigg,loop,ncoord)
_IFN1(fuv)      i=idamax(ncoord,g,1)
_IFN1(fuv)      bigg=dabs(g(i))


c-dbg f

c
c     ----- save restart data -----
c

c      call wrrec1(nserch,npts,iupdat,icode,p,g,func0,gs0,alpha,dx,p)
c      if (tim.ge.timlim) return
c      if (bigg.le.tolg) icode = 3
c      if (zruntp.eq.zgrad) icode = 3
c      write(6,*)'icode in optx = ',icode
c      go to 40
c 20   alpha = done
c_IF(drf)
c      if (irepeat .eq. 1) then
c        alpha = -alpha
c      endif
c_ENDIF
c
c      call makdx(g,dx,ncoord,core)
c
c     ----- symmetrize displacement vector -----
c
c      call symdr(dx,core)
c
c     ----- search for a minimum in the -dx- direction -----
c
c 30   call smsl(tolg,bigg,orstrt,orset,core)
c      write(6,*)'puffo'
c      if (irest.ne.0 .and. tim.ge.timlim) return
c      if (nserch.gt.mnls .or. npts.gt.jm) then
c         write (iwr,6020)
c      write (6,*),'we set icode from ',icode,'to 3'
c         icode = 3
c      end if
c_IF(drf)
c      if ((icode .eq. 2) .and. (irepeat .eq. 1)) then
c        write (iwr,6020)
c        icode = 3
c      endif
c_ENDIF
c 40   dfunc = func - func0
c
c
c
c     ----- print update data -----
c
c      if(icode.ne.4) then
c       write (iwr,6030) nserch , iupdat , npts , func , gnrm , alpha ,
c     +                icode , func0 , dfunc
c       write (iwr,6040)
c       do 45 iat = 1 , nat
c       n = 3*(iat-1)
c       if(zopti(iat).ne.'no') write (iwr,6050) iat ,
c     +        zaname(iat),czan(iat) , (p(n+i),i=1,3) ,   (g(n+i),i=1,3)
c 45    continue
c       write (iwr,6060) bigg , tolg , bigg0
c      endif
c      bigg0 = bigg
c      iupdat = iupdat + 1
c      nserch = nserch + 1
c      if (icode.lt.2) then
c
c     ----- update hessian matrix -----
c
cjk         call seta1(cz,cc,ncoord,core)
cjk Pass through the change in gradient in q
cjk and the step in cz
cjk
c         call seta1(q,cz,cc,ncoord,core)
c_IF(drf)
c         irepeat = 0
c_ENDIF
c         go to 20
c      else if (icode.eq.2) then
c
c     ----- start search -----
c
c         icode = 1
c         iupdat = 1
c         call seta0(ncoord,core)
c_IF(drf)
c         irepeat = irepeat + 1
c_ENDIF
c         go to 20
c      else if (icode.eq.4) then
c         write(iwr,6011)
c         return
c      else
c
c     ----- end of run -----
c
         write(6,*)'nu breekt mijn klomp!'
         nprint = nprino
c         write (iwr,6010)
c         if(oprn(28)) call anamom(core)
c         call intr(core)
c         call optend(core,nprint)
         qmmmen = qmener 
         write (6,*)'hallo zeg, qm ener = ',qmener
         do iat=1,nrqmat*3
         qmgrad(iat)=g(iat)
         enddo
         do iat=1+nrqmat*3,nat*3
         mmgrad(iat-nrqmat*3)=g(iat)
         enddo      
c ook weer even de intr aanzetten!!!
c
c     ----- wavefunction properties -----
c
         zruntp = 'prop'
         return
c      end if
 6010 format (//1x,129('=')/)
 6011 format (/10x,'Gradient evaluation for IRC finished')
 6020 format (/40x,27('*')/40x,'minimisation not converging'/40x,27('*')
     +        /)
 6030 format (/10x,'nserch  update   npts       func            ',
     +        'gnorm',8x,'  alpha   icode'/10x,i5,i8,i7,f17.8,f15.8,
     +        f14.5,i5//15x,'previous energy',f17.8/15x,
     +        '    convergence',f17.8)
 6040 format (1x,113('*')//30x,'coordinates (bohr)',32x,
     +        'gradient (hartree/bohr)'/8x,'atom     znuc',7x,'x',13x,
     +        'y',13x,'z',20x,'x',13x,'y',13x,'z'/1x,113('*')/)
 6050 format (i3,2x,a8,2x,f6.1,3f14.7,8x,3f14.7)
 6060 format (1x,113('*')//32x,'largest component of gradient ',f14.7,
     +        ' ( tolg = ',f14.7,' )'/32x,
     +        'previous largest component    ',f14.7)
 6070 format (40x,21('*')/40x,'geometry optimization'/40x,21('*')
     +        //' convergence threshold on gradient   = ',
     +        f10.5/' maximum number of calculations      = ',
     +        i10/' restriction distance on coordinates = ',f10.5)
 6071 format (' Updating hessian with BFGS ')
 6072 format (' Updating hessian with Murtagh Sargent ')
      end



c      orset = .false.
c      nprino = nprint
c      tolg = rcvopt
c      if (zruntp.eq.zirc)  icode = 4
c_IF(drf)
ccahv
c      irepeat = -1
ccahv
c_ENDIF
ccjk
cc     hack gg
c
c      write(6,*)'atoms at start of optx:'
c      nat = nrqmat+nrmmat
c      if(nat.gt.maxat) then
c      write(6,*) 'too many particles'
c      return
c      endif
c      do iat=1,nrqmat
c      do igg=1,3
c      c(igg,iat)=qmcrd(3*(iat-1)+igg)
c      enddo
c      enddo
c      do iat=nrqmat+1,nat
c      do igg=1,3
c      c(igg,iat)=mmcrd(3*(iat-nrqmat-1)+igg)
c      enddo
c      czan(iat)=mmchrg(iat-nrqmat)
c      symz(iat)=mmchrg(iat-nrqmat)
c      nuct(iat)=3
c      imass(iat)=0
c      amasold(iat)=0.0
c      ipseud(iat)=0
c      zaname(iat)(1:8)='       '
c      zaname(iat)(1:2)='bq'
c      enddo       
c
c      bigg0 = 0.0d0
c      bigg = 0.0d0
c      npts = -1


c
c ----- allocate gradient section on dumpfile
c
c      ncoord = 3*nat
c      nc2 = ncoord*ncoord
c      isize = lensec(nc2) + lensec(mach(7))
c      call secput(isect(495),m17,isize,ibl3g)
c      ibl3hs = ibl3g + lensec(mach(7))
cc
cc     ----- fresh start, evaluate first gradient -----
cc      
c
c
c      call vclr(dx,1,ncoord)
c      call vclr(egrad,1,ncoord)
c      call dcopy(ncoord,c(1,1),1,p,1)
c
c      call vclr(g,1,ncoord)
c      gs0 = 0.0d0
c      write (6,*)'calling valopt'
c      call valopt(core)
c      write (6,*)'return from calling valopt'
cc
cc     ----- end of run -----
cc
c      nprint = nprino
cc      call intr(core)
c      call optend(core,nprint)
cc
cc     ----- wavefunction properties -----
cc
c      do iat=1,nrqmat*3
c        qmgrad(iat)=g(iat)
c      enddo
c      do iat=1+nrqmat*3,nat*3
c         mmgrad(iat)=g(iat)
c      enddo
c      qmmmen = qmener
c      return
c      end
