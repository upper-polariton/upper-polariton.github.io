c 
c  $Author: psh $
c  $Date: 1999/07/30 12:48:38 $
c  $Locker:  $
c  $Revision: 1.49 $
c  $Source: /c/qcg/cvs/psh/GAMESS-UK/m4/parallel.m,v $
c  $State: Exp $
c  
c  NB - in this version pg_dgop/igop do not take a work argument
c
c ======================================================================
c
c  ** pg_begin : initialise parallel processing
c
c
      subroutine pg_begin
      implicit none
_IF(mpi)
      include 'mpif.h'
INCLUDE(common/mpidata)
_IF(dynamic)
c
c  this version assumes a process has been set aside for
c  global index serving
c
      integer stat(MPI_STATUS_SIZE) 
      logical more
      integer ierr1, ierr2, ierr3, ierr4, ierr, n, nw, i, k, iw, ifrom
      integer icode, ic
      Integer me

      logical debug
      data debug/.false./

      integer linfo
      parameter(linfo=2*max_processors)
      integer info(2,0:max_processors - 1)

      Integer union_membership_number( 0:max_processors - 2 )
      Integer all_group, the_union ! i.e. a group of workers

      logical test_verb
      write(6,*)'calling mpi_init'
C      call MPI_INIT(ierr)
      write(6,*)'called mpi_init'
      call push_verb(1)

      if(ierr.ne.0)call pg_errmsg('init',ierr)
*
*  Need to know how big the job is to set up the secondary
* communicator - I.J.B.
*
      call mpi_comm_size( MPI_COMM_WORLD, n , ierr1)
      call mpi_comm_rank( MPI_COMM_WORLD, me, ierr2)
      If( ierr1 .NE. 0 .OR .ierr2 .NE. 0 ) Then
         call caserr( 'bad initialisation' )
      End If
*
*  Set up The MPI_COMM_WORKERS communicator - I.J.B.
*
      Do i = 0, n - 2
         union_membership_number( i ) = i
      End Do

      Call MPI_comm_group( MPI_comm_world, all_group, ierr )
      If( ierr .NE. 0 ) Then
         Call caserr( 'bad initialisation of MPI_COMM_WORKERS' )
      End If

      Call MPI_group_incl( all_group, n - 1, union_membership_number,
     +                     the_union, ierr )
      If( ierr .NE. 0 ) Then
         Call caserr( 'bad initialisation of MPI_COMM_WORKERS' )
      End If

      Call MPI_comm_create( MPI_COMM_WORLD  , the_union, 
     +                      MPI_COMM_WORKERS, ierr )
      If( ierr .NE. 0 ) Then
         Call caserr( 'bad initialisation of MPI_COMM_WORKERS' )
      End If
c
c  first we must ensure that the communicators are
c  correct

****      call mpi_comm_size( MPI_COMM_WORKERS, nw, ierr1 )
      nw = n - 1
      If( me .NE. nw ) Then
         call mpi_comm_rank( MPI_COMM_WORKERS, iw, ierr2 )
      End If
      ierr4 = 0


*      If( ierr1 .NE. 0 .AND. ierr2 .NE. 0 .AND. me .EQ. nw ) Then
      If( me .EQ. nw ) Then
         do i = 0,max_processors - 1
            do k=1,2
               info(k,i) = 0
            enddo
         enddo

         write(6,*)'i am the global counter'
         write(6,*)'there are ',n-1,' workers'
c
c simple global counter
c icode = 1  counter request
c         2  serve an index
c         3  close down
c
         ic = 0
         more = .true.
         do while (more)
c
c   wait for a message from any node ...
c  
            if(debug)write(6,*)'post rcv 971'
            call mpi_recv  (icode ,1, MPI_INTEGER, MPI_ANY_SOURCE,
     &        971,MPI_COMM_WORLD,stat,ierr)
***            call MPI_GET_SOURCE(stat,ifrom)
            ifrom = stat( MPI_source )
            if(debug)write(6,*)'get code ',icode,' from ',ifrom
c
c   act on it
c
            if (ierr.ne.0)then
               call pg_errmsg('recv',ierr)
            else if (icode.eq.-1)then
c
c  counter reset
c
               ic = 0
*               call debugp('pg_dlbreset: global counter reset')
               call mpi_send  (icode ,1, MPI_INTEGER, ifrom,
     &              972,MPI_COMM_WORLD,ierr)

            else if (icode.eq.-2)then
c
c  end of while loop
c
               more = .false.
            else if (icode.eq.-3)then
c
c  stats call
c
               call MPI_SEND (info,linfo,MPI_INTEGER, ifrom, 972,
     &              MPI_COMM_WORLD,ierr)            
               if (ierr.ne.0) call pg_errmsg('send',ierr)

            else
c
c  increment and send
c
               ic = ic + 1
               call MPI_SEND (ic,1,MPI_INTEGER, ifrom, 972,
     &              MPI_COMM_WORLD,ierr)            
               if (ierr.ne.0) call pg_errmsg('send',ierr)
               info(1,ifrom) = info(1,ifrom) + 1
               info(2,ifrom) = info(2,ifrom) + icode
c
c  then skip chunked parameters
c
               ic = ic + icode - 1
               if(debug)write(6,*)'send index ',ic,' icode was',icode,
     &              'next ic',ic
            endif
         enddo

         icode = 0
         call MPI_FINALIZE(icode)

         call exit(0)
      else if(ierr4 .eq. 0)then
c
c normal worker process 
c
        call pg_init

        if(test_verb(1))then
            write(6,*)'dynamic lb: worker started'
        endif
      else
         call caserr('bad worker initialisation')
      endif
_ELSE
c
c MPI static load balancing version
c
      integer ierr
      integer n
      logical test_verb
      write(6,*)'calling stat mpi_init'
C      call MPI_INIT(ierr)
      write(6,*)'called stat mpi_init'
      call push_verb(1)
C     manually set ierr to zero. MPI_init will have caused crash before in gmx
      ierr=0
C
      write(6,*)'called push_verb. IERR=',ierr
      if(ierr.ne.0)call pg_errmsg('init',ierr)
*
*  Still need the workers communicator in certain cases - I.J.B.
*
      write(6,*)'calling MPI_comm_dup. IERR=',ierr
      Call MPI_comm_dup( MPI_COMM_WORLD, MPI_COMM_WORKERS, ierr )
      write(6,*)'called MPI_comm_dup. IERR=',ierr
      if(ierr.ne.0)call pg_errmsg('comm_dup',ierr)
      call pg_init
      write(6,*)'called pginit'
      if(test_verb(1))then
         write(6,*)'static lb: worker started'
      endif

_ENDIF
_ELSEIF(tcgmsg)
INCLUDE(common/parcntl)
      logical opg_root
      odebugp = .false.
      call debugp('pg_begin')
      call PBEGINF
      ga_initted = .false.
      call pg_init
_ELSEIF(nx)
       call pg_init
_ELSEIF(pvm)
c
c spawn remaining processes
c
      include '/tmp/pvm3/fpvm3.h'  
      integer mytid,stride,msgtag,nhost,ndform,ierr,next,isave
      integer nodes,tids,me,n1,parent,i,numt,dtid,speed
      integer islash, inonblank, k
      character*30 name(0:63),arch,num
      character*100 exefile
      logical abort 
      common/mypvm/nodes,tids(0:63),me 
      data stride/1/,msgtag/0/
      logical test_verb
      data nodes/-999/

c no messages by default
      call push_verb(1)

      call pvmfmytid (mytid)
      call pvmfparent (parent)
      call pvmfconfig (nhost,ndform,dtid,name(0),arch,speed,ierr) 
      call getarg (0,exefile) 
c  default no. nodes = no. hosts   + 1 for DLB
      nodes=nhost+1
      if(iargc() .eq. 1)then
         call getarg (1,num) 
         read(num,'(i3)',err=999) nodes
      endif
c  strip a directoryname from exefile
      inonblank = -1
      islash = -1
      do k=len(exefile),1,-1
         if(exefile(k:k) .ne. " " .and. inonblank .eq. -1)then
            inonblank = k
         else if(exefile(k:k) .eq. "/" .and. islash .eq. -1)then
            islash = k
         endif
      enddo
      if(inonblank .eq. len(exefile))then
         write(6,*)'name truncation error - use relative'
         write(6,*)'path or redimension mpi2pvm.f'
         call exit(-1)
      endif
      if(islash.eq.-1)islash=0
      exefile=exefile(islash+1:inonblank)
      write(6,*) ' exe file = ',exefile 
      n1=nodes-1 
      msgtag=2001 
      if(parent.eq.PvmNoParent) then 
         if(test_verb(1))then
         write(6,*) ' >>>>>>>>>>> start pvm root process <<<<<<<<<<<<<'
         write(6,*) ' exe file = ',exefile 
         write(6,*) ' no. hosts =',nhost 
         write(6,*) ' no. nodes =',nodes
         endif
         tids(0)=mytid 
         me=0
         if(n1 .ne. 0)then
c
c obtain the host list
           isave=0
           do i = 1, nhost-1
           call pvmfconfig (isave,ndform,dtid,name(i),arch,speed,ierr) 
           enddo

           write(6,*)'names'
           do i = 0, nhost-1
              write(6,*)i,name(i)
           enddo

c gdf: round-robin distribution of nodes over hosts 
           next = 0
            do i = 1, n1
	       next = next + 1
               if(next .eq. nhost)next = 0
               write(6,*)name(next)
               call pvmfspawn (exefile,PVMHOST,name(next),
     *              1,tids(i),numt)
               if(numt .ne. 1)then
                   write(6,*)'problem on host',name(next)
                   write(6,*)'error code ',ierr
                   call pg_errmsg('problem in process creation',-1)
               else
                  write(6,*)'spawned process',i,' on ',name(next)
               endif
            enddo
c
c  check for problem starting processes
            abort = .false.
            do i = 1, nodes-1
               if(tids(i) .le. 0)then
                  write(6,*)'bad tid',i,tids(i)
                  abort = .true.
               else
                  if(test_verb(4))write(6,*)'new process',i,tids(i)
               endif
            enddo
            if(abort)call pg_errmsg('problem in process creation',-1)
c
c broadcast node count
            call pvmfinitsend (PVMDEFAULT,ierr) 
            call pvmfpack (INTEGER4,nodes,1,stride,ierr) 
            call pvmfmcast (n1,tids(1),msgtag,ierr) 
c
c broadcast TID array
            call pvmfinitsend (PVMDEFAULT,ierr) 
            call pvmfpack (INTEGER4,tids(0),nodes,stride,ierr) 
            call pvmfmcast (n1,tids(1),msgtag,ierr) 
         endif
      else 
	 call pvmfrecv (parent,msgtag,ierr) 
	 call pvmfunpack (INTEGER4,nodes,1,stride,ierr) 
         n1 = nodes - 1
	 call pvmfrecv (parent,msgtag,ierr) 
	 call pvmfunpack (INTEGER4,tids(0),nodes,stride,ierr) 
	 do i=1,n1
            if(mytid.eq.tids(i)) me = i 
         enddo
         if(test_verb(1))then
         write(6,*) 
     &        ' >>>>>>>>>>> start pvm  process ',me,' <<<<<<<<<<<<<'
         write(6,*) ' exe file = ',exefile 
         write(6,*) ' no. hosts =',nhost 
         write(6,*) ' no. nodes =',nodes
         endif  
      endif  
c
c print host name
      if(test_verb(1))then
         call hostcc(name(1),len(name))
         write(6,*)'node ',me,' is on host ',name(1)
      endif
c ps - assume nothing is wrong..
      ierr = 0
      return
c
 999  continue
      call pg_errmsg('bad argument',-1)
_ELSE
c
c serial code
c
        call pg_init
_ENDIF
      return
      end
c
c ========================================================================
c
c subroutine pg_ga_begin : intialise GA storage
c
c  allocate memory for globals arrays. For machines without data servers
c  this is postponed till after initj to allow the memory to be set in
c  the input file
c
      subroutine  pg_ga_begin(size)
      implicit none
INCLUDE(common/parcntl)
INCLUDE(common/sizes)
      integer size
_IF(ga)
#include "mafdecls.fh"
      logical value, opg_root
      MA_INTEGER i1, i2, i3
      integer ipg_nodeid, ipg_nnodes

      if( .not. ga_initted) then
          call debugp('pg_ga_begin')
         i1 = MT_DBL
         i2 = 1
         i3 = size
         value = ma_init(i1,i2,i3)
         if(.not.value)call caserr('ma init')
         call ga_initialize()
      endif
      ga_initted = .true.
      call set_nodinf
_ENDIF
      return
      end
c
c this is to evade name clash between nnodes in common/nodinf
c and the tcgmsg function
c
      subroutine set_nodinf
      implicit none
INCLUDE(common/sizes)
INCLUDE(common/nodinf)
      integer ipg_nodeid, ipg_nnodes
      mpid = 0
      minode = ipg_nodeid()
      mihost = 0
      nnodes = ipg_nnodes()
      nodscr = 1
      return
      end
c
c ========================================================================
c
c   pg_init: set parameters needed for  node process
c 

      subroutine pg_init
      implicit none
c
INCLUDE(common/sizes)
INCLUDE(common/parallel)
INCLUDE(common/parcntl)
      integer ipg_nnodes
_IF(ga)
_IFN(t3d,ipsc,rs6000)
      integer nn, id, nodeid, nnodes
_ENDIF
_ENDIF
c
      call set_nodinf
c
      call debugp('pg_init')
c
c use parallel diag for n > num nodes
       idpdiag = max(ipg_nnodes(),200)
c
c use parallel linear algebra for n > 200
       idpdiis = 200
c
c    note that the ga orthog routine causes problems in
c    geometry optimisations .. this is now disabled in
c    start as a function of runtype until this is rationalised
       idporth = 200
c
       idpmult2 = 200
c aim for 40 chunks per scf
       ntchnk = 40
c limit on chunk size
       limchnk = -1
c dont do parallel tests
       iptest = 0
c
c I/O mode default is node zero i/o 
c screening out of parts of SCF to reduce comms
c is supressed pending further work
c
      ipiomode = IO_NZ
c
c dynamic lb counter
      icount_dlb = 0
c
c print parameter
c 
c 1 = root times
c 2 = all node times
c 3 = ga sizes
c
      iparapr = 1
c
      return
      end
c
c =================================================================
c
c  pg_pproc : print node information
c
_IF(parallel)
      subroutine pg_pproc
      implicit none
      character*30 host, user, mach
      integer  ibuff(91), liw, if, il, i, ipid, lenwrd
      REAL fac
      logical opg_root
      integer ipg_nnodes
INCLUDE(common/iofile)
INCLUDE(common/parcntl)

_IF(ipsc)
      if(opg_root())write(iwr,1000)ipg_nnodes()
 1000 format(//,40x,'iPSC/860 parallel implementation on ',
     &     i5,' nodes',/,40x,50('-'))
_ELSEIF(t3d)
      if(opg_root())write(iwr,1000)ipg_nnodes()
 1000 format(//,40x,'T3D parallel implementation on ',
     &     i5,' nodes',/,40x,50('-'))
_ELSEIF(ksr)
      if(opg_root())write(iwr,1000)ipg_nnodes()
 1000 format(//,40x,'KSR parallel implementation on ',
     &     i5,' nodes',/,40x,50('-'))
_ELSE
      mach=
_IF1(x)     *'CONVEX  version 6.2'
_IF1(a)     *'ALLIANT version 6.2'
_IF1(s)     *'  SUN   version 6.2'
_IF1(p)     *'APOLLO  version 6.2'
_IF1(g)     *'  SGI   version 6.2'
_IF1(b)     *'HP-700  version 6.2'
_IF1(d)     *'  DEC   version 6.2'
_IF1(r)     *'RS-6000 version 6.2'
_IF1(t)     *' TITAN  version 6.2'
_IF1(c)     *'UNICOS  version 6.2'
_IF1(k)     *' KSR-2  version 6.2'
_IF1(G)     *'Generic version 6.2'
c
c cluster
c
      call hostcc(host,30)
      call namcc(user,30)
      call pidcc(ipid)
      call getload(fac)
      liw = 8/lenwrd()
      if(opg_root())then
         write(iwr,1000)
         write(iwr,1001)ipid,host(1:12),user(1:12),mach,fac
         do 10 i=1,ipg_nnodes()-1
            call pg_rcv(100,ibuff,91*liw,il,i,if,1)
            call pg_rcv(100,fac,8,il,i,if,1)
            call itoch(ibuff(1),host)
            call itoch(ibuff(31),user)
            call itoch(ibuff(61),mach)
            ipid=ibuff(91)
         write(iwr,1002)i,ipid,host(1:12),user(1:12),mach,fac
   10    continue
      else
         call chtoi(ibuff(1),host)
         call chtoi(ibuff(31),user)
         call chtoi(ibuff(61),mach)
         ibuff(91)=ipid
         call pg_snd(100,ibuff,91*liw,0,1)
         call pg_snd(100,fac,8,0,1)

      endif
_IF(pvm)
 1000 format(//,40x,'PVM parallel implementation - ',
     &  'node information:',/,40x,47('-'),//
_ELSEIF(mpi)
 1000 format(/,/,40x,'MPI parallel implementation - ',
     &  'node information:',/,40x,47('-'),/,/
_ELSEIF(ga)
 1000 format(/,/,40x,'TCGMSG/GA-Tools parallel implementation - ',
     & 'node information:',/,40x,58('-'),/,/
_ELSEIF(tcgmsg)
 1000 format(//,40x,'TCGMSG parallel implementation - ',
     & 'node information:',/,40x,50('-'),//
_ELSE
 1000 format(//,40x,'node information',/,40x,16('-'),//
_ENDIF
     & 40x,'node     pid   hostname     user           version',/)
 1001 format(40x,'root',i8,3x,a12,1x,a12,1x,a30,f6.2)
 1002 format(40x,   i4 ,i8,3x,a12,1x,a12,1x,a30,f6.2)
_ENDIF

      if(opg_root())then
         write(iwr,*)
         write(iwr,*)
         write(iwr,*)
         write(iwr,*)
         if(iparapr.eq.0)write(iwr,1018)
 1018    format(40x,'supress print of timing information')
         if(iparapr.eq.1)write(iwr,1019)
 1019    format(40x,'print timings for root node')
         if(iparapr.eq.2)write(iwr,1021)
 1021    format(40x,'print timings for all nodes')

_IF(diag_parallel)
         if(idpdiag.eq.99999999)then
            write(iwr,1022)
 1022       format(40x,'use serial jacobi diagonaliser')
         else
            write(iwr,1023)idpdiag
 1023       format(40x,'use parallel diagonaliser for matrices size',
     &           i5,' and above')
         endif
_ENDIF

_IF(ga)
         if(idpdiis.eq.99999999)then
            write(iwr,1026)
 1026       format(40x,'use serial diis solver')
         else
            write(iwr,1027)idpdiis
 1027       format(40x,'use parallel diis solver for matrices',
     &           i5,' and above')
         endif

         if(idpmult2.eq.99999999)then
            write(iwr,1028)
 1028       format(40x,'use serial mult2 ')
         else
            write(iwr,1029)idpmult2
 1029       format(40x,'use parallel mult2 for matrices',
     &           i5,' and above')
         endif

         if(idporth.eq.99999999)then
            write(iwr,1034)
 1034       format(40x,'use serial orthog. ')
         else
            write(iwr,1035)idporth
 1035       format(40x,'use parallel orthog. for matrices',
     &           i5,' and above')
         endif
_ENDIF
         write(iwr,1024)ntchnk
 1024    format(40x,'chunk size based on ',i6,' tasks/SCF cycle')

         if(limchnk.ne.-1)write(iwr,1025)limchnk
 1025    format(40x,'chunk size limit is ',i5)

         if(ipiomode.eq.IO_NZ)then
            write(iwr,1031)
         else if(ipiomode.eq.IO_NZ_S)then
            write(iwr,1032)
         else if(ipiomode.eq.IO_A)then
            write(iwr,1033)
         endif
 1031    format(40x,'i/o will be routed through node 0')
 1032   format(40x,'i/o will be routed through node 0, minimise comms',
     &        ' by master/slave model',/,40x
_IF(ga)
     &        ,'NB - this will reduce use of parallel linear algebra')
_ELSE
     &        )
_ENDIF
 1033    format(40x,'each node will maintain a copy of ed3,ed7 ')

      endif
      end
_ELSE
c serial stub
      subroutine pg_pproc
      end
_ENDIF 
c
c========================================================================
c
c  ** pg_end
c
      subroutine pg_end
      implicit none
_IF(mpi)
      integer ierr
_IF(dynamic)
      include 'mpif.h'
INCLUDE(common/mpidata)
      integer stat(MPI_STATUS_SIZE) 
      integer i,n
      integer linfo, nw, icode
      parameter (linfo=2*max_processors)
      integer info(2,0:max_processors - 1)
      call MPI_COMM_RANK(MPI_COMM_WORLD,i,ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,n,ierr)
      nw = n - 1

      write(6,*)'pg_end',i,n

      if(i.eq.0)then
         if(n.ne.nw)then
            icode = -3
            call MPI_SEND (icode,1,MPI_INTEGER, n-1, 
     &           971, MPI_COMM_WORLD,ierr) 
            call mpi_recv (info ,linfo, MPI_INTEGER, n-1,
     &           972,MPI_COMM_WORLD,stat,ierr)
            write(6,*)'task allocation stats (node,#allocation,#tasks)'
            do i = 0, nw-1
               write(6,*)i,info(1,i),info(2,i)
            enddo
            icode = -2
            call MPI_SEND (icode,1,MPI_INTEGER, n-1, 
     &           971, MPI_COMM_WORLD,ierr) 
         endif
      endif

      write(6,*)'pg_end calling mpi_finalize',i,ierr

      if(ierr.ne.0)call pg_errmsg('finalize',ierr)
_ENDIF
 
C      call MPI_FINALIZE(icode)

      write(6,*)'pg_end called  mpi_finalize'

      call exit(0)
_ELSEIF(tcgmsg)
_IF(ga)
INCLUDE(common/parcntl)
      if(ga_initted)then
        call ga_terminate
      endif
_ENDIF
      call debugp('pg_end')
      call pend

_IFN(chemshell)
      call exit(0)
_ENDIF

_ELSEIF(pvm)
      include '/tmp/pvm3/fpvm3.h'  
      integer nodes,tids,me 
      common/mypvm/nodes,tids(0:63),me
      integer ierr
      logical test_verb
      if(test_verb(1))then
         write(6,*) '............. process ',me,' ends .........'
      endif
      call pvmfexit (ierr)
      call exit(0)
_ELSE
c     elseif nx??
c no op
_ENDIF
      end
c
c========================================================================
c
c  ** opg_root() : return .true. for root process
c
      logical function opg_root()
      opg_root = ipg_nodeid() .eq. 0
      return
      end

      logical function oroot()
      oroot = ipg_nodeid() .eq. 0
      return
      end
c
c========================================================================
c
c ** ipg_nodeid() : return index (0 - (nnodes-1)) for the 
c                   current process
c
      integer function ipg_nodeid()
      implicit none
_IF(mpi)
      include 'mpif.h'
INCLUDE(common/mpidata)
      integer ierr
      logical test_verb

      call MPI_COMM_RANK(MPI_COMM_WORKERS,ipg_nodeid,ierr)
      if(ierr .ne. 0)call pg_errmsg('comm_rank',ierr)

_ELSEIF(tcgmsg)
_IF(ga)
INCLUDE(common/parcntl)    
      MA_INTEGER  ga_nodeid, nodeid
      if(ga_initted)then
         ipg_nodeid = ga_nodeid()
      else
         ipg_nodeid = nodeid()
      endif
_ELSE
      MA_INTEGER nodeid
      ipg_nodeid = nodeid()
_ENDIF
_ELSEIF(nx)
      integer mynode
      ipg_nodeid = mynode()
_ELSEIF(pvm)
      include '/tmp/pvm3/fpvm3.h'  
      integer comm,size,ierr,nodes,tids,me  
      common/mypvm/nodes,tids(0:63),me 
      ipg_nodeid = me      
_ELSE
      ipg_nodeid = 0
_ENDIF
      return
      end
c
c========================================================================
c
c ** ipg_nnodes() : return number of nodes
c

      integer function ipg_nnodes()
      implicit none
_IF(mpi)
      include 'mpif.h'
INCLUDE(common/mpidata)
      integer ierr
      call MPI_COMM_SIZE(MPI_COMM_WORKERS,ipg_nnodes,ierr)
      if(ierr .ne. 0)call pg_errmsg('comm_size',ierr)
_ELSEIF(tcgmsg)
_IF(ga)
INCLUDE(common/parcntl)    
      MA_INTEGER ga_nnodes, nnodes
      if(ga_initted)then
        ipg_nnodes = ga_nnodes()
      else
        ipg_nnodes = nnodes()
      endif
_ELSE
      MA_INTEGER nnodes
      ipg_nnodes = nnodes()
_ENDIF
_ELSEIF(nx)
      integer numnodes
      ipg_nnodes = numnodes()
_ELSEIF(pvm)
      include '/tmp/pvm3/fpvm3.h'  
      integer nodes,tids,me 
      common/mypvm/nodes,tids(0:63),me
      ipg_nnodes = nodes -1
_ELSE
      ipg_nnodes = 1
_ENDIF
      return
      end
c
c========================================================================
c
c  static load balancing functions
c
      function oipsci()
      implicit REAL  (a-h,o-z)
      logical oipsci
_IF(parallel)
INCLUDE(common/parallel)
c
c...   decide  if we should process this integral batch
c...   called from jkin70 (integs) etc.
c
       icount_slb = icount_slb + 1
       oipsci = mod(icount_slb,ipg_nnodes()).ne.ipg_nodeid()
_ELSE
       oipsci = .true.
_ENDIF
       end
c
c========================================================================
c
       function  iipsci()
c
c...   initialise
c
_IF(parallel)
INCLUDE(common/parallel)
       icount_slb = 0
_ENDIF
       iipsci = 0
       return
       end
c
c========================================================================
c
c  ** subroutine pg_dgop : double precision global sum
c
c     Double Global OPeration.
c     x(1:n) is a vector present on each process. ggop 'sums'
c     x accross all nodes using the commutative operator op.
c     The result is broadcast to all nodes. Supported operations
c     include '+', '*', 'max', 'min', 'absmax', 'absmin'.
c
c
      subroutine pg_dgop(TYPE, X, N, OP)
      integer TYPE         
      REAL X(N)
      character*(*) OP     
INCLUDE(common/timeperiods)
_IF(mpi)
      include 'mpif.h'
INCLUDE(common/mpidata)
      logical test_verb
      integer igmem_alloc
c
INCLUDE(common/vcore)
c

      if(test_verb(2))write(6,*)'dgop type=',type,' length=',n

      call start_time_period(TP_DGOP)
      dumtim = dclock()

      if(op .eq. '+')then
         iop = MPI_SUM
      else
         write(6,*)op
         call pg_errmsg('unsuported op',-1)
      endif
c
      if(test_verb(2))write(6,*)'dgop type=',type,' length=',n
c
      iwork = igmem_alloc(n)

      call MPI_ALLREDUCE (x,Q(iwork),n,
     &	   MPI_DOUBLE_PRECISION,
     &     iop, MPI_COMM_WORKERS, ierr)

      call dcopy( n, Q(iwork), 1, x, 1 )
      call gmem_free(iwork)

      if(ierr .ne. 0)call pg_errmsg('all_reduce',ierr)
*     tgoptm = tgoptm + (dclock()-dumtim)
      call end_time_period(TP_DGOP)
_ELSEIF(tcgmsg)
      MA_INTEGER type8, n8
INCLUDE(common/parcntl)
      if(n.eq.0)return
      call start_time_period(TP_DGOP)
      n8=n
      type8=type
_IF(ga)
      if(ga_initted)then
        call ga_dgop(TYPE8, X, N8, OP)
      else
_ENDIF
c - synchs for benefit of sp2
      call pg_synch(101)
      call dgop(TYPE8, X, N8, OP)
      call pg_synch(102)
_IF(ga)
      endif
_ENDIF
      call end_time_period(TP_DGOP)

_ELSEIF(nx)

      integer igmem_alloc
      iwork = igmem_alloc(n)
      call start_time_period(TP_DGOP)
      call gdsum(x,n,Q(iwork))
      call end_time_period(TP_DGOP)
      call gmem_free(iwork)

c   elseif pvm??

_ELSE
c
c simple binary tree implementation

      integer n1, n2, n3, me, nproc, iwork
      integer igmem_alloc
INCLUDE(common/vcore)
c
      me = ipg_nodeid()
      nproc = ipg_nnodes()
      iwork = igmem_alloc(n)
      n1 = 2*me+1
      n2 = 2*me+2
      n3 = (me-1)/2

      if (n2.lt.nproc) then
         call pg_rcv(type,Q(iwork),n*8,nret,-1,ndfm,1)
         if(nret.ne.n*8)call pg_err(23)
         call ddoop(n, op, x, Q(iwork))
      endif
      if (n1.lt.nproc) then
         call pg_rcv(type,Q(iwork),n*8,nret,-1,ndfm,1)
         if(nret.ne.n*8)call pg_err(23)
         call ddoop(n, op, x, Q(iwork))
      endif
      if (me.ne.0) call pg_snd(type, x, n*8, n3, 1)
      call pg_brdcst(type, x, 8*n, 0)

      call gmem_free(iwork)

c
100   format(1x,12e9.2)
_ENDIF
      return
      end
c
      subroutine ddoop(n, op, x, work)
      implicit none
      integer n
      REAL x(n), work(n)
      character *(*) op
      integer i
c
c     REAL  Do Op ... do the operation for pg_dgop
c
      if (op .eq. '+') then
	do 10 i = 1,n
	  x(i) = x(i) + work(i)
10      continue
      else if (op .eq. '*') then
	do 20 i = 1,n
	  x(i) = x(i) * work(i)
20      continue
      else if (op .eq. 'max') then
	do 30 i = 1,n
	  x(i) = max(x(i), work(i))
30      continue
      else if (op .eq. 'min') then
	do 40 i = 1,n
	  x(i) = min(x(i), work(i))
40      continue
      else if (op .eq. 'absmax') then
	do 50 i = 1,n
	  x(i) = max(abs(x(i)), abs(work(i)))
50      continue
      else if (op .eq. 'absmin') then
	do 60 i = 1,n
	  x(i) = min(abs(x(i)), abs(work(i)))
60      continue
      else
         call pg_errmsg('bad dgop',-1)
      endif
      end
c========================================================================
c
c  ** subroutine pg_igop : integer global sum
c
c     Integer Global OPeration.
c     x(1:n) is a vector present on each process. ggop 'sums'
c     x accross all nodes using the commutative operator op.
c     The result is broadcast to all nodes. Supported operations
c     include '+', '*', 'max', 'min', 'absmax', 'absmin'.
c
c
      subroutine pg_igop(TYPE, X, N, OP)
      integer TYPE
      integer X(N)
      character*(*) OP     
INCLUDE(common/timeperiods)
_IF(mpi)
      include 'mpif.h'
INCLUDE(common/mpidata)
INCLUDE(common/vcore)
      integer iwork, igmem_alloc
      Integer i
      logical test_verb
      integer lenwrd
      external lenwrd
c
      call start_time_period(TP_DGOP)

      if(op .eq. '+')then
         iop = MPI_SUM
      else
         write(6,*)op
         call pg_errmsg('unsuported op',-1)
      endif
c
      if(test_verb(2))write(6,*)'igop type=',type,' length=',n

      iwork = igmem_alloc((n-1)/lenwrd() + 1)
      call MPI_ALLREDUCE (x,Q(iwork),n,MPI_INTEGER,
     &     iop, MPI_COMM_WORKERS, ierr)

      if(ierr .ne. 0)call pg_errmsg('all_reduce',ierr)
c
c copy result back - MPI leaves it in work
c
      call icopy(n,Q(iwork),1,x,1)

      call gmem_free(iwork)

      call end_time_period(TP_DGOP)
_ELSEIF(tcgmsg)

      MA_INTEGER type8, n8
      MA_INTEGER ibuff(4096)
      integer first, last, ii
INCLUDE(common/parcntl)

      if(n.eq.0)return
      call start_time_period(TP_DGOP)
      type8=type

      do first=1,n,4096
         last = min(n,first + 4095)
         n8 = (last - first + 1)
         do ii = 1, n8
            ibuff(ii) = x(first+ii-1)
         enddo
_IF(ga)
         if(ga_initted)then
            call ga_igop(type8, ibuff, N8, OP)
         else
_ENDIF
           call pg_synch(101)
           call igop(TYPE8, ibuff, N8, OP)
           call pg_synch(102)
_IF(ga)
         endif
_ENDIF
         do ii = 1, n8
            x(first+ii-1) = ibuff(ii)
         enddo
      enddo
      call end_time_period(TP_DGOP)

_ELSEIF(nx)
INCLUDE(common/vcore)
      integer iwork, igmem_alloc
      integer lenwrd
      external lenwrd, igmem_alloc

      iwork = igmem_alloc((n-1)/lenwrd() + 1)
      call start_time_period(TP_DGOP)
      call gdsum(x,n,Q(iwork))
      call end_time_period(TP_DGOP)
      call gmem_free(iwork)

c   elseif pvm??

_ELSE
c
c simple binary tree implementation
c      subroutine pg_igop_pvm(itype, x, n, op)
c
      integer n1, n2, n3, me, nproc
INCLUDE(common/vcore)
      integer iwork, igmem_alloc
      integer lenwrd
      external lenwrd
c
      me = ipg_nodeid()
      nproc = ipg_nnodes()
      n1 = 2*me+1
      n2 = 2*me+2
      n3 = (me-1)/2

      iwork = igmem_alloc((n-1)/lenwrd() + 1)

      if (n2.lt.nproc) then
         call pg_rcv(type,Q(iwork),
     &        n*4,nret,-1,ndfm,1)
         if(nret.ne.n*4)call pg_err(23)
         call ddoop(n, op, x, Q(iwork))
      endif
      if (n1.lt.nproc) then
         call pg_rcv(type,Q(iwork),
     &        n*4,nret,-1,ndfm,1)
         if(nret.ne.n*4)call pg_err(23)
         call idoop(n, op, x, Q(iwork))
      endif
      if (me.ne.0) call pg_snd(type, x, n*8, n3, 1)
      call pg_brdcst(type, x, 8*n, 0)

      call gmem_free(iwork)
c
100   format(1x,12e9.2)
_ENDIF
      return
      end
c
      subroutine idoop(n, op, x, work)
      implicit none
      integer n
      integer x(n), work(n)
      character *(*) op
      integer i
c
c     integer Do Op ... do the operation for pg_igop
c
      if (op .eq. '+') then
	do 10 i = 1,n
	  x(i) = x(i) + work(i)
10      continue
      else if (op .eq. '*') then
	do 20 i = 1,n
	  x(i) = x(i) * work(i)
20      continue
      else if (op .eq. 'max') then
	do 30 i = 1,n
	  x(i) = max(x(i), work(i))
30      continue
      else if (op .eq. 'min') then
	do 40 i = 1,n
	  x(i) = min(x(i), work(i))
40      continue
      else if (op .eq. 'absmax') then
	do 50 i = 1,n
	  x(i) = max(iabs(x(i)), iabs(work(i)))
50      continue
      else if (op .eq. 'absmin') then
	do 60 i = 1,n
	  x(i) = min(iabs(x(i)), iabs(work(i)))
60      continue
      else
         call pg_errmsg('bad igop',-1)
      endif
      end
c========================================================================
c
c ** pg_brdcst : byte-wise broadcast
c
      subroutine pg_brdcst(TYPE, BUF, LENBUF, IFROM)
      implicit none
      INTEGER TYPE    
      INTEGER LENBUF  
      INTEGER IFROM   
_IF(mpi)
INCLUDE(common/timeperiods)
      include 'mpif.h'
INCLUDE(common/mpidata)
      integer ierr
      integer buf(*)
      call start_time_period(TP_BCAST)
      call MPI_BCAST (buf,lenbuf,MPI_BYTE,ifrom,MPI_COMM_WORKERS,ierr) 
      if(ierr.ne.0)call pg_errmsg('brdcst',ierr)
      call end_time_period(TP_BCAST)
_ELSEIF(tcgmsg)
      integer buf(*)
c      BYTE BUF(LENBUF)
INCLUDE(common/timeperiods)
INCLUDE(common/parcntl)
      MA_INTEGER type8, lenbuf8, ifrom8
      call start_time_period(TP_BCAST)
      type8=type
      lenbuf8=lenbuf
      ifrom8=ifrom
_IF(ga)
      if(ga_initted)then
        call ga_brdcst(TYPE8, BUF, LENBUF8, IFROM8)
      else
_ENDIF
        call brdcst(TYPE8, BUF, LENBUF8, IFROM8)
_IF(ga)
      endif
_ENDIF
      call end_time_period(TP_BCAST)

_ELSEIF(nx)
      byte buf(*)
INCLUDE(common/sizes)
INCLUDE(common/nodinf)
INCLUDE(common/timeperiods)
      integer mynode
      call start_time_period(TP_BCAST)
      if(mynode().eq.ifrom)then
         call csend(type,buf,lenbuf,-1,mpid)
      else
         call crecv(type,buf,lenbuf)
      endif
      call end_time_period(TP_BCAST)
_ELSEIF(pvm)
      include '/tmp/pvm3/fpvm3.h'
      integer count,datatype,comm,stride,msgtag,ierr,root
      integer nodes,tids,me,others(63),n1,i,j,nmax
      common/mypvm/nodes,tids(0:63),me
      integer sendbuf(*)
      logical test_verb
      data stride/1/,msgtag/0/ 
      if(test_verb(2))
     &     write(6,*)'mpi_bcast root ',root,' length ',count,
     &     'type ',datatype
      ierr = 0

      root = ifrom
      nmax=nodes-1
      n1=nmax - 1

      if(root .lt. 0 .or. root .ge. nmax )then
         call pg_errmsg('bad root node',-1)
      endif

      if(me .eq. root)then
c  set up tids array of the OTHER nodes 
         j=0 
         do i=0,n1
            if (i.ne.me) then 
               j=j+1
               others(j)=tids(i)
            endif
         enddo

         call pvmfinitsend (PVMDEFAULT,ierr) 
         call pvmfpack (BYTE1,sendbuf,count,stride,ierr)
         if(ierr.ne.0)call pg_errmsg('mpi_bcast:pvmfpack',ierr)
         call pvmfmcast (n1,others,msgtag,ierr) 
      else
         call pvmfrecv(tids(root),msgtag,ierr)
         if(ierr.lt.0)call pg_errmsg('mpi_bcast:pvmfrecv',ierr)
         call pvmfunpack (BYTE1,sendbuf,count,stride,ierr)
         if(ierr.ne.0)call pg_errmsg('mpi_bcast:pvmfunpack',ierr)
      endif
_ELSE
c no op
      integer buf(*)
_ENDIF
      return 
      end
c
c========================================================================
c
c     subroutine pg_synch : synchronisation
c
      subroutine pg_synch(code)
      implicit none
      integer code
_IF(mpi)
      include 'mpif.h'
INCLUDE(common/mpidata)
      integer ierr, isync
c  NB - isync is not used here
      logical test_verb
      if(test_verb(2))write(6,*)'barrier'

      call MPI_BARRIER(MPI_COMM_WORKERS,ierr)
      if(ierr.ne.0)call pg_errmsg('barrier',ierr)
_ELSEIF(tcgmsg)
      MA_INTEGER code8
INCLUDE(common/parcntl)
      call debugp('synch')
      code8=code
_IF(ga)
      if(ga_initted)then
        call ga_sync(code8)
      else
        write(6,*)'non GA sync'
_ENDIF
        call synch(code8)
_IF(ga)
      endif
_ENDIF

_ELSEIF(nx)
      call gsync
_ELSEIF(pvm)
      call caserr('pg_synch')
_ENDIF
      return
      end
c
c ======================================================================
c
c  ** taskinfodata : load balancing initialisation
c
      block data taskinfodata
INCLUDE(common/taskinfo)
      data ichi /1/
      data ichl /1/
      data istat /0/
      data nleft /0/
      data itaskl /0/
      end
c
c =====================================================================
c
c  ** pg_dlbchunk : set chunk size
c
      subroutine pg_dlbchunk(ichunk,prnt)
c
c  set up initial chunck size 
c  must be called with the same value from all nodes.
c  this determines the first index of the loop counter
c
      implicit none
INCLUDE(common/taskinfo)
INCLUDE(common/iofile)
      integer ichunk
      logical prnt
_IF(parallel)
      logical opg_root
      if(opg_root() .and. prnt)write(iwr,*)'p: set chunk size',ichunk
_ENDIF
      ichi = ichunk
      ichl = ichunk
      end
c
c ====================================================================
c
c  ** pg_dlbreset : reset global and local counters
c
      subroutine pg_dlbreset
c
c  reset the task allocation code
c
      implicit none
INCLUDE(common/taskinfo)
INCLUDE(common/parallel)
_IF(mpi)
c
c  reset the task allocation code
c
_IF(dynamic)
      integer n,i,ierr,icode,idum
      integer ipg_nodeid
      logical opg_root
      include 'mpif.h'
      integer stat(MPI_STATUS_SIZE)
      integer ipg_nnodes
c
_ENDIF

c      if(istat .ne. 0)then
c         call caserr('bad counter state')
c      endif

      call debugp('pg_dlbreset #')
_IF(dynamic)
      n = ipg_nnodes()
      i = ipg_nodeid()
      call pg_synch(100)
      if(opg_root())then
         icode = -1
         call MPI_SEND (icode,1,MPI_INTEGER, n, 
     &        971, MPI_COMM_WORLD,ierr) 
         call mpi_recv  (idum ,1, MPI_INTEGER, n,
     &        972,MPI_COMM_WORLD,stat,ierr)
      endif
      call pg_synch(101)
_ENDIF
      icount_dlb = 0
      nleft=0
      itaskl=0      

_ELSEIF(tcgmsg)
      integer idum, ipg_nnodes
      MA_INTEGER n8, nxtval
      external nxtval
      call debugp('pg_dlbreset #')
      n8 = -ipg_nnodes()
      idum = nxtval(n8)
      nleft=0
      itaskl=0
      icount_dlb = 0
_ELSEIF(nx)
      common/nodin2/mcount,itask,ichunk
c to placate implicit none....
      integer gtask, ircvts
      external gtask
      logical ohand
      common/handle/ohand
      data ohand/.false./
      logical opg_root
      call gsync
c check if init????
      if(.not.ohand)then
         if(opg_root())call hrecv(3215,ircvts,4,gtask)
         ohand = .true.
      endif
_ELSEIF(pvm)
c   elseif pvm??
    caserr('pg_dlbreset pvm')
_ELSE
      itaskl=0
      icount_dlb = 0
_ENDIF
      return
      end
c
c ====================================================================
c
c  ** pg_dlbfin : end dlb section
c
      subroutine pg_dlbfin
      implicit none
INCLUDE(common/taskinfo)
      call pg_synch(999)
      call debugp('pg_dlbfin')
      istat = 0
      end
c
c ====================================================================
c
c  ** ipg_dlbtask() :  get a global index
c
      integer function ipg_dlbtask()
      implicit none
INCLUDE(common/timeperiods)
INCLUDE(common/taskinfo)
_IF(mpi)
      integer n,i,ierr,icode
      integer ipg_nnodes, ipg_nodeid
      logical opg_root
      include 'mpif.h'

      integer stat(MPI_STATUS_SIZE)

      call debugp('pg_dlbtask')

      n = ipg_nnodes()
      i = ipg_nodeid()
      istat = 1

_IF(dynamic)
c
c  MPI:  master/slave model
c
      istat = 1
c
c  send local chunck parameter to reserve ichi indices
c
      if(nleft .gt. 0 )then
         nleft = nleft - 1
         itaskl = itaskl + 1
c         write(6,*)'##node',i,': locally allocate index',itaskl,
c     &        ' nleft ',nleft
      else
         icode = ichi
         call start_time_period(TP_NXTVAL)
         call MPI_SEND (icode,1,MPI_INTEGER, n, 
     &        971, MPI_COMM_WORLD,ierr) 
         call mpi_recv  (itaskl ,1, MPI_INTEGER, n,
     &        972,MPI_COMM_WORLD,stat,ierr)
         call end_time_period(TP_NXTVAL)
         if(ierr .ne. 0)call pg_errmsg('nxtask:recv',ierr)
         nleft = ichi - 1
c         write(6,*)'##node',i,': fetch index',itaskl
      endif
      ipg_dlbtask = itaskl
_ELSE
c
c MPI with static (round-robin) scheme
c
      if(itaskl.eq.0)then
         itaskl = i+1
      else
         itaskl = itaskl + n
      endif
      ipg_dlbtask = itaskl
_ENDIF
_ELSEIF(tcgmsg)
      integer n,i
      integer ipg_nnodes, ipg_nodeid
      logical opg_root
      integer itsk_tcg
ctfp
      MA_INTEGER nxtval, n8

      call debugp('pg_dlbtask')

      n = ipg_nnodes()
      i = ipg_nodeid()
      istat = 1
      if(nleft .gt. 0 )then
         nleft = nleft - 1
         itaskl = itaskl + 1
      else
c
c we count from 1, tcgmsg from 0 

         call start_time_period(TP_NXTVAL)
         n8 = n
         itsk_tcg = nxtval(n8)
         call end_time_period(TP_NXTVAL)
         itaskl = itsk_tcg*ichi + 1
         nleft = ichi - 1
         if(opg_root())then
c            write(6,*)'##node',i,': fetch index',itaskl
         endif
      endif
      ipg_dlbtask = itaskl
_ELSEIF(pvm)
      caserr('ipg_dlbtask pvm')
_ELSEIF(nx)
INCLUDE(common/sizes)
INCLUDE(common/nodinf)
c     integer n,i
c     logical opg_root
      integer gtask, iflop, ichka1, itsk_ipsc, irecv
      external gtask
c
      if(nleft .gt. 0 )then
         nleft = nleft - 1
         itaskl = itaskl + 1
c         if(opg_root())then
c            write(6,*)'##node',i,': locally allocate index',itaskl,
c     &           ' nleft ',nleft
c         endif
      else
         call start_time_period(TP_NXTVAL)
         ichka1 = irecv(3216,itsk_ipsc,4)
         call csend(3215,iflop,4,0,mpid)
         call msgwait(ichka1)
         call end_time_period(TP_NXTVAL)
         itaskl = itsk_ipsc*ichi + 1
         nleft = ichi - 1
c         if(opg_root())then
c            write(6,*)'##node',i,': fetch index',itaskl
c         endif
      endif
      ipg_dlbtask = itaskl


c	elseif pvm??
_ELSEIF(pvm)
	call caserr('pvm')

_ELSE
c
c serial version
c
      if(itaskl.eq.0)then
         itaskl = 1
      else
         itaskl = itaskl + 1
      endif
      ipg_dlbtask = itaskl
_ENDIF
      end
c
c ====================================================================
c
c   ** ipg_dblpush :  get a global index
c
c            push the last index back for re-use, for use when a task 
c            is not used (as at the end of a load-balanced loop)
c
      subroutine pg_dlbpush
      implicit none
INCLUDE(common/taskinfo)
_IF(mpi)
_IFN(dynamic)
      integer ipg_nnodes
_ENDIF
_ENDIF
      call debugp('pg_dlbpush *')
_IF(mpi)
_IF(dynamic)
c
c decrement local batch counter
c
      nleft = nleft + 1
      itaskl = itaskl - 1
_ELSE
c
c no chunking - back-set round-robin scheme
c
      itaskl = itaskl - ipg_nnodes()
_ENDIF
_ELSE
c
c decrement local batch counter
c
      nleft = nleft + 1
      itaskl = itaskl - 1
_ENDIF
      end
c
_IF(ipsc)
_IF(nx)
c ====================================================================
c
c ** fgtask : fortran part of intel interrupt processor
c 
      subroutine fgtask(ireqn)
      implicit integer(a-z)
c
      external gtask
      common/nodin2/mcount,itask,ichunk
      common/nodinf/mpid,minode,mihost,ldim,nnodes
c
c      print *,'node ',ireqn,' given task ',itask
      call csend(3216,itask,4,ireqn,mpid)
      call hrecv(3215,rcvtk,4,gtask)
c
      itask=itask+1
      return
      end
_ENDIF
_ENDIF
c
c  
c
c
      subroutine pg_dlbtest
      implicit none
INCLUDE(common/parallel)
INCLUDE(common/iofile)
      REAL tot, tester
      integer ipg_nodeid
      integer ipg_dlbtask
      integer i, niter, next

      tot = 0.0d0
      niter=10

      call pg_dlbreset
      next = ipg_dlbtask()

      do i = 1,niter
         icount_dlb = icount_dlb+1
         if(icount_dlb .eq. next)then
            write(6,*)'task',next,'on node',ipg_nodeid()
            tot = tot + dble(icount_dlb)
c
c check push of indices
c
            next = ipg_dlbtask() 
            call pg_dlbpush

            next = ipg_dlbtask()
         endif
      enddo

      call pg_dgop(1010,tot,1,'+')
      tester= dble(niter*(niter+1)/2)
      if(dabs(tot - tester).lt.1.0d-10)then
         write(iwr,*)'test passed',tot
      else
         write(iwr,*)'test failed',tot,tester
      endif
      call pg_dlbpush

      write(iwr,*)'all done',ipg_nodeid()

      end
c
c ======================================================================
c
c ** pg_err : parallel error handling
c
c   code : numerical error code
c
      subroutine pg_err(code)
      implicit none
      integer code
_IF(mpi)
      include 'mpif.h'
      integer ierr
      write(6,*) ' PG_ERR INVOKED, code = ', code
      call mpi_abort(MPI_COMM_WORLD,code,ierr)
      write(6,*) ' RETURN from mpi_abort',ierr

_ELSEIF(tcgmsg)

_IF(rs6000)
c
c to avoid ambiguous messages a special routine has been 
c available
c
      call pexitc(code)
_ELSE
c
c error handling by TCGMSG function
c
      MA_INTEGER code8
      code8 = code
      call parerr(code8)
_ENDIF
c
_ELSEIF(pvm)
      integer i, tids, me, nodes, info
      common/mypvm/nodes,tids(0:63),me
      write(6,*)'parerr called on node ',ipg_nodeid(), 'code ',code
      do i=0,nodes-1
         if (i.ne.me) then 
            call pvmfkill(tids(i),info)
            write(6,*)'kill ',i,' code',info
         endif
      enddo
_ELSEIF(nx)
c     elseif nx??
_ELSE
c
c serial implementations
_IF(rs6000,hp700,hp800)
c
c use C exit routine
c
      call exitc(code)
_ELSE
      call exit(code)
_ENDIF
_ENDIF
      end

      subroutine pg_errmsg(s,i)
      implicit none
      integer ipg_nodeid, i, me
      character s*(*)
      me=ipg_nodeid()
      write(6,*)'******* fatal error on node',me,' code =',i
      write(6,*)'       ',s
      call pg_err(i)
      return
      end
c
c ======================================================================
c
c ** pg_snd : snd bytewise message
c
      subroutine pg_snd(TYPE, BUF, LENBUF, NODE, SYNC)
      implicit none
      INTEGER TYPE      
      INTEGER LENBUF    
      INTEGER NODE      
      INTEGER SYNC      
_IF(mpi)
      integer buf(*)

      integer ierr
      include 'mpif.h'
      logical test_verb

      if(test_verb(2))then
         write(6,*)'pg_snd to',node
      endif

      if(sync.ne.1)call pg_errmsg('async',0)

      call MPI_SEND (buf,lenbuf,MPI_BYTE, node, type,
     &     MPI_COMM_WORLD,ierr)            

      if (ierr.ne.0) call pg_errmsg('send',ierr)
_ELSEIF(tcgmsg)
      integer  BUF(*)  
      MA_INTEGER type8, node8, sync8, lenbuf8
INCLUDE(common/parcntl)    
      integer ga_id_to_msg_id, to
_IF(ga)
      if(ga_initted)then
        to =  ga_id_to_msg_id(node)
      else
        to=node
      endif
_ELSE
      to = node
_ENDIF
      type8=type
      node8=node
      sync8=sync
      lenbuf8=lenbuf
      call snd(TYPE8, BUF, LENBUF8, NODE8, SYNC8)
_ELSEIF(nx)
INCLUDE(common/sizes)
INCLUDE(common/nodinf)
      BYTE BUF(LENBUF)  
      call csend(type,buf,lenbuf,node,mpid)
_ELSEIF(pvm)
      byte buf(lenbuf) 
      include '/tmp/pvm3/fpvm3.h'
      integer tag,comm,stride,ierr 
      integer nodes,tids,me
      common/mypvm/nodes,tids(0:63),me
      data stride/1/
      call pvmfinitsend (PVMDEFAULT,ierr) 
      call pvmfpack (BYTE1,buf,lenbuf,stride,ierr) 
      call pvmfsend (tids(node),type,ierr) 
_ELSE
      integer buf(*)
      call caserr('pg_snd')
_ENDIF
      return
      end
c
c ======================================================================
c
c ** pg_rcv : receive bytewise message
c
      SUBROUTINE pg_rcv(TYPE, BUF, LENBUF, LENMES, NODESEL, 
     &     NODEFROM, SYNC)
      implicit none
      INTEGER TYPE      
      INTEGER LENBUF     
c      BYTE BUF(LENBUF)   
      integer BUF(*)   
      INTEGER LENMES     
      INTEGER NODESEL    
      INTEGER NODEFROM   
      INTEGER SYNC       
      
ctfp
_IF(mpi,tcgmsg,ga,pvm,nx)
      MA_INTEGER ga_nnodes
      integer ga_id_to_msg_id, iii
      MA_INTEGER isel, ifrom
      MA_INTEGER type8, sync8, lenbuf8, lenmes8
_ENDIF
_IF(mpi)
      include 'mpif.h'
      integer stat(MPI_STATUS_SIZE)
      integer ierr
      logical test_verb

      if(test_verb(2))then
         write(6,*)'pg_rcv from',ifrom
      endif

      if(sync.ne.1)call pg_errmsg('async',0)

      ifrom = nodesel
      if (nodesel.eq.-1) ifrom = MPI_ANY_SOURCE

      call mpi_recv  (buf ,lenbuf, MPI_BYTE, ifrom,
     &     type,MPI_COMM_WORLD,stat,ierr)

      if (ierr.ne.0) call pg_errmsg('recv',ierr)

***      call MPI_GET_SOURCE(stat,nodefrom)
      nodefrom = stat( MPI_source )

c @@ don't get the true length yet
      lenmes = 0
_ELSEIF(tcgmsg)
INCLUDE(common/parcntl)    
      isel = NODESEL
      if(nodesel.ne.-1)then
_IF(ga)
      if(ga_initted)then
         isel =  ga_id_to_msg_id(nodesel)
      else
         isel = nodesel
      endif
_ELSE
         isel =  nodesel
_ENDIF
      endif
      type8 = type
      lenbuf8=lenbuf
      sync8=sync
      call rcv(TYPE8, BUF, LENBUF8, LENMES8, isel, ifrom, SYNC8)
      lenmes=lenmes8
_IF(ga)
      if(ga_initted)then

      NODEFROM = -999
      do iii = 0,ga_nnodes()-1
         if(ga_id_to_msg_id(iii) .eq. ifrom)then
            NODEFROM = iii
         endif
      enddo
      if(NODEFROM .eq. -999)call caserr('node mapping error')
      else
        NODEFROM = ifrom
      endif
_ELSE
      NODEFROM = ifrom
_ENDIF
_ELSEIF(nx)
c
c nb nodesel isnt implemented - perhaps should
c concoct suitable type
c
      call crecv(type,buf,lenbuf)
      lenmes = lenbuf
_ELSEIF(pvm)
        implicit none
        include '/tmp/pvm3/fpvm3.h'
        integer count,datatype,source,tag,comm,tid, intag, bytes
        integer words, ii, id
	integer nodes,tids,me,stride,ierr,stat(*) 
        common/mypvm/nodes,tids(0:63),me
        byte recvbuf(*) 
        logical test_verb
        data stride/1/

        if (nodesel.eq.-1) then
           tid=-1
        else
           tid=tids(nodesel)
        endif
	call pvmfrecv (tid,tag,id) 
        if(id.lt.0)call pg_errmsg('pg_rcv:pvmfrecv',id)

        call pvmfbufinfo(id, bytes, intag, tid, ierr)
        if(test_verb(3))
     &       write(6,*)'recv returns ',ierr,' bytes ',bytes,
     &       ' tid ',tid,' tag ',intag
c
c  locate sending process
        stat(1) = -1
        do ii = 0,nodes-1
           if(tids(ii) .eq. tid)stat(1) = ii
        enddo
        if(stat(1) .eq. -1)then
           call pg_errmsg('bad recv tid',tid)
        endif

        words = bytes 
        if(words .gt. lenbuf )then
           call pg_errmsg('bmpi_recv:message too long',-1)
        endif

	call pvmfunpack (BYTE1,recvbuf,count,stride,ierr)  
        if(ierr.ne.0)call pg_errmsg('impi_recv:pvmfunpack',-1)
_ELSE
      call caserr('pg_rcv')
_ENDIF
      return
      end

_IF(ga)
_IF(t3d)
c
c currently seems to be missing
c
      integer function ga_id_to_msg_id(id)
      implicit none
      integer id
      ga_id_to_msg_id = id      
      end
_ELSEIFN(hp700,hp800,ipsc)
c
c for newer ga (eg currently on SP2 and challenge but not DL HP or iPSC)
c
      integer function ga_id_to_msg_id(id)
      implicit none
      integer id
      integer iflag, list
      integer ipg_nnodes, iii
      common/nodeids/list(0:511)
      MA_INTEGER list8(0:511),nodes8
      save iflag
      data iflag/0/
      if(iflag .eq.0)then
c
c determine mapping list from tcgmsg
c
         nodes8 = ipg_nnodes()
         call ga_list_nodeid(list8,nodes8)
         do iii = 0,nodes8-1
            list(iii) = list8(iii)  
         enddo
         iflag = 1
      endif
      ga_id_to_msg_id = list(id)
      return
      end
_ENDIF
_ENDIF

c
c>>>>>>>>>>>>>>>>> old file below here <<<<<<<<<<<<<<<<<<<<<
c
c  should discontinue using these stubs, so as to 
c  help restrict the calls to the intel code, but the
c  parallel i/o "uses" them so heavily they are kept 
c
c - except dclock, as it is so widely used, now in machscf.m
c
_IFN(ipsc)
_IF(parallel)
      function isend()
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      call caserr('*** invalid isend call')
      isend = 0
      return
      end
      function irecv()
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      call caserr('*** invalid irecv call')
      irecv = 0
      return
      end
      function iwrite()
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      call caserr('*** invalid iwrite call')
      iwrite = 0
      return
      end
      function iread()
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      call caserr('*** invalid iread call')
      iread = 0
      return
      end
      function iowt()
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      call caserr('*** invalid iowt call')
      iowt = 0
      return
      end
      subroutine crecv()
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      entry synget
      entry synput
      entry msgw
      entry cprob
      call caserr('*** invalid comms subroutine')
      return
      end
      subroutine csend(ityp,buffer,lenbuf,nodes,mpid)
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      dimension buffer(*)
      call caserr('invalid message from csend')
      return
      end
      function infocount()
      implicit REAL  (a-h,p-w),integer    (i-n),logical    (o)
      implicit character *8 (z),character *1 (x)
      implicit character *4 (y)
      call caserr('*** invalid infocount call')
      infocount = 0
      return
      end
_ENDIF
_ENDIF
c
c  utility routines
c
c test cpu availability - this seems to work quite well for 
c IBM SP2 nodes - poorer for HP700. probably some tuning
c of the sample period is required
c
      subroutine getload(fac)
      implicit none
      REAL dumdum
      common/foolery/dumdum
      REAL buf(3),w0,c0,w,c, fac
      integer i
      call gms_cputime(buf)
      c0 = buf(1)
      call walltime(w0)
      dumdum=0.0d0
      do i = 1,1000000
         dumdum=dumdum+sqrt(dble(i))
      enddo
      call gms_cputime(buf)
      c = buf(1)-c0
      call walltime(w)
      w = w -w0
      fac = c / w
      return
      end

      subroutine chtoi(i,s)
      integer i(*)
      character s*(*)
      ilen=len(s)
      do 10 ii = 1,ilen
        i(ii)=ichar(s(ii:ii))
10    continue
      return
      end
      subroutine itoch(i,s)
      integer i(*)
      character s*(*)
      ilen=len(s)
      do 10 ii = 1,ilen
        s(ii:ii)=char(i(ii))
10    continue
      return
      end

c  simple internal write fix (needed for apollo)
      subroutine intwrt(s,ip,iv,ic,otrunc)
      character cc(8)*(1), num(9)*(1), s*(*)
      logical otrunc
      otrunc=.false.
      do 10 iii=1,8
 10         cc(iii)='0'
      do 11 iii=1,9
 11      num(iii)=char(iii+ichar('1')-1)
      itmp=iv
      im=100000000
      itest=itmp/im
      if(itest.ne.0)then
         otrunc=.true.
         itmp=itmp-im*itest
      endif
      im=im/10
      do 20 iii=1,8
         itest=itmp/im
         if(itest.ne.0)then
            if(iii.le.8-ic)then
               otrunc=.true.
            else
               cc(iii)=num(itest)
            endif
         endif
         itmp=itmp-itest*im
 20      im=im/10
      do 30 iii=1,ic
 30      s(ip+iii:ip+iii)=cc(8-ic+iii)
      ip=ip+ic
      return
      end
c
c ======================================================================
c  control of error messages
c

      block data verbodat
      implicit none
      integer iverb, ilevel
      common/verbo/iverb(100),ilevel
      data iverb/100*0/
      data ilevel/0/
      end

      subroutine push_verb(i)
      implicit none
      integer iverb, ilevel
      common/verbo/iverb(100),ilevel
      integer i
      ilevel = ilevel + 1
      if(ilevel.eq.101)then
         call pg_errmsg('recursion gone mad',-1)
      endif
      iverb(ilevel)=i
      return
      end

      subroutine pop_verb
      implicit none
      integer iverb, ilevel
      common/verbo/iverb(100),ilevel
      ilevel = ilevel -1
      if(ilevel.le.0)then
         call pg_errmsg('pop_verb gone mad',-1)
      endif
      return
      end
c
      logical function test_verb(i)
      implicit none
      integer i
      integer iverb, ilevel
      common/verbo/iverb(100),ilevel
      if(ilevel.eq.0)then
         call pg_errmsg('bad initialisation or pop_verb error',-1)
      endif
      test_verb = i.le.iverb(ilevel)
      return
      end
c
      integer function get_verb()
      implicit none
      integer iverb, ilevel
      common/verbo/iverb(100),ilevel
      get_verb = iverb(ilevel)
      return
      end
_IF(ga)
c
c  i4->i8 conversion mappings
c
c    Always used, but generally perform no function.
c    Currently (July 98) they are important on DEC running OSF
c    Also SGI platforms depending on build flags (but not
c    the SGI_N32 build)
c
c    Note in one case (ga_locate_region) there is an extra argument.
c
      subroutine pg_get(tag,i1,i2,i3,i4,buff,i5)
      implicit none
      integer i1,i2,i3,i4,i5,tag
      MA_INTEGER i1_8,i2_8,i3_8,i4_8,i5_8,tag_8
      REAL buff
      i1_8 = i1
      i2_8 = i2
      i3_8 = i3
      i4_8 = i4
      i5_8 = i5
      tag_8=tag
      call ga_get(tag_8,i1_8,i2_8,i3_8,i4_8,buff,i5_8)
      return
      end
 
      subroutine pg_put(tag,i1,i2,i3,i4,buff,i5)
      implicit none
      integer i1,i2,i3,i4,i5,tag
      MA_INTEGER i1_8,i2_8,i3_8,i4_8,i5_8,tag_8
      REAL buff
      i1_8 = i1
      i2_8 = i2
      i3_8 = i3
      i4_8 = i4
      i5_8 = i5
      tag_8 = tag
      call ga_put(tag_8,i1_8,i2_8,i3_8,i4_8,buff,i5_8)
      return
      end
      
      subroutine pg_acc(tag,i1,i2,i3,i4,buff,i5,fact)
      implicit none
      integer i1,i2,i3,i4,i5,tag
      MA_INTEGER i1_8,i2_8,i3_8,i4_8,i5_8,tag_8
      REAL buff, fact
      i1_8 = i1
      i2_8 = i2
      i3_8 = i3
      i4_8 = i4
      i5_8 = i5
      tag_8 = tag
      call ga_acc(tag_8,i1_8,i2_8,i3_8,i4_8,buff,i5_8,fact)
      return
      end
      
      subroutine pg_error(s,i)
      implicit none
      character s*(*)
      integer i
      MA_INTEGER i8
      i8=i
      call ga_error(s,i8)
      return
      end
      
      logical function pg_create(type,dim1,dim2,array_name,
     &     chunk1, chunk2, tag)
      implicit none
#include "mafdecls.fh"
      integer type, dim1, dim2, chunk1, chunk2, tag
      MA_INTEGER type_8, dim1_8, dim2_8, chunk1_8, chunk2_8, tag_8
      character*(*)  array_name
      MA_LOGICAL  ga_create

      dim1_8 = dim1
      dim2_8 = dim2
      chunk1_8 = chunk1
      chunk2_8 = chunk2
      type_8=MT_DBL

      pg_create = ga_create(type_8, dim1_8, dim2_8, array_name, 
     &     chunk1_8, chunk2_8, tag_8)
      tag = tag_8

      return
      end


      subroutine pg_distribution(tag, node, i1, i2, i3, i4)
      implicit none
      integer tag, node, i1, i2, i3, i4
      MA_INTEGER tag_8, node_8, i1_8,  i2_8,  i3_8,  i4_8

      tag_8 = tag
      node_8 = node
      call ga_distribution(tag_8, node_8, i1_8, i2_8, i3_8, i4_8)
      i1 = i1_8
      i2 = i2_8
      i3 = i3_8
      i4 = i4_8
      return
      end

      logical function pg_destroy(handle)
      implicit none
      integer handle
      MA_INTEGER handle_8
      MA_LOGICAL ga_destroy
      handle_8 = handle
      pg_destroy = ga_destroy(handle_8)
      return
      end
c
c  NOTE extra argument - work array must be allocated in 
c  calling procedure
c
      logical function pg_locate_region(handle, 
     &     ilo, ihi, jlo, jhi, map, np, map_8)

      implicit none
      integer handle, ilo,ihi,jlo,jhi,map(5,*),np

      MA_INTEGER handle_8,ilo_8,ihi_8,jlo_8,jhi_8,np_8, map_8(5,*)
      MA_LOGICAL ga_locate_region
      logical result
      integer i,j

      handle_8 = handle
      ilo_8 = ilo
      ihi_8 = ihi
      jlo_8 = jlo
      jhi_8 = jhi

      result = ga_locate_region(handle_8,
     &     ilo_8,ihi_8,jlo_8,jhi_8,map_8,np_8)

      pg_locate_region = result
      if(.not.result)return

      np = np_8
      do i=1,np
         do j = 1,5
            map(j,i)=map_8(j,i)
         enddo
      enddo

      return
      end

      subroutine pg_zero(handle)
      implicit none
      integer handle
      MA_INTEGER handle_8
      handle_8 = handle
      call ga_zero(handle_8)
      return
      end

      subroutine pg_dscal_patch(handle1,ix1,ix2,iy1,iy2,fac)
      implicit none
      integer handle1,ix1,ix2,iy1,iy2
      MA_INTEGER handle1_8,ix1_8,ix2_8,iy1_8,iy2_8
      REAL fac
      handle1_8 = handle1
      ix1_8 = ix1
      ix2_8 = ix2
      iy1_8 = iy1
      iy2_8 = iy2
      call ga_dscal_patch(handle1_8,ix1_8,ix2_8,iy1_8,iy2_8,fac)
      return
      end

      REAL function pg_ddot(handle1,handle2)
      implicit none
      integer handle1, handle2
      MA_INTEGER handle1_8, handle2_8
      REAL ga_ddot
_IF(hp700)
      REAL `vec_$ddot'
_ELSEIF(cray,t3d)
      REAL `sdot'
_ELSE
      REAL ddot
_ENDIF
      REAL temp
      integer ilo, ihi, jlo, jhi, ipg_nodeid,igmem_alloc
      integer ibuff1, ibuff2, nx, ny
INCLUDE(common/vcore)
      if(handle1 .eq. handle2)then
         call pg_distribution(handle1, ipg_nodeid(), ilo, ihi, jlo, jhi)
         nx = ihi - ilo + 1
         ny = jhi - jlo + 1
         ibuff1 = igmem_alloc(nx*ny)
         ibuff2 = igmem_alloc(nx*ny)
         call pg_get(handle1,ilo, ihi, jlo, jhi,Q(ibuff1),nx)
         call pg_get(handle2,ilo, ihi, jlo, jhi,Q(ibuff2),nx)
         temp = ddot(nx*ny,Q(ibuff1),1,Q(ibuff2),1)
         call pg_dgop(2001,temp,1,'+')
         pg_ddot = temp
         call gmem_free(ibuff2)
         call gmem_free(ibuff1)
      else
         handle1_8 = handle1
         handle2_8 = handle2
         pg_ddot =  ga_ddot(handle1_8, handle2_8)
      endif
      return
      end
c
c specially extended to cover case where handle1 = handle3 for diis
c solver
c
      subroutine pg_dadd(fac1,handle1,fac2,handle2,handle3) 
      implicit none
      integer handle1, handle2, handle3
      MA_INTEGER handle1_8, handle2_8, handle3_8
      REAL fac1, fac2

      integer ilo, ihi, jlo, jhi, ipg_nodeid,igmem_alloc
      integer ibuff1, ibuff2, nx, ny, iii
INCLUDE(common/vcore)

      if(handle1 .eq. handle3)then
         call pg_distribution(handle1, ipg_nodeid(), ilo, ihi, jlo, jhi)
         nx = ihi - ilo + 1
         ny = jhi - jlo + 1
         ibuff1 = igmem_alloc(nx*ny)
         ibuff2 = igmem_alloc(nx*ny)
         call pg_get(handle1,ilo, ihi, jlo, jhi,Q(ibuff1),nx)
         call pg_get(handle2,ilo, ihi, jlo, jhi,Q(ibuff2),nx)
         do iii = 1,nx*ny
            Q(ibuff1 + iii - 1) = 
     &           Q(ibuff1 + iii - 1)*fac1 +
     &           Q(ibuff2 + iii - 1)*fac2
         enddo
         call pg_put(handle1,ilo, ihi, jlo, jhi,Q(ibuff1),nx)
         call gmem_free(ibuff2)
         call gmem_free(ibuff1)
         call pg_synch(2002)
      else
         handle1_8 = handle1
         handle2_8 = handle2
         handle3_8 = handle3
         call ga_dadd(fac1,handle1_8,fac2,handle2_8,handle3_8) 
      endif
      return
      end

      subroutine pg_print(handle)
      implicit none
      integer handle
      MA_INTEGER handle_8
      handle_8 = handle
      call ga_print(handle_8)
      return
      end

      subroutine pg_inquire(handle, type, nx, ny)
      implicit none
      integer handle, type, nx, ny
      MA_INTEGER handle_8, type_8, nx_8, ny_8
      handle_8 = handle
      call ga_inquire(handle_8,type_8,nx_8,ny_8)
      type = type_8
      nx = nx_8
      ny = ny_8
      return
      end

      subroutine pg_dgemm(x1,x2,n1,n2,n3,fac1,
     &     handle1, handle2, fac2, handle3)
      REAL fac1, fac2
      character*1 x1,x2
      integer n1,n2,n3
      integer handle1, handle2, handle3
      MA_INTEGER handle1_8, handle2_8, handle3_8
      MA_INTEGER n1_8, n2_8, n3_8

      n1_8 = n1
      n2_8 = n2
      n3_8 = n3
      handle1_8 = handle1
      handle2_8 = handle2
      handle3_8 = handle3

      call ga_dgemm(x1,x2,n1_8,n2_8,n3_8,fac1,
     &     handle1_8, handle2_8, fac2, handle3_8)

      return
      end

_ENDIF
c
c                        Memory allocation routines
c
c  These routines return offsets to dynamically allocated 
c  memory addresses. 
c
c  the addresses returned by igmem_alloc(size) may be used in one
c  of two ways:
c
c     i) they may be resolved as references into the core array
c        directly (core is as passed from mains to master):
c
c            subroutine junk(core,....)
c            real*8 core(*)
c            ....
c            i10 = igmem_alloc(n*n)
c            do i=1,n*n
c               core(i10 + i - 1) = funct(i)
c            enddo
c
c    ii) they may be considered as references into the common/vcore/qq
c        array when offset by ivoff (stored in common/vcoreoff/)
c
c        This offset is incorporated into the source during m4 processing
c        if the macro Q() is used.
c
c   gmem_set(q) 
c
c   gmem_set is called once at the start of the run to establish the
c   relationship between the core array (generally denoted q) which is
c   passed to the driver routines, and specific arrays in common 
c
c   The exact function on the routine depends on the underlying mechanism
c   used to obtain the memory.
c
c   GA-tools
c
c   When the GA tools are used the offset between the q(1) and the dbl_mb
c   array is stored (in real*8 words) as iqoff.
c
c   UNIX-memory:
c
c   When memory is being allocated dynamically from the OS, the 
c
c   When the gmem_routines are simply allocating memory from the GAMESS-UK 
c   stack, via getscm the offset between the qq array in vcore
c
      subroutine gmem_set(q)
      implicit none
INCLUDE(common/gmemdata)
INCLUDE(common/vcore)
      REAL q(*)
c
c
_IF(ma)
#include "mafdecls.fh"
c
c  in the MA-case we will need to know the offset between
c  the gamess core and the dbl_mb array tha MA returns its
c  addresses with respect to
c
      call gmem_c_pointer_diff(q(1),dbl_mb(1),iqoff)
_ENDIF
c
c now compute ivoff
c
      call gmem_c_pointer_diff(q(1),qq(1),ivoff)
      numheap = 0
      igmem_count = 0
      return
      end
c


c ********************************************************************
c * igmem_alloc(size) :  allocate a segment of memory 
c *
c *                size - size of memory segment (input)
c *
c ********************************************************************

      integer function igmem_alloc(size)

      implicit none
INCLUDE(common/gmemdata)
_IF(ma)
#include "mafdecls.fh"
      MA_INTEGER size_8, type_8, itag_8, iq_8
_ELSEIF(dynamic_memory)
INCLUDE(common/vcore)
_ELSE
INCLUDE(common/vcore)
INCLUDE(common/segm)
INCLUDE(common/iofile)
       integer i10, need, loc10, loccm,  loadcm
_ENDIF
      logical ostat, opg_root
      character tag*30
      integer size
c
      numheap = numheap + 1
      write(tag,100)numheap
 100  format('gamess_',i3.3)

_IF(ma)
      type_8 = MT_DBL
      size_8 = size
c MA tools wont allow zero allocation
      if(size_8.eq.0)size_8=1
      ostat = ma_alloc_get(
     &     type_8,
     &     size_8,
     &     tag,
     &     itag_8,
     &     iq_8)
      itag_heap(numheap) = itag_8
c
c  store as the GAMESS-UK core
c	
      iq_heap(numheap) = iq_8 - iqoff
c
_ELSEIF(dynamic_memory)
c
c handle is actually the exact memory address relative to 
c the vcore qq array. eventually, it may differ from
c iq_heap because of the need to ensure correct allignment
c
      call mallocc2(qq(1),qq(2),size,iq_heap(numheap),
     &     itag_heap(numheap))
c
      ostat = (iq_heap(numheap) .ne. 0)
c
_ELSE
c
c implementation in terms of GAMESS-UK stack
c
c  based on model code...
c      length=n*n           
c      call cmem(loadcm  sets loadcm to top of core  (ntotly)
c      call setscm(i10)  get address of next scm bloc
c            k of core (ntotly + 1)
c      last=i10+length      !  
c      loc10=loccm()        !   ( returns ntotly )
c      need=loc10+length    !   
c      call setc(need)      !
c
c
c get current top of core
      call cmem(loadcm) 
c
c return address of the nect scm block of core
      call setscm(i10)
c
c loccm currently always returns ntotly
      loc10 = loccm()
c
c new top address, allowing 4 words for guards
      need = loc10 + size + 4

      if(oprintm) then
         write(iwr,*) 'loadcm ',loadcm,
     &        ' loc10 ',loc10,' i10 ',i10
      endif

      if(need.gt.nmaxly)then
         write(iwr,200) size, nmaxly-loc10-4
 200     format(1x,'memory allocation error: need',i10,' avail ',i10)
         call caserr('memory ran out')
      endif

      call setc(need) 
c
c  determine stored offset (following MA convention)
c
      iq_heap(numheap) = i10 + 2
c
c  allocate a tag for tracing 
c
      igmem_count = igmem_count+1
      itag_heap(numheap) = igmem_count
c
c the gamess allocator as currently implemented uses a single
c physical stack - so the address allocated is 1 greater than 
c the "loadcm" value used to free the data. We use this
c assumption, so check for changes that may invalidate it
c
      if(i10 .ne. loadcm+1)call caserr('addressing error')
c
c define values of guards
c
      iqoff=0

      Q(iq_heap(numheap)-iqoff-2) = size
      Q(iq_heap(numheap)-iqoff-1) = 111.0d0
      Q(iq_heap(numheap)-iqoff+size) = 222.0d0
      Q(iq_heap(numheap)-iqoff+size+1) = size
c
      ostat = .true.
c
_ENDIF
      if (.not.ostat)then
          if(opg_root())then
             call ma_summarize_allocated_blocks
             write(6,*)'allocation failed:', size, 'words'
          endif
          call caserr('allocating heap ')
      endif
c
      igmem_alloc = iq_heap(numheap) 

      if(opg_root() .and. ogmem_debug )
     &     write(6,*)'allocate ',tag(1:10),' size=',size,' handle= ',
     &     itag_heap(numheap),'  gamess address=', iq_heap(numheap)

      end

      function igmem_max_memory ()
c
      implicit none
      integer igmem_alloc_all, igmem_max_memory
      integer nmaxly, ioff
c
c     determine total memory currently available, and use
c     this in deriving data storage. Note that the allocation
c     and subsequent freeing may be inefficient.
c
      ioff = igmem_alloc_all(nmaxly)
      igmem_max_memory = nmaxly
c     now free memory
      call gmem_free(ioff)
c
      return
      end

c ********************************************************************
c *
c * igmem_alloc_all(size) :  allocate all memory available
c *
c *                size - size of memory segment (output)
c *
c ********************************************************************

      integer function igmem_alloc_all(size)

      implicit none
INCLUDE(common/gmemdata)
      integer size
      character tag*30
      integer lentag
      logical ostat
      logical opg_root

_IF(ma)
#include "mafdecls.fh"
c      MA_INTEGER ma_inquire_heap
c      MA_LOGICAL ma_alloc_get

      MA_INTEGER type_8, size_8, itag_8, iq_8
c
      numheap = numheap + 1

      tag="all_remaining_mem"

      type_8 = MT_DBL
      size_8 = ma_inquire_heap(type_8)
      ostat = ma_alloc_get(
     &     type_8,
     &     size_8,
     &     tag,
     &     itag_8,
     &     iq_8)
      itag_heap(numheap) = itag_8
      iq_heap(numheap) = iq_8 - iqoff
      size = size_8

_ELSEIF(dynamic_memory)
      igmem_alloc_all = 0
      call caserr('gmem func not available')
_ELSE
c
c  GAMESS-UK stack implementation
c
INCLUDE(common/vcore)
INCLUDE(common/segm)
INCLUDE(common/iofile)
      integer loadcm, i10, loc10, loccm, need
c
c  based on model code...
c      length=n*n           
c      call cmem(loadcm  sets loadcm to top of core  (ntotly)
c      call setscm(i10)  get address of next scm bloc
c            k of core (ntotly + 1)
c      last=i10+length      !  
c      loc10=loccm()        !   ( returns ntotly )
c      need=loc10+length    !   
c      call setc(need)      !
c
      numheap = numheap + 1

      tag="all_remaining_mem"

c get current top of core
      call cmem(loadcm)   

c return address of the nect scm block of core
      call setscm(i10)

c loccm currently always returns ntotly
      loc10 = loccm()
c
c work out available size allowing 4 words for guards
      size = nmaxly - i10 - 4
      need = nmaxly

      if(size.lt.0)then
         write(iwr,200)
 200     format(1x,'memory allocation error: gmem_alloc_all but no',
     +        ' core remains')
         call caserr('memory ran out')
      endif

      call setc(need) 
c
c  determine stored offset (following MA convention
c
c      write(6,*)'gamess addr',i10
      iq_heap(numheap) = i10 + 2
c
c  allocate a tag for tracing 
c
      igmem_count = igmem_count+1
      itag_heap(numheap) = igmem_count
c
c the gamess allocator as currently implemented uses a single
c physical stack - so the address allocated is 1 greater than 
c the "loadcm" value used to free the data. We use this
c assumption, so check for changes that may invalidate it
c
      if(i10 .ne. loadcm+1) call caserr('addressing error')
c
c define values of guards
c
      iqoff=0

      Q(iq_heap(numheap)-iqoff-2) = size
      Q(iq_heap(numheap)-iqoff-1) = 111.0d0
      Q(iq_heap(numheap)-iqoff+size) = 222.0d0
      Q(iq_heap(numheap)-iqoff+size+1) = size
c
      ostat = .true.
c
_ENDIF
      if (.not.ostat)call caserr('allocating heap ')
c
      igmem_alloc_all = iq_heap(numheap)

      call strtrm(tag,lentag)
      if(opg_root() .and. ogmem_debug)
     &     write(6,*)'allocate all tag= ',tag(1:lentag),
     &     ' size= ',size,' handle= ',
     &     itag_heap(numheap),'  gamess address=', iq_heap(numheap)

      return
      end

c **********************************************************************
c *
c *   gmem_free(iq) : free memory at offset iq
c *
c **********************************************************************

      subroutine gmem_free(iq)
      implicit none
c
INCLUDE(common/gmemdata)
_IF(ma)
c
c decls to replace include of "mafdecls.h"
c
      MA_INTEGER itag_8
      MA_LOGICAL ma_free_heap
_ELSEIF(dynamic_memory)
INCLUDE(common/vcore)
      integer istat
_ELSE
INCLUDE(common/vcore)
INCLUDE(common/iofile)
      integer loadcm, size
_ENDIF
c
      logical ostat, opg_root
      integer iq, ix
      ostat = .true.
      do  ix = 1,numheap
         if(iq.eq. iq_heap(ix))then
            if(ix .ne. numheap)call caserr('non-stack memory')
_IF(ma)
            itag_8 = itag_heap(ix)
            ostat = ma_free_heap(itag_8)
_ELSEIF(dynamic_memory)
            call freec(qq(1),itag_heap(ix),istat)
            ostat = istat .eq. 0
_ELSE
c
c implementation in terms of GAMESS-UK stack
c
c check guards
c
            size = nint(Q(iq_heap(ix)-2))
            if(ogmem_debug)then
               call gmem_check_guards

               write(6,*)size,Q(iq_heap(ix)-1)
               write(6,*)nint(Q(iq_heap(ix)+size+1)),
     &              Q(iq_heap(ix)+size)
            endif

            if(size.lt.0.or.
     &           Q(iq_heap(ix)-1).ne.111.0d0)then
               write(iwr,100)itag_heap(ix),iq_heap(ix)
 100           format(1x,'problem with lower guard, memory handle',i7,
     &                ' address',i7)
               call caserr('problem with memory guard')
            endif

            if(nint(Q(iq_heap(ix)+size+1)) .ne. size .or.
     &           Q(iq_heap(ix)+size).ne.222.0d0)then
               write(iwr,101)itag_heap(ix),iq_heap(ix)
 101           format(1x,'problem with upper guard, memory handle',i7,
     &                ' address',i7)
               call caserr('problem with memory guard')
            endif

            loadcm = iq_heap(ix) - 3
            call setc( loadcm )
_ENDIF
            if(opg_root() .and. ogmem_debug)
     &           write(6,*)'free memory handle= ',itag_heap(ix)
            numheap = numheap - 1
            if (.not.ostat)call caserr('problem freeing memory ')
            return
         endif
      enddo

      if(opg_root())write(6,*)'attempt to free address= ',iq

      call caserr('gmem_free - bad address')
      end
c
c  - allocate a reserved region
c
      integer function igmem_reserve_region(size)
      implicit none
INCLUDE(common/gmemdata)
_IF(ma)
#include "mafdecls.fh"
      MA_INTEGER size_8, type_8, itag_8, iq_8
      logical ostat
_ELSE
INCLUDE(common/vcore)
_ENDIF
      integer size
      logical opg_root
      character tag*30

      nresreg = nresreg + 1
      if(nresreg.gt.mxresreg)call 
     &     caserr('too many reserved memory regions requested')

      numheap = numheap + 1
      write(tag,100)nresreg
 100  format('reserve_',i3.3)

_IF(ma)
      type_8 = MT_DBL
      size_8 = size
      ostat = ma_alloc_get(
     &     type_8,
     &     size_8,
     &     tag,
     &     itag_8,
     &     iq_8)
      itag_heap(numheap) = itag_8
      iq_heap(numheap) = iq_8 - iqoff
      if (.not.ostat)call caserr('allocating heap ')
_ELSE
      call caserr('gmem func unavailable')
_ENDIF
c
      iadr(0,nresreg) = iq_heap(numheap) - 1
      isize(nresreg) = size
      nres(nresreg) = 0
      igmem_reserve_region = nresreg
      if(opg_root()  .and. ogmem_debug)then
         write(6,*)'reserve region tag=',tag(1:10),
     &        'base address =',iadr(0,nresreg) + 1,' size=',size
      endif
      return
      end

      subroutine gmem_free_region(ires)
      implicit none

INCLUDE(common/gmemdata)
_IF(ma)
c
c replacements for include "mafdecls.h"
c
      MA_LOGICAL ma_free_heap
      MA_INTEGER tag_8
      logical ostat
_ELSE
INCLUDE(common/vcore)
_ENDIF
      integer ires
      logical opg_root
      integer iq, ix
      if(ires.ne.nresreg)call 
     &     caserr('bad gmem_free_region call')

      iq = iadr(0,nresreg) + 1

      do  ix = 1,numheap
         if(iq.eq. iq_heap(ix) )then
            if(ix .ne. numheap)call caserr('non-stack memory')
_IF(ma)
      tag_8 = itag_heap(ix)
            ostat = ma_free_heap(tag_8)
_ELSE
      call caserr('gmem func unavailable')
_ENDIF
            if(opg_root()  .and. ogmem_debug)
     &           write(6,*)'free reserver handle= ',itag_heap(ix)
            numheap = numheap - 1
cccc
            nresreg = nresreg - 1
cccc
            return
         endif
      enddo
      call caserr('gmem_free - bad address')
      end
c
      integer function igmem_alloc_reserved(ires,size)
      implicit none
INCLUDE(common/gmemdata)
      integer size, nleft, ires
      logical opg_root
c
c check space..
c
      if(ires.lt.0.or.ires.gt.nresreg)call 
     &     caserr('bad reserved memory region index')

      nleft = isize(ires) - ( iadr(nres(ires),ires) - 
     &     iadr(0,ires) )
      if(nleft . lt . size)call 
     &     caserr('reserved memory region too small')
      nres(ires) = nres(ires) + 1
      if(nres(ires) .gt. mxressect)call
     &     caserr('too many sections from reserved region')
      iadr(nres(ires),ires) = iadr(nres(ires) - 1,ires) + size
      igmem_alloc_reserved = iadr(nres(ires) - 1,ires) + 1
      if(opg_root()  .and. ogmem_debug)then
        write(6,*)'mem from reserved region=',ires,'index=',nres(ires),
     &        'address=',igmem_alloc_reserved
      endif
      return
      end

      subroutine gmem_free_reserved(ires,i)
      implicit none
INCLUDE(common/gmemdata)
      logical opg_root
      integer ires, i
      if(ires.lt.0.or.ires.gt.nresreg)call 
     &     caserr('bad reserved memory region index')
      if(opg_root()  .and. ogmem_debug)then
         write(6,*)'free mem from reserved region=',ires,
     &        'index=',nres(ires),
     &        'address=',iadr(nres(ires) - 1,ires) + 1
         if(iadr(nres(ires) - 1,ires) .ne. i - 1)call
     &        caserr('non-stack gmem_free_reserved call')
      endif
      nres(ires) = nres(ires) - 1
      return
      end
_IFN(ma)
      subroutine ma_summarize_allocated_blocks
      return
      end
_ENDIF


c ********************************************************************
c *
c * gmem_check_guards :  checks guards without freeing memory
c *
c *  
c *
c ********************************************************************

      subroutine gmem_check_guards
      implicit none
INCLUDE(common/gmemdata)
INCLUDE(common/vcore)
INCLUDE(common/iofile)

_IF(ma)
      logical MA_verify_allocator_stuff, ostat
      write(iwr,*)'checking memory guards'
      ostat = MA_verify_allocator_stuff()
_ELSEIF(dynamic_memory)
      write(iwr,*)'checking memory guards not available'
_ELSE
      integer ix, size
      write(iwr,*)'checking memory guards numheap=',numheap
      do  ix = 1,numheap

         size = nint(Q(iq_heap(ix)-2))
         if(ogmem_debug)then
cc            write(6,*)size,Q(iq_heap(ix)-1)
cc            write(6,*)nint(Q(iq_heap(ix)+size+1)),
cc     &           Q(iq_heap(ix)+size)
         endif
 
         if(size.lt.0.or.
     &        Q(iq_heap(ix)-1).ne.111.0d0)then
            write(iwr,100)itag_heap(ix),iq_heap(ix)
 100        format(1x,'problem with lower guard, memory handle',i7,
     &             ' address',i7)
         endif
 
         if(nint(Q(iq_heap(ix)+size+1)) .ne. size .or.
     &        Q(iq_heap(ix)+size).ne.222.0d0)then
            write(iwr,101)itag_heap(ix),iq_heap(ix)
 101        format(1x,'problem with upper guard, memory handle',i7,
     &             ' address',i7)
         endif
         
      enddo
_ENDIF
      return
      end
c
c controls memory debug print
c
      subroutine gmem_set_debug(flag)
      implicit none
      logical flag
      INCLUDE(common/gmemdata)
      ogmem_debug = flag
      end
c
      subroutine memtest(q,nmaxly)
      implicit none
      REAL q(*)
      REAL small
      integer i, iq, iq2, nmaxly, itest

      integer igmem_alloc
      external igmem_alloc

INCLUDE(common/vcore)  

      small = 0.0001d0
c
c test passed q
c
      write(6,*)'test q'
      do i=1, nmaxly
         q(i) = 99.0d0
      enddo
      do i=1, nmaxly
         if(dabs(q(i) - 99.0d0) .gt. small)write(6,*)q(i)
      enddo
c

      itest = (nmaxly - 100) / 2
      write(6,*)'allocate',itest
      iq = igmem_alloc(itest)
      write(6,*)'allocate',itest
      iq2 = igmem_alloc(itest)

      write(6,*)'test q(iq)', iq, iq2
      do i=1, itest
         q(iq+i-1) = 99.0d0
      enddo
      do i=1, itest
         if(dabs(q(iq+i-1) - 99.0d0) .gt. small)write(6,*)q(i)
      enddo
          
      call gmem_free(iq2)
      call gmem_free(iq)

      write(6,*)'test Q(iq)'

      itest = nmaxly - 100
      iq = igmem_alloc(itest)

      do i=1, itest
         Q(iq+i-1) = 99.0d0
      enddo
      do i=1, itest
         if(dabs(Q(iq+i-1) - 99.0d0) .gt. small)
     &        write(6,*)Q(iq+i-1)
      enddo

      call gmem_free(iq)

      end
      subroutine ver_parallel(s,r,d)
      character*80 source
      character*30 revision
      character*30 date
      character s*(*), r*(*), d*(*)
      data source /
     +     "$Source: /c/qcg/cvs/psh/GAMESS-UK/m4/parallel.m,v $"
     +     /
      data revision /"$Revision: 1.49 $"/
      data date /"$Date: 1999/07/30 12:48:38 $"/
      s=source(9:)
      r=revision(11:)
      d=date(7:)
      return
      end

c>>>>>>>>>>>>>>>>>>>>>>>

_IF(pvm)

c
c   mappings from mpi to pvm3.2.6
c
c  mpi_init
c  mpi_finalize
c  mpi_comm_rank
c  mpi_comm_size
c  mpi_send
c  mpi_recv
c  mpi_bcast
c  mpi_allreduce
c  mpi_barrier
c  mpi_get_source
c
c  extra routine
c   pg_errmsg(message,code)
c
c
c  this version call hostcc from c.m, and supports MPI_COMM_WORKERS
c  as a communicator that leaves out the top process
c
c  verbosity levels 0 these routines are silent
c                   1 only start and end
c                   2 only master routines give o/p
c                   3 all routines identify themselves
c                   4 other diags
c
c current model
c      on calling a routine, the level is reduced by one
c
c  initialisation routine: spawn processes  
c  
c  interrogate to find rank (node number or `mynode')  
      subroutine MPI_COMM_RANK (comm,rank,ierr)
      implicit none 
      include 'mpif.h' 
      include '/tmp/pvm3/fpvm3.h'  
      integer comm,rank,ierr,nodes,tids,me 
      common/mypvm/nodes,tids(0:63),me
      if(comm .eq. MPI_COMM_WORLD ) then
         ierr = 0
         rank=me 
      else if(comm .eq. MPI_COMM_WORKERS ) then
         if(me .eq. nodes - 1)then
            rank = -1
            ierr =  1
         else
            ierr = 0
            rank=me 
         endif
      else
         call pg_errmsg('unknown communicator',-1)
      endif
      end

c  interrogate to find number of nodes/processes  
      subroutine MPI_COMM_SIZE (comm,size,ierr)
      implicit none 
      include 'mpif.h' 
      if(comm .eq. MPI_COMM_WORLD ) then
         ierr = 0
         size=nodes
      else if(comm .eq. MPI_COMM_WORKERS ) then
         ierr = 0
         size=nodes-1
      else
         call pg_errmsg('unknown communicator',-1)
      endif
      end

c  top level `synchronous' send 
      subroutine MPI_SEND (sendbuf,count,datatype,dest,tag,comm,ierr) 
      implicit none 
      include 'mpif.h'
      integer sendbuf(*), count, datatype, dest, tag, comm, ierr, rank
      integer icode
      logical test_verb
      ierr = 0

      call MPI_COMM_RANK(comm,rank,ierr)

      if(datatype .eq. MPI_DOUBLE_PRECISION .and.
     &     (comm .eq. MPI_COMM_WORLD .or.
     &     comm .eq. MPI_COMM_WORKERS ) ) then
         
         if(test_verb(2))
     &        write(6,*)'send ',count,' doubles from',rank,'to',dest,
     &        ' tag ',tag

         call DMPI_SEND (sendbuf,count,datatype,dest,tag,comm,ierr) 

      else if(datatype .eq. MPI_INTEGER.and.
     &        (comm .eq. MPI_COMM_WORLD .or.
     &        comm .eq. MPI_COMM_WORKERS ) ) then

         if(test_verb(2))
     &   write(6,*)'send ',count,' integers from',rank,' to',dest,
     &        ' tag ',tag
         call IMPI_SEND (sendbuf,count,datatype,dest,tag,comm,ierr) 


      else if(datatype .eq. MPI_BYTE.and.
     &        (comm .eq. MPI_COMM_WORLD .or. 
     &        comm .eq. MPI_COMM_WORKERS ) ) then

         if(test_verb(2))
     &   write(6,*)'send ',count,' bytes from',rank,' to',dest,
     &        ' tag ',tag
         call BMPI_SEND (sendbuf,count,datatype,dest,tag,comm,ierr) 

      else
         write(6,*)'missing routine in mpi_send'
         icode = -1
         call MPI_FINALIZE(icode)
      endif
      return
      end

c  double precision locally-blocking send 
      subroutine DMPI_SEND (sendbuf,count,datatype,dest,
     &     tag,comm,ierr) 
      implicit none 
      include 'mpif.h' 
      include '/tmp/pvm3/fpvm3.h'
      integer count,datatype,dest,tag,comm,stride,ierr 
      integer nodes,tids,me
      common/mypvm/nodes,tids(0:63),me
      real*8 sendbuf(*) 
      data stride/1/

c      write(6,*) 'pvmfinitsend:',PVMDEFAULT
      call pvmfinitsend (PVMDEFAULT,ierr) 
c      write(6,*) 'pvmfpack:',REAL8,sendbuf(1),count
      call pvmfpack (REAL8,sendbuf,count,stride,ierr) 
c      write(6,*) 'pvmfsend:',tids(dest),tag 
      call pvmfsend (tids(dest),tag,ierr) 
      end

c  integer locally-blocking send 
      subroutine IMPI_SEND (sendbuf,count,datatype,dest,
     &     tag,comm,ierr) 
      implicit none 
      include 'mpif.h' 
      include '/tmp/pvm3/fpvm3.h'
      integer count,datatype,dest,tag,comm,stride,ierr 
      integer nodes,tids,me
      common/mypvm/nodes,tids(0:63),me
      integer sendbuf(*) 
      data stride/1/

c      write(6,*) 'pvmfinitsend:',PVMDEFAULT
      call pvmfinitsend (PVMDEFAULT,ierr) 
c      write(6,*) 'pvmfpack:',INTEGER4,sendbuf(1),count
      call pvmfpack (INTEGER4,sendbuf,count,stride,ierr) 
c      write(6,*) 'pvmfsend:',tids(dest),tag 
      call pvmfsend (tids(dest),tag,ierr) 
      end




c  double precision receive 
      subroutine MPI_RECV (recvbuf,count,datatype,source,
     &     tag,comm,stat,ierr) 
      implicit none
      include 'mpif.h' 
      integer count,datatype,source,tag,comm,rank
      integer ierr,stat(*) 
      integer recvbuf(*) 
      logical test_verb
      ierr = 0

      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

      if(datatype .eq. MPI_DOUBLE_PRECISION .and.
     &     (comm .eq. MPI_COMM_WORLD .or.
     &     comm .eq. MPI_COMM_WORKERS ) ) then


         if(test_verb(2))
     &   write(6,*)'recv ',count,' doubles from',source,' to ',rank,
     &        ' tag ',tag
         call DMPI_RECV (recvbuf,count,datatype,source,
     &        tag,comm,stat,ierr) 

      else if(datatype .eq. MPI_INTEGER.and.
     &        (comm .eq. MPI_COMM_WORLD .or.
     &        comm .eq. MPI_COMM_WORKERS ) ) then

         if(test_verb(2))
     &   write(6,*)'recv ',count,' integers from',source,' to ',rank,
     &        ' tag ',tag
         call IMPI_RECV (recvbuf,count,datatype,source,
     &        tag,comm,stat,ierr) 

      else if(datatype .eq. MPI_BYTE.and.
     &        (comm .eq. MPI_COMM_WORLD .or.
     &        comm .eq. MPI_COMM_WORKERS ) ) then

         if(test_verb(2))
     &   write(6,*)'recv ',count,' bytes from',source,' to ',rank,
     &        ' tag ',tag
         call BMPI_RECV (recvbuf,count,datatype,source,
     &        tag,comm,stat,ierr) 

      else
         call pg_errmsg('missing routine in mpi_recv',-1)
      endif

      end
c
c  double precision receive 
	subroutine DMPI_RECV (recvbuf,count,datatype,source,
     &	tag,comm,stat,ierr) 
        implicit none
	include 'mpif.h' 
        include '/tmp/pvm3/fpvm3.h'
        integer count,datatype,source,tag,comm,tid,intag,ii
	integer nodes,tids,me,stride,ierr,stat(*) 
        common/mypvm/nodes,tids(0:63),me
        logical test_verb
        real*8 recvbuf(*) 
        integer bytes, words, id
        data stride/1/

        if (source.eq.-1) then
           tid=-1
        else
           tid=tids(source)
        endif

c 	write(6,*) 'pvmfrecv:',tid,tag,count,tids(0),tids(1) 
	call pvmfrecv (tid,tag,id) 
        if(id.lt.0)call pg_errmsg('dmpi_recv:pvmfrecv',id)

        call pvmfbufinfo(id, bytes, intag, tid, ierr)
        if(test_verb(3))
     &       write(6,*)'recv returns ',ierr,' bytes ',bytes,
     &       ' tid ',tid,' tag ',intag
c
c  locate sending process
        stat(1) = -1
        do ii = 0,nodes-1
           if(tids(ii) .eq. tid)stat(1) = ii
        enddo
        if(stat(1) .eq. -1)then
           call pg_errmsg('bad recv tid',tid)
        endif

        words = bytes / 8
        if(words .gt. count )then
           call pg_errmsg('dmpi_recv:message too long',-1)
        endif

c	write(6,*) 'pvmfunpack:',recvbuf(1),count,stride  
	call pvmfunpack (REAL8,recvbuf,words,stride,ierr)  
        if(ierr.ne.0)call pg_errmsg('dmpi_recv:pvmfunpack',-1)
	end

        subroutine MPI_GET_SOURCE(stat,ifrom)
        implicit none
	include 'mpif.h' 
        integer stat(MPI_STATUS_SIZE), ifrom
        ifrom = stat(1)
        end

c  integer receive 
	subroutine IMPI_RECV (recvbuf,count,datatype,source,
     &	tag,comm,stat,ierr) 
        implicit none
	include 'mpif.h' 
        include '/tmp/pvm3/fpvm3.h'
        integer count,datatype,source,tag,comm,tid, words, bytes
	integer nodes,tids,me,stride,ierr,stat(*),id,intag, ii
        common/mypvm/nodes,tids(0:63),me
        integer recvbuf(*) 
        logical test_verb
        data stride/1/

        if (source.eq.MPI_ANY_SOURCE) then
           tid=-1
        else
           tid=tids(source)
        endif

	call pvmfrecv (tid,tag,id) 
        if(id.lt.0)call pg_errmsg('impi_recv:pvmfrecv',id)

        call pvmfbufinfo(id, bytes, intag, tid, ierr)
        if(test_verb(3))
     &       write(6,*)'recv returns ',ierr,' bytes ',bytes,
     &       ' tid ',tid,' tag ',intag
c
c  locate sending process
        stat(1) = -1
        do ii = 0,nodes-1
           if(tids(ii) .eq. tid)stat(1) = ii
        enddo
        if(stat(1) .eq. -1)then
           call pg_errmsg('bad recv tid',tid)
        endif

        words = bytes / 4
        if(words .gt. count )then
           call pg_errmsg('impi_recv:message too long',-1)
        endif
c	write(6,*) 'pvmfunpack:',recvbuf(1),count,stride  
	call pvmfunpack (INTEGER4,recvbuf,count,stride,ierr)  
        if(ierr.ne.0)call pg_errmsg('impi_recv:pvmfunpack',-1)
	end

c
c  global block to synchronise 
c this version doesnt need group server
      subroutine MPI_BARRIER (comm,ierr)  
      implicit none 
      include 'mpif.h' 
      include '/tmp/pvm3/fpvm3.h'  
      integer nodes,tids,me,comm,ierr
      common/mypvm/nodes,tids(0:63),me
      logical test_verb
c	character*5 group 
c	data group/'local'/ 
c	write(6,*) 'group: ',group,'  nodes: ',nodes   
c	call pvmfjoingroup (group,inst) 
c	call pvmfjoingroup ("local",inst) 
c	write(6,*) 'instance: ',inst
c	call pvmfbarrier (group,nodes,ierr)
c
c  there seems to be a problem with the groupserver..
      real*8 buff1, buff2
      ierr = 0
      if(test_verb(2))
     &     write(6,*) 'mpi_barrier'
      buff1 = 0.0
      call MPI_ALLREDUCE (buff1, buff2,1,MPI_DOUBLE_PRECISION,
     &     MPI_SUM, comm, ierr)
      end 
      
c     double precision global sum (simple N-step + broadcast method)   
      subroutine SMPI_ALLREDUCE (sendbuf,recvbuf,count,
     &     datatype,op,comm,ierr)  
      implicit none 
      include 'mpif.h' 
      include '/tmp/pvm3/fpvm3.h'  
      integer count,datatype,comm,tag,ierr,op
      integer stat(MPI_STATUS_SIZE) 
      integer nodes,tids,me,i,j,dest,source 
      common/mypvm/nodes,tids(0:63),me
      real*8 sendbuf(*),recvbuf(*)
      
      do 1 i=0,nodes-2 
         if(i.eq.me) then 
c     pass vector to next node 
            dest=me+1 
            write(6,*) 'node',me,'is sending to',dest
            call MPI_SEND (sendbuf,count,datatype,dest,
     &           tag,comm,ierr) 
         else if(me.eq.i+1) then  
c     receive vector and sum 
            source=me-1 
            write(6,*) 'node',me,'is receiving from',source   
            call MPI_RECV (recvbuf,count,datatype,source,
     &           tag,comm,stat,ierr) 
            do 2 j=1,count
               sendbuf(j)=sendbuf(j)+recvbuf(j) 
 2          continue
c     line for global product 
c2    sendbuf(j)=sendbuf(j)*recvbuf(j) 
         endif 
 1    continue 
         
c     broadcast from (n-1)th node 
      if(me.eq.nodes-1) then 
         write(6,*) 'node',me,'is broadcasting'
         do 3 i=1,count 
            recvbuf(i)=sendbuf(i) 
 3       continue
         call MPI_BCAST (sendbuf,count,datatype,me,comm,ierr) 
      else 
         source=nodes-1
         write(6,*) ' node',me,'is receiving from',source   
         call MPI_RECV (recvbuf,count,datatype,source,
     &        tag,comm,stat,ierr)
      endif 
      end  
_ENDIF
      subroutine debugp(s)
      implicit none
      character s*(*)
INCLUDE(common/parcntl)
      integer ipg_nodeid
      if(odebugp)then
         write(6,*)'PAR:',ipg_nodeid(),s
      endif
      end
