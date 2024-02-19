*CMZ :  2.02/01 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.01/08 14/08/2020  07.33.39  by  Michael Scheer
*-- Author :    Michael Scheer   13/08/2020
      subroutine undumag_bpolyeder_single(xin,yin,zin,bxout,byout,bzout,ifail)

      use omp_lib
      use bpolyederf90m
      use undumagf90m

      use commandlinef90m

      implicit none
*KEEP,seqdebug.
      include 'seqdebug.cmn'
*KEND.

      double precision xin,yin,zin,bxout,byout,bzout,bo(3,nthreadp)


      integer nmaxth,ith,ifail,ic,imag,
     &  kfail(nthreadp),kinsidelocal(nthreadp)

c calculate field at (xin,yin,zin)

      if (magmag.lt.0) then
        return
      endif !magmag.le.0

      nmaxth=1
      ith=1
      nmaxth=nthreads
      nmaxth=OMP_GET_MAX_THREADS()
      if (nthreads.gt.0) nmaxth=min(nmaxth,nthreads,nthreadp)
c        write(lun6,*)"Number of CPU cores used:",nmaxth

      bo=0.0d0
      kfail=0
      kinsidelocal=kinside

!$OMP PARALLEL NUM_THREADS(nmaxth) DEFAULT(PRIVATE)
!$OMP& SHARED(bo,kfail,kinsidelocal)
!$OMP& FIRSTPRIVATE(nmaxth,nmag,xin,yin,zin,magmag)
      ith=OMP_GET_THREAD_NUM()+1
      bo(1:3,ith)=0.0d0
!$OMP DO

      do imag=1,nmag

        call undumag_bpolyeder1_sym(imag,xin,yin,zin,bxout,byout,bzout,ifail)

        if (ifail.ne.0) kfail(ith)=ifail
        if (kinside.ne.0) kinsidelocal(ith)=kinside

        bo(1,ith)=bo(1,ith)+bxout
        bo(2,ith)=bo(2,ith)+byout
        bo(3,ith)=bo(3,ith)+bzout

      enddo !imag=1,nmag

!$OMP END DO
!$OMP END PARALLEL

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      ifail=0
      kinside=0

      do ic=1,nmaxth
        ifail=ifail+kfail(ic)
        if (kinsidelocal(ic).ne.0) kinside=kinsidelocal(ic)
        bxout=bxout+bo(1,ic)
        byout=byout+bo(2,ic)
        bzout=bzout+bo(3,ic)
      enddo

      return
      end
