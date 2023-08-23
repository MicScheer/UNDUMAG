*CMZ :  2.02/01 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.01/08 14/08/2020  10.22.50  by  Michael Scheer
*CMZ :  2.01/02 27/04/2018  12.08.59  by  Michael Scheer
*CMZ :  2.00/02 17/04/2018  13.43.36  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  15.38.41  by  Michael Scheer
*CMZ :  2.00/00 09/04/2018  12.17.36  by  Michael Scheer
*CMZ :  1.25/05 05/04/2018  19.27.23  by  Michael Scheer
*CMZ :  1.25/03 23/03/2018  17.02.28  by  Michael Scheer
*CMZ :  1.25/02 21/03/2018  12.54.05  by  Michael Scheer
*CMZ :  1.25/01 20/03/2018  16.04.47  by  Michael Scheer
*CMZ :  1.25/00 16/03/2018  12.51.10  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_bcoils_omp(xin,yin,zin,bxout,byout,bzout,istat)

      use omp_lib
      use undumagf90m

      implicit none

      double precision, dimension (:,:), allocatable :: wold
      double precision xin,yin,zin,bxout,byout,bzout,wire7(7,nthreadp),
     &  bx,by,bz,
     &  bo(3,nthreadp)

      integer istat,i,ith,nmaxth,nthreads

      character(2048) cline

      if (ncwires.eq.0) return

      istat=0
      bo=0.0d0

      nmaxth=1
      nmaxth=OMP_GET_MAX_THREADS()
      if (nthreads.gt.0) nmaxth=min(nmaxth,nthreads)

!$OMP PARALLEL NUM_THREADS(nmaxth) DEFAULT(PRIVATE)
!$OMP& SHARED(bo,wire)
!$OMP& FIRSTPRIVATE(ncwires,xin,yin,zin)
!$OMP DO

      do i=1,ncwires

        ith=OMP_GET_THREAD_NUM()+1
        if (wire(2,i).eq.0.0d0) cycle

        wire7(1,ith)=wire(2,i)
        wire7(2:7,ith)=wire(3:8,i)/1000.0d0

        call undumag_bwireana(wire7(1:7,ith),xin,yin,zin,bx,by,bz)

        bo(1,ith)=bo(1,ith)+bx
        bo(2,ith)=bo(2,ith)+by
        bo(3,ith)=bo(3,ith)+bz

      enddo

!$OMP END DO
!$OMP END PARALLEL

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      do ith=1,nmaxth
        bxout=bxout+bo(1,ith)
        byout=byout+bo(2,ith)
        bzout=bzout+bo(3,ith)
      enddo

      if (abs(bxout).lt.1.0d-9) bxout=0.0d0
      if (abs(byout).lt.1.0d-9) byout=0.0d0
      if (abs(bzout).lt.1.0d-9) bzout=0.0d0

      return
      end
