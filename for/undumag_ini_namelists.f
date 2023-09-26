*CMZ :  2.04/22 25/09/2023  12.27.21  by  Michael Scheer
*CMZ :  2.04/03 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/02 25/02/2022  09.54.41  by  Michael Scheer
*CMZ :  2.02/01 09/02/2022  18.49.27  by  Michael Scheer
*-- Author :    Michael Scheer   19/10/2021
      subroutine undumag_ini_namelists

      use commandlinef90m
      use undumagf90m
      use bpolyederf90m
      use magnets_structure
      use utilmod

      implicit none

*KEEP,random.
      integer*8 irancalls
      integer, parameter :: irnsize=64
      integer irnseed(irnsize),irnmode,irnseedi(irnsize)
      common /randomc/ irancalls,irnseed,irnmode,irnseedi

      namelist /randomn/ irnmode,irnseed
*KEND.

      integer lunnam,istat
      logical lexist

      character(128) c128

      inquire(file=trim(Fnam),exist=lexist)
      if (lexist.eqv..false.) then
        write(lun6,*)""
        write(lun6,*)"*** Error in undumag_ini_namelists: File " // trim(Fnam) // " not found ***"
        write(lun6,*)"*** Program UNDUMAG aborted ***"
        stop
      endif

      open(newunit=lunnam,file=trim(Fnam),form='formatted',status='old')
      read(lunnam,undumagn)
      read(lunnam,randomn)
      close(lunnam)

      kcalcvars=1 !to avoid problems with undumag.in if coating is used

      if (kbextern.eq.0) then
        bxex=0.0d0
        byex=0.0d0
        bzex=0.0d0
      endif

      rcvthron=abs(rcvthron)
      if (rcvthron.eq.0.0d0) rcvthron=1.0d30

      if (cuttiny.eq.0.0d0) cuttiny=0.1
      if (hulltiny.eq.0.0d0) hulltiny=1.0d-6

      if (nchiiron.le.0) nchiiron=1

      nchimax=0

      if (chicut.eq.-9999.0d0) chicut=max(abs(hconv),1.0d-10)
      corrtiny=corrtiny/1000.0d0 ! mm->meter

      open(unit=99,file='.util_spline_or_simpson_integral.dat')
      write(99,*)isimpson
      if (isimpson.gt.0) then
        write(lun6,*)
        write(lun6,*)"*** Simpson integrations applied ***"
        write(lun6,*)
      endif
      flush(99)
      close(99)

      hconva=abs(hconv)
      if (hconv.le.-1000.0d0) hconva=0.0d0

      if (dampiron.eq.0.0d0.or.dampiron.gt.1.0d0) then
        dampiron=1.0d0
      endif

      if (maxiteriron.eq.-9999.and.dampiron.lt.1.0d0) then
        maxiteriron=nint(log(0.001)/log(1.0d0-dampiron)+1)
      endif

      dampi=dampiron

      if (maxiteriron.eq.-9999) maxiteriron=1

      if(kdumpconv.ne.0) open(newunit=lunconv,file="undumag.cnv")

      if (perlen.eq.9999.0d0.and.newclc.eq.0) then
        c128="perlen"
        call undumag_parameter(c128,perlen,istat)
        if (istat.ne.0) then
          c128="Perlen"
          call undumag_parameter(c128,perlen,istat)
          if (istat.ne.0) then
            c128="PerLen"
            call undumag_parameter(c128,perlen,istat)
          endif
        endif
        if (istat.ne.0) then
          write(lun6,*)
          write(lun6,*)"*** Error in undumag_ini_namelists: Bad return from undumag_parameter for PerLen ***"
          write(lun6,*)"Make sure, that " // trim(Fclc) // " contains parameter or variable PerLen!"
          write(lun6,*)
          stop
        else
          write(lun6,*)
          write(lun6,*)"Period length read from " // trim(Fclc) // ":",sngl(perlen)
          write(lun6,*)
          if (perlen.le.0.0d0) then
            perlen=100.0d0
            write(lun6,*)
            write(lun6,*)"*** Warning in undumag_ini_namelists:Zero or negative period-length found ***"
            write(lun6,*)"*** Set to 100 ***"
            write(lun6,*)
          endif
        endif
      endif

      jrunnum=krunnum
      jcomment=kcomment
      jrunnum=krunnum
      jdate=kdate
      nthreads=nuthreads

      if (dedgefb.eq.0.0d0) dedgefb=0.02d0

      if (iforcedip.ne.0) idipoles=1

      modsimp=modsimphull

      return
      end
