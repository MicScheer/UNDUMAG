*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.25/00 08/03/2018  14.12.29  by  Michael Scheer
*CMZ :  1.22/00 04/07/2017  09.10.00  by  Michael Scheer
*CMZ :  1.11/01 09/01/2017  13.37.40  by  Michael Scheer
*-- Author :    Michael Scheer   06/01/2017
      subroutine undumag_force_parameters

      use bpolyederf90m
      use undumagf90m

      use commandlinef90m

      implicit none

      double precision val
      integer luni,last,istat,nbuff,ipos(2,3),nwords,ifound,i
      character(128) cline,cvar
      character(32) cw1,cw2,cw3
      character c1
      integer ic1
      equivalence (ic1,c1)

      open(newunit=luni,file='undumag.in')

      ifound=0
      ic1=0

      do while (.true.)
        read(luni,'(a)',end=9)cline
c        write(lun6,*)cline
        if (cline(1:25).eq.'* Results of undumag_calc') then
          ifound=1
          exit
        endif
      enddo
9     continue

      if (ifound.eq.0) then
        write(lun6,*)
        write(lun6,*)"*** Error in undumag_force_parameters: No results of undumag_calc found ***"
        write(lun6,*)
        stop
      endif

      do while (.true.)
        read(luni,'(a)')cline
        if (cline(1:8).eq.'*EndCalc') exit

        do i=1,128
          if (cline(i:i).eq.':') then
            ifound=i
            exit
          endif
        enddo

        if (ifound.ne.0) then
          cvar=cline(2:ifound-1)
          read(cline(ifound+1:128),*)val
c          write(lun6,*)cvar,val
        endif

        do i=1,len_trim(cvar)
          c1=cvar(i:i)
          if (ic1.ge.65.and.ic1.le.90) ic1=ic1+32
          cvar(i:i)=c1
        enddo

        if (cvar.eq.'iplforce') iplforce=val

        if (cvar.eq.'ubflenx') ubflenx=val
        if (cvar.eq.'ubfleny') ubfleny=val
        if (cvar.eq.'ubflenz') ubflenz=val

        if (cvar.eq.'ubfcenx') ubfcenx=val
        if (cvar.eq.'ubfceny') ubfceny=val
        if (cvar.eq.'ubfcenz') ubfcenz=val

        if (cvar.eq.'utorqcenx') utorqcenx=val
        if (cvar.eq.'utorqceny') utorqceny=val
        if (cvar.eq.'utorqcenz') utorqcenz=val

        if (cvar.eq.'mbforcex') mbforcex=val
        if (cvar.eq.'mbforcey') mbforcey=val
        if (cvar.eq.'mbforcez') mbforcez=val

        if (cvar.eq.'mfcolor') mfcolor=val
      enddo

      close(luni)

      return
      end
