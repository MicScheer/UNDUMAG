*CMZ :  2.02/01 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.25/00 08/03/2018  14.12.29  by  Michael Scheer
*CMZ :  1.22/00 04/07/2017  10.31.41  by  Michael Scheer
*CMZ :  1.11/01 09/01/2017  13.37.40  by  Michael Scheer
*-- Author :    Michael Scheer   06/01/2017
      subroutine undumag_parameter(cparin,val,istat)

      use bpolyederf90m
      use undumagf90m

      use commandlinef90m

      implicit none

      double precision val
      integer luni,last,istat,nbuff,ipos(2,3),nwords,ifound,i
      character(128) cline,cvar,cpar,cparin
      character(32) cw1,cw2,cw3
      integer ic1
      character c1
      equivalence (ic1,c1)

      istat=-1
      ic1=0
      cpar=cparin

      open(newunit=luni,file='undumag.in')

      ifound=0

      do while (.true.)
        read(luni,'(a)',end=9)cline
        if (cline(1:25).eq.'* Results of undumag_calc') then
          ifound=1
          exit
        endif
      enddo
9     continue

      if (ifound.eq.0) then
c        write(lun6,*)
c        write(lun6,*)"*** Warning in undumag_parameter: No results of undumag_calc found ***"
c        write(lun6,*)
        goto 9999
      endif

      ic1=0
      do i=1,len_trim(cpar)
        c1=cpar(i:i)
        if (ic1.ge.65.and.ic1.le.90) ic1=ic1+32
        cpar(i:i)=c1
      enddo

      do while (.true.)
        read(luni,'(a)',end=9999)cline
        if (cline(1:8).eq.'*EndCalc') exit

        ifound=0
        do i=1,128
          if (cline(i:i).eq.':') then
            ifound=i
            exit
          endif
        enddo

        if (ifound.ne.0) then
          cvar=cline(2:ifound-1)
          read(cline(ifound+1:128),*)val
        endif

        ic1=0
        do i=1,len_trim(cvar)
          c1=cvar(i:i)
          if (ic1.ge.65.and.ic1.le.90) ic1=ic1+32
          cvar(i:i)=c1
        enddo

        if (cvar.eq.cpar) then
          istat=0
          exit
        endif

      enddo

9999  close(luni)

      return
      end
