*CMZ :  2.04/01 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/00 14/01/2023  14.39.42  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.33.07  by  Michael Scheer
*CMZ :  2.02/01 19/01/2022  10.38.36  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_inhom

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision coef
      double precision undumag_variable_getval

      integer imag,inh,l,nwords,ipos(2,10),istat,iend,nc,i1,i2,i3,mc,k,ibr,ixyz

      character(512) cnam,cmoth,cline
      character(32) cbr,cxyz

      do imag=1,nmag_t+nspecmag_t
        cmoth=t_magnets(imag)%cmoth
        cnam=t_magnets(imag)%cnam
        t_magnets(imag)%IsInhom=0
        do inh=1,ninhom_t
          cline=clcinhom(inh)
          call util_string_split(cline,10,nwords,ipos,istat)
          if (nwords.lt.1) cycle
          if (cline(ipos(1,1):ipos(2,1)).eq.cmoth) then
            nmaginhom_t=nmaginhom_t+1
            t_magnets(imag)%IsInhom=inh
            t_magnets(imag)%xyzinh(1)=
     &        undumag_variable_getval(cline(ipos(1,2):ipos(2,2)))
            t_magnets(imag)%xyzinh(2)=
     &        undumag_variable_getval(cline(ipos(1,3):ipos(2,3)))
            t_magnets(imag)%xyzinh(3)=
     &        undumag_variable_getval(cline(ipos(1,4):ipos(2,4)))

            cbr=cline(ipos(1,5):ipos(2,5))
            call util_lower_case(cbr)
            if (cbr(1:3).eq.'nor') then
              t_magnets(imag)%xyzinh(4)=1.0d0
            else
              t_magnets(imag)%xyzinh(4)=0.0d0
            endif
          endif
        enddo
      enddo !nmag_t

      !call util_break
      allocate(maginhom_t(nmaginhom_t))
      nmaginhom_t=0

      do imag=1,nmag_t+nspecmag_t

        cmoth=t_magnets(imag)%cmoth
        cnam=t_magnets(imag)%cnam
        inh=t_magnets(imag)%IsInhom

        if (inh.ne.0) then

          mc=0
          nc=0
          iend=0

          do l=inh+1,ninhom_t
            if (clcinhom(l)(1:5).eq.'! End') then
              iend=l
            endif
          enddo

          if (iend.eq.0) then
            print*,"*** Error in clcmag_inhom: End marker not found for inhomogeneity for magnet: ",
     &        trim(cnam),trim(cmoth)
            stop "*** Aborted ***"
          endif

          nc=iend-inh-1
          t_magnets(imag)%IsInhom=nc

          nmaginhom_t=nmaginhom_t+1
          maginhom_t(nmaginhom_t)=imag

          allocate(t_magnets(imag)%CInhom(nc))

          k=inh
          do l=1,nc
            k=k+1
            t_magnets(imag)%cinhom(l)=clcinhom(k)
          enddo

        endif

      enddo

      return
      end
