*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.01/02 26/04/2018  08.50.33  by  Michael Scheer
*CMZ :  1.23/01 07/08/2017  14.25.23  by  Michael Scheer
*CMZ :  1.22/02 27/07/2017  10.58.16  by  Michael Scheer
*CMZ :  1.19/00 16/06/2017  13.20.59  by  Michael Scheer
*CMZ :  1.11/04 24/01/2017  15.47.17  by  Michael Scheer
*CMZ :  1.11/03 17/01/2017  16.13.31  by  Michael Scheer
*-- Author :    Michael Scheer   16/01/2017
      subroutine undumag_duplicate_mags

      use bpolyederf90m
      use undumagf90m

      use commandlinef90m

      implicit none

! ibpecorn(iplan,imag)=ncorn
! ibpecol(imag) color index

! bpebc(1:3,... position x,y,z
! bpebc(4:6,... M vector, might be changed during relaxation
! bpebc(7,... length of M vector, might be changed during relaxation
! bpebc(8,... type
! bpebc(9,imag)=imat
! bpebc(10,imag) special magnet flag
! bpebc(11:13,imag) normalized M vector at the beginning, will survive
! bpebc(14,imag) length M vector at the beginning, will survive
! bpebc(15,imag) Mother volume

      integer imag,icorn,kcorn,ncorn,iplan,nplan,nfirst,nlast,
     &  mspecmag,nmago,lunlis,ieof

      character(1) c32a(32)
      character(32) c32,chsel
      equivalence (c32a,c32)

      bpebc(17,1:nmag)=0.0d0

      if (noduplis.ne.0) then
        open(newunit=lunlis,file="undumag_no_duplication.lis")
        do while (.true.)
          call util_skip_comment_end(lunlis,ieof)
          if (ieof.ne.0) exit
          read(lunlis,'(a)')chsel
          do imag=1,nmag
            write(chmoth,'(32a)')chmoths(1:32,imag)
            if (chmoth.eq.chsel) then
              bpebc(17,imag)=1
            endif
          enddo
        enddo
        close(lunlis)
      endif !(noduplis.ne.0) then

      nmago=nmag

      mspecmag=0
      do imag=1,nmag
        if (bpebc(10,imag).ne.0) then
          mspecmag=mspecmag+1
        endif
      enddo

      if (ixsym.lt.0) then

        do imag=1,nmag

          bpebc(17,nmag+imag)=bpebc(17,imag)

          bpebc(1:14,nmag+imag)=bpebc(1:14,imag)
          bpebc(15,nmag+imag)=nmag+imag
          chmags(1:32,nmag+imag)=chmags(1:32,imag)
          c32a=chmoths(1:32,imag)
          call util_string_trim(c32,nfirst,nlast)
          if (nlast-nfirst.gt.27) then
            write(lun6,*)
            write(lun6,*)"*** Error in undumag_duplicate_mags: Too long a name:"
            write(lun6,*)c32
            write(lun6,*)
          endif
          c32=c32(nfirst:nlast)//"x"
          chmoths(1:32,nmag+imag)=c32a

          bpebc(10,nmag+imag)=bpebc(10,imag)
          if (bpebc(10,nmag+imag).ne.0) then
            bpebc(10,nmag+imag)=bpebc(10,imag)+mspecmag
          endif

          bpebc(1,nmag+imag)=-bpebc(1,imag)
          bpebc(4,nmag+imag)=-bpebc(4,imag)
          bpebc(11,nmag+imag)=-bpebc(11,imag)

c          if (bpebc(17,imag).ne.0) then
c            bpebc(4:7,nmag+imag)=0.0d0
c          endif

          nplan=ibpeplan(imag)
          do iplan=1,nplan
            ncorn=ibpecorn(iplan,imag)
            do icorn=1,ncorn
              kcorn=ncorn-icorn+1
              bpemag(1,kcorn,iplan,nmag+imag)=
     &          -bpemag(1,icorn,iplan,imag)
              bpemag(2:3,kcorn,iplan,nmag+imag)=
     &          bpemag(2:3,icorn,iplan,imag)
            enddo
          enddo

          ibpeplan(nmag+imag)=ibpeplan(imag)
          ibpecol(nmag+imag)=ibpecol(imag)
          ibpecorn(1:nplanmax,nmag+imag)=ibpecorn(1:nplanmax,imag)

        enddo !nmag

        nmag=2*nmag
        mspecmag=2*mspecmag

      endif

      if (iysym.lt.0) then

        do imag=1,nmag

          bpebc(17,nmag+imag)=bpebc(17,imag)

          bpebc(1:14,nmag+imag)=bpebc(1:14,imag)
          bpebc(15,nmag+imag)=nmag+imag
          chmags(1:32,nmag+imag)=chmags(1:32,imag)
          c32a=chmoths(1:32,imag)
          call util_string_trim(c32,nfirst,nlast)
          if (nlast-nfirst.gt.27) then
            write(lun6,*)
            write(lun6,*)"*** Error in undumag_duplicate_mags: Too long a name:"
            write(lun6,*)c32
            write(lun6,*)
          endif
          c32=c32(nfirst:nlast)//"y"
          chmoths(1:32,nmag+imag)=c32a

          bpebc(10,nmag+imag)=bpebc(10,imag)
          if (bpebc(10,nmag+imag).ne.0) then
            bpebc(10,nmag+imag)=bpebc(10,imag)+mspecmag
          endif

          bpebc(2,nmag+imag)=-bpebc(2,imag)
          bpebc(4,nmag+imag)=-bpebc(4,imag)
          bpebc(6,nmag+imag)=-bpebc(6,imag)
          bpebc(12,nmag+imag)=-bpebc(12,imag)

c          if (bpebc(17,imag).ne.0) then
c            bpebc(4:7,nmag+imag)=0.0d0
c          endif

          nplan=ibpeplan(imag)
          do iplan=1,nplan
            ncorn=ibpecorn(iplan,imag)
            do icorn=1,ncorn
              kcorn=ncorn-icorn+1
              bpemag(1,kcorn,iplan,nmag+imag)=
     &          bpemag(1,icorn,iplan,imag)
              bpemag(2,kcorn,iplan,nmag+imag)=
     &          -bpemag(2,icorn,iplan,imag)
              bpemag(3,kcorn,iplan,nmag+imag)=
     &          bpemag(3,icorn,iplan,imag)
            enddo
          enddo

          ibpeplan(nmag+imag)=ibpeplan(imag)
          ibpecol(nmag+imag)=ibpecol(imag)
          ibpecorn(1:nplanmax,nmag+imag)=ibpecorn(1:nplanmax,imag)

        enddo !nmag

        nmag=2*nmag
        mspecmag=2*mspecmag

      endif

      if (izsym.lt.0) then

        do imag=1,nmag

          bpebc(17,nmag+imag)=bpebc(17,imag)

          bpebc(1:14,nmag+imag)=bpebc(1:14,imag)

          bpebc(10,nmag+imag)=bpebc(10,imag)
          if (bpebc(10,nmag+imag).ne.0) then
            bpebc(10,nmag+imag)=bpebc(10,imag)+mspecmag
          endif

          bpebc(3,nmag+imag)=-bpebc(3,imag)
          bpebc(6,nmag+imag)=-bpebc(6,imag)
          bpebc(13,nmag+imag)=-bpebc(13,imag)
          bpebc(15,nmag+imag)=nmag+imag
c          if (bpebc(17,imag).ne.0) then
c            bpebc(4:7,nmag+imag)=0.0d0
c          endif

          chmags(1:32,nmag+imag)=chmags(1:32,imag)
          c32a=chmoths(1:32,imag)
          call util_string_trim(c32,nfirst,nlast)
          if (nlast-nfirst.gt.27) then
            write(lun6,*)
            write(lun6,*)"*** Error in undumag_duplicate_mags: Too long a name:"
            write(lun6,*)c32
            write(lun6,*)
          endif
          c32=c32(nfirst:nlast)//"z"
          chmoths(1:32,nmag+imag)=c32a

          nplan=ibpeplan(imag)
          do iplan=1,nplan
            ncorn=ibpecorn(iplan,imag)
            do icorn=1,ncorn
              kcorn=ncorn-icorn+1
              bpemag(1:2,kcorn,iplan,nmag+imag)=
     &          bpemag(1:2,icorn,iplan,imag)
              bpemag(3,kcorn,iplan,nmag+imag)=
     &          -bpemag(3,icorn,iplan,imag)
            enddo
          enddo

          ibpeplan(nmag+imag)=ibpeplan(imag)
          ibpecol(nmag+imag)=ibpecol(imag)
          ibpecorn(1:nplanmax,nmag+imag)=ibpecorn(1:nplanmax,imag)

        enddo !nmag

        nmag=2*nmag

      endif !izsym

      do imag=nmago+1,nmag
        if (bpebc(17,imag).ne.0.0d0) then
          bpebc(4:7,imag)=0.0d0
        endif
      enddo

      do imag=1,nmago
        if (bpebc(17,imag).ne.0.0d0) then
          bpebc(17,imag)=-9999.0d0
        endif
      enddo

      return
      end
