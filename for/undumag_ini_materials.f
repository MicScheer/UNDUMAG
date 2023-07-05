*CMZ :  2.03/00 13/08/2022  14.11.16  by  Michael Scheer
*CMZ :  2.02/02 20/02/2022  16.26.00  by  Michael Scheer
*CMZ :  2.02/01 08/01/2022  16.53.06  by  Michael Scheer
*-- Author :    Michael Scheer   19/10/2021
      subroutine undumag_ini_materials

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision bcm,bcmo,hmat,hmato

      integer :: ib,nmat,imat,lmat,mapmode,nmatpoi,nmatpoimax=0,ieof,lunmat,
     &  kmat,nline

      character(1024) cfile

      if (nclcmat.le.0) return

      read(clcmat(1),*) nmat

      bcmo=-1.0d30
      hmato=-1.0d30

      do ib=1,nmat
        read(clcmat(ib+1),*) imat,lmat,mapmode,cfile
        nmatpoi=0
        open(newunit=lunmat,file=trim(cfile),status='old')
        do while (.true.)
          call util_skip_comment_end(lunmat,ieof)
          if (ieof.ne.0) exit
          read(lunmat,*)hmat,bcm
          if (bcm.ne.bcmo) then
            nmatpoi=nmatpoi+1
          endif
          hmato=hmat
          bcmo=bcm
        enddo
        close(lunmat)
        nmatpoimax=max(nmatpoimax,nmatpoi)
      enddo

      allocate(bcmat(3,nmatpoimax,nmat))
      bcmat=0.0d0

      allocate(feh1(nmatpoimax))
      allocate(fem1(nmatpoimax))

      if (isplinefm.ne.0) then
        allocate(fespl1(nmatpoimax))
        allocate(ufespl1(nmatpoimax))
        allocate(fewspl1(nmatpoimax))
        allocate(fewspl2(nmatpoimax))
        allocate(fewspl3(nmatpoimax))
        allocate(fewspl4(nmatpoimax))
      endif

      write(lun6,*)
      write(lun6,*) "Material files (material number, material type, mode, filename):"
      write(lun6,*)

      do kmat=1,nmat

        read(clcmat(kmat+1),*) imat,lmat,mapmode,cfile

        nmatpoi=0
        bcmo=-1.0d30
        hmato=-1.0d30

        open(newunit=lunmat,file=trim(cfile),status='old')

        write(lun6,*)imat,lmat,mapmode,"      ",trim(cfile)
        nline=0

115     call util_skip_comment_end(lunmat,ieof)

        if (ieof.ne.0) goto 995

        perksi=0.0d0
        ! lmat = 1: Anisotropic magnet material with an easy axis
        !           mapmode=1: Linear material
        !           mapmode=2: Non-linear material

        if (lmat.eq.1) then

          if (mapmode.eq.1) then

            nline=nline+1

            if (nline.gt.1) then
              write(lun6,*)"*** Error in undumag_ini_materials ***"
              write(lun6,*)"*** Material file for mapmode 1 must contain only one data line ***"
              write(lun6,*)"File: ,",trim(cfile)
              stop
            endif

            read(lunmat,*)bcmat(2:3,1,kmat) ! read mu=1.+parksi and perksi
            bcmat(2,1,kmat)=bcmat(2,1,kmat)-1.0d0

          else if (mapmode.eq.2) then
            read(lunmat,*)hmat,bcm,perksi
            bcmat(1,nmatpoi,kmat)=hmat
            bcmat(2,nmatpoi,kmat)=bcm
            bcmat(3,nmatpoi,kmat)=perksi
            bcmo=bcm
            hmato=hmat
          else
            write(lun6,*)"*** Bad material mode found in input file ***"
            stop
          endif
          goto 115
        else if (lmat.eq.2) then
            ! lmat = 2: Isotropic material
            if (mapmode.eq.0
     &          .or.mapmode.eq.2
     &          .or.mapmode.eq.3
     &          .or.mapmode.eq.4
     &          .or.mapmode.eq.5
     &          .or.mapmode.eq.6
     &          .or.mapmode.eq.7
     &          .or.mapmode.eq.8
     &          ) then
              read(lunmat,*)hmat,bcm
            else
              write(lun6,*)"*** Bad material mode found in input file ***"
              stop
            endif
c            if (bcm.ne.bcmo.or.hmato.ne.hmat) then
            if (bcm.ne.bcmo) then
              nmatpoi=nmatpoi+1
            endif
            bcmat(1,nmatpoi,kmat)=hmat
            bcmat(2,nmatpoi,kmat)=bcm
            bcmat(3,nmatpoi,kmat)=perksi
            if (
     &          mapmode.eq.2
     &          .or.mapmode.eq.3
     &          .or.mapmode.eq.4
     &          .or.mapmode.eq.5
     &          .or.mapmode.eq.6
     &          .or.mapmode.eq.7
     &          .or.mapmode.eq.8
     &          ) then
              if (mapmode.eq.3.and.hmat.eq.0.0d0) hmat=1.0d-30
              if (mapmode.eq.3.and.bcm.eq.0.0d0) bcm=1.0d-30
              feh1(nmatpoi)=hmat
              fem1(nmatpoi)=bcm
              bcmo=bcm
              hmato=hmat
            endif
c            endif
            goto 115
          else
            write(lun6,*)"*** Bad material type found in input file ***"
            write(lun6,*)"*** Must be 1 for anisotropic or 2 for isotropic material ***"
            stop
          endif !lmat

995     continue

        matmaps(1,kmat)=imat
        matmaps(2,kmat)=lmat
        matmaps(3,kmat)=mapmode
        matmaps(4,kmat)=nmatpoi

        close(lunmat)

        if (mapmode.gt.1.and.nmatpoi.lt.2) then
          write(lun6,*)"*** Error in undumag_ini_materials ***"
          write(lun6,*)"*** Material file for mapmode > 1 must contain more then one data line ***"
          write(lun6,*)"File: ,",trim(cfile)
          stop
        endif
        if (lmat.eq.2.and.isplinefm.ne.0) call util_spline_coef(feh1,fem1,nmatpoi,
     &    0.0d0,0.0d0,fespl1,fewspl1,fewspl2,fewspl3,fewspl4)

      enddo !nmat

      nmat_t=nmat
      nmatfiles=nmat_t

      return
      end
