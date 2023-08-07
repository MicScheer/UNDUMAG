*CMZ :  2.04/06 02/08/2023  09.04.03  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/01 22/01/2023  13.04.45  by  Michael Scheer
*CMZ :  2.04/00 14/01/2023  14.39.42  by  Michael Scheer
*CMZ :  2.03/00 15/08/2022  12.39.08  by  Michael Scheer
*CMZ :  2.02/02 09/03/2022  09.42.30  by  Michael Scheer
*CMZ :  2.02/01 11/02/2022  08.28.50  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine undumag_read_clc

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      integer i,j,ieof,lunclc,istat,ipos(2,1000),nwords,
     &  lmat,nmat,l,ncorn,nfila,lunf

      character(512) cline,cword
      cundutit='* No User Comment'

      nclcbuff=0
      ncwires=0

      nwind=0
      nrace=0
      ncrace=0
      narc=0
      ncarc=0
      nrbar=0
      nthwir=0
      nmagcyl=0

      ntransrotcop=0
      transrotcop=0.0d0

      nmodule_t=0
      modegui=0

      open(newunit=lunclc,file=Fclc)

      do while (.true.)
        call util_skip_comment_end(lunclc,ieof)
        if (ieof.ne.0) exit
        read(lunclc,'(a)') cline
        !if (kechocalc.ne.0) print*,trim(cline)
        cline=adjustl(cline)
        if (cline(1:4).eq.'Mode') then
          call util_string_split(cline,1000,nwords,ipos,istat)
          if (nwords.gt.1) then
            if (cline(ipos(1,2):ipos(2,2)).eq.'Mirror') then
              modegui=3
            else if (cline(ipos(1,2):ipos(2,2)).eq.'Hybrid') then
              modegui=2
            else if (cline(ipos(1,2):ipos(2,2)).eq.'AppleII') then
              modegui=1
            endif
          endif
        endif
        if (cline(1:1).eq.'&') then
          call util_string_split(cline,1000,nwords,ipos,istat)
          if (cline(ipos(1,2):ipos(2,2)).eq.'User_Comment') then
            nclcbuff=nclcbuff+1
            !if (kechocalc.ne.0) print*,trim(cline)
            call util_skip_comment_end(lunclc,ieof)
            if (ieof.ne.0) exit
            cline=''
            unducomment=''
            read(lunclc,'(a)') cline
            nclcbuff=nclcbuff+1
            unducomment=trim(cline(1:512))
            usercom=adjustl(trim(unducomment))
            cundutit='* ' // adjustl(trim(unducomment))
            read(lunclc,'(a)') cline
            cycle ! to avoid confusion, if key words appear in comment
          endif
        endif
        if (len_trim(cline).eq.0) cycle
        nclcbuff=nclcbuff+1
      enddo

      allocate(
     &  magmodule(nclcbuff),
     &  clcbuff(nclcbuff),
     &  clcmag(nclcbuff),
     &  clcspec(nclcbuff),
     &  clcmat(nclcbuff),
     &  clcmod(nclcbuff),
     &  clccoil(nclcbuff),
     &  clccop(nclcbuff),
     &  clcvar(nclcbuff),
     &  clcinhom(nclcbuff)
     &  )

      rewind(lunclc)

      nclcbuff=0
      do while (.true.)
        ieof=0
        call util_skip_comment_end(lunclc,ieof)
        if (ieof.ne.0) exit
        read(lunclc,'(a)') cline
        cline=adjustl(cline)
        !print*,trim(cline)
        if (len_trim(cline).eq.0) cycle
        if (nclcbuff.gt.0.and.cline(1:1).eq.'&') then
          call util_string_split(cline,1000,nwords,ipos,istat)
          if (cline(ipos(1,2):ipos(2,2)).eq.'End') then
            nclcbuff=nclcbuff+1
            cline(1:1)='!'
            clcbuff(nclcbuff)=cline
            cycle
          endif
          if (clcbuff(nclcbuff)(1:1).eq.'&') then
            !print*,cline(ipos(1,2):ipos(2,2))
            !call util_break
            print*,"*** Error in undumag_read_clc: Missing data lines after"
            print*,trim(clcbuff(nclcbuff))
            stop
          endif
          if (cline(ipos(1,2):ipos(2,2)).eq.'User_Comment') then
            ieof=0
            call util_skip_comment_end(lunclc,ieof)
            if (ieof.ne.0) exit
            read(lunclc,'(a)') cline
            cycle
          endif
        endif
        nclcbuff=nclcbuff+1
        clcbuff(nclcbuff)=cline
      enddo

      close(lunclc)

      nclcmag=0
      nclcspec=0
      nclccoil=0
      nclcvar=0
      nclcmod=0
      nclcmat=0

      nmag_t=0
      niron_t=0
      ninhom_t=0

      i=0
      do while (i.lt.nclcbuff)

        i=i+1
        cline=clcbuff(i)
        if (kechocalc.ne.0) print*,trim(cline)
        call util_string_split(cline,1000,nwords,ipos,istat)

        if (cline(ipos(1,1):ipos(2,1)).eq.'&') then
          if (cline(ipos(1,2):ipos(2,2)).eq.'User_Comment') then
            i=i+1
          else if (cline(ipos(1,2):ipos(2,2)).eq.'Inhomogeneity') then
            !call util_break
            ninhom_t=ninhom_t+1
            clcinhom(ninhom_t)=cline(ipos(1,2):ipos(2,2))
            i=i+1
            do while (.true.)
              !print*,i,trim(clcbuff(i))
              ninhom_t=ninhom_t+1
              clcinhom(ninhom_t)=clcbuff(i)
              if (kechocalc.ne.0) print*,trim(clcbuff(i))
              call util_string_split(clcinhom(ninhom_t),1000,nwords,ipos,istat)
              if (clcinhom(ninhom_t)(ipos(1,2):ipos(2,2)).eq.'End') exit
              i=i+1
            enddo
            !call util_break
          else if (cline(ipos(1,2):ipos(2,2)).eq.'Pole' .or.
     &        cline(ipos(1,2):ipos(2,2)).eq.'Magnet') then
            nmag_t=nmag_t+1
            magmodule(nmag_t)=nmodule_t+1
            if (cline(ipos(1,2):ipos(2,2)).eq.'Pole') niron_t=niron_t+1
            nclcmag=nclcmag+1
            clcmag(nclcmag)=cline(ipos(1,2):ipos(2,2))
            i=i+1
            cline=clcbuff(i)
            if (kechocalc.ne.0) print*,trim(cline)
            call util_string_split(cline,1000,nwords,ipos,istat)
            if (
     &          cline(ipos(1,1):ipos(2,1)).eq.'BlockChamf'.or.
     &          cline(ipos(1,1):ipos(2,1)).eq.'BlockUsChamf'.or.
     &          cline(ipos(1,1):ipos(2,1)).eq.'BlockDsChamf'
     &          ) then
              do l=1,5
                nclcmag=nclcmag+1
                clcmag(nclcmag)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'Block') then
              do l=1,5
                nclcmag=nclcmag+1
                clcmag(nclcmag)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'Cylinder') then
              nmagcyl=nmagcyl+1
              do l=1,5
                nclcmag=nclcmag+1
                clcmag(nclcmag)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'Corners') then
              read(clcbuff(i+4),*) ncorn
              do l=1,5+ncorn
                nclcmag=nclcmag+1
                clcmag(nclcmag)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'File') then
              do l=1,5
                nclcmag=nclcmag+1
                clcmag(nclcmag)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else
              print*,"*** Error in undumag_read_clc: Unknown magnet type in line"
              print*,trim(cline)
              stop
            endif
          else if (cline(ipos(1,2):ipos(2,2)).eq.'Module') then
            nmodule_t=nmodule_t+1
            nclcmod=nclcmod+1
            clcmod(nclcmod)=clcbuff(i)
            do l=1,4
              i=i+1
              nclcmod=nclcmod+1
              clcmod(nclcmod)=clcbuff(i)
              if (kechocalc.ne.0) print*,trim(clcbuff(i))
            enddo
          else if (cline(ipos(1,2):ipos(2,2)).eq.'Translate') then
            ntransrotcop=ntransrotcop+1
            if (ntransrotcop.gt.ntransrotcop_p) then
              stop "*** Error in undumag_read_clc: Too many remanence, translations, rotations changes, check parameter ntransrotcop_p ***"
            endif
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            ctransrotcop(ntransrotcop)=trim(clcbuff(i))
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            !read(clcbuff(i),*) transrotcop(1:3,ntransrotcop)
            transrotcop(1,ntransrotcop)=dble(i)
            transrotcop(8,ntransrotcop)=0.0d0
          else if (cline(ipos(1,2):ipos(2,2)).eq.'Remanence') then
            ntransrotcop=ntransrotcop+1
            if (ntransrotcop.gt.ntransrotcop_p) then
              stop "*** Error in undumag_read_clc: Too many remanence, translations, rotations changes, check parameter ntransrotcop_p ***"
            endif
            transrotcop(8,ntransrotcop)=3.0d0
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            ctransrotcop(ntransrotcop)=trim(clcbuff(i))
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            transrotcop(1,ntransrotcop)=dble(i)
            !read(clcbuff(i),*) transrotcop(1:6,ntransrotcop)
          else if (cline(ipos(1,2):ipos(2,2)).eq.'Copy') then
            nclccop_t=nclccop_t+1
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            clccop(nclccop_t)=trim(clcbuff(i))
            ntransrotcop=ntransrotcop+1
            if (ntransrotcop.gt.ntransrotcop_p) then
              stop "*** Error in undumag_read_clc: Too many remanence, translations, rotations changes, check parameter ntransrotcop_p ***"
            endif
            transrotcop(8,ntransrotcop)=-dble(nclccop_t)
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            cline=clcbuff(i)
            call util_string_split(cline,1000,nwords,ipos,istat)
            cword=cline(ipos(1,1):ipos(2,1))
            ctransrotcop(ntransrotcop)=trim(cword)
          else if (
     &        cline(ipos(1,2):ipos(2,2)).eq.'Rotate'.or.
     &        cline(ipos(1,2):ipos(2,2)).eq.'Rotate_Shape') then
            ntransrotcop=ntransrotcop+1
            if (cline(ipos(1,2):ipos(2,2)).eq.'Rotate_Shape') then
              transrotcop(8,ntransrotcop)=1.0d0
            else
              transrotcop(8,ntransrotcop)=2.0d0
            endif
            if (ntransrotcop.gt.ntransrotcop_p) then
              stop "*** Error in undumag_read_clc: Too many remanence, translations, rotations changes, check parameter ntransrotcop_p ***"
            endif
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            ctransrotcop(ntransrotcop)=trim(clcbuff(i))
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            transrotcop(1,ntransrotcop)=dble(i)
            !read(clcbuff(i),*) transrotcop(1:3,ntransrotcop)
            i=i+1
            if (kechocalc.ne.0) print*,trim(clcbuff(i))
            !read(clcbuff(i),*) transrotcop(4:7,ntransrotcop)
          else if (
     &        cline(ipos(1,2):ipos(2,2)).eq.'Special_Magnet'.or.
     &        cline(ipos(1,2):ipos(2,2)).eq.'Special_Pole'
     &        ) then
            nclcspec=nclcspec+1
            clcspec(nclcspec)=cline(ipos(1,2):ipos(2,2))
            nspecmag_t=nspecmag_t+1
            if (cline(ipos(1,2):ipos(2,2)).eq.'Special_Pole') niron_t=niron_t+1
            i=i+1
            cline=clcbuff(i)
            call util_string_split(cline,1000,nwords,ipos,istat)
            if (
     &          cline(ipos(1,1):ipos(2,1)).eq.'BlockChamf'.or.
     &          cline(ipos(1,1):ipos(2,1)).eq.'BlockUsChamf'.or.
     &          cline(ipos(1,1):ipos(2,1)).eq.'BlockDsChamf'
     &          ) then
              do l=1,5
                nclcspec=nclcspec+1
                clcspec(nclcspec)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'Block') then
              do l=1,5
                nclcspec=nclcspec+1
                clcspec(nclcspec)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'Cylinder') then
              do l=1,4
                nclcspec=nclcspec+1
                clcspec(nclcspec)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'Corners') then
              read(clcbuff(i+4),*) ncorn
              do l=1,5+ncorn
                nclcspec=nclcspec+1
                clcspec(nclcspec)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else if (cline(ipos(1,1):ipos(2,1)).eq.'File') then
              do l=1,5
                nclcspec=nclcspec+1
                clcspec(nclcspec)=clcbuff(i)
                if (kechocalc.ne.0) print*,trim(clcbuff(i))
                i=i+1
              enddo
              i=i-1
            else
              print*,"*** Error in undumag_read_clc: Unknown magnet type in line"
              print*,trim(cline)
              stop
            endif !shape

          else if (cline(ipos(1,2):ipos(2,2)).eq.'Materials') then
            i=i+1
            cline=clcbuff(i)
            nclcmat=nclcmat+1
            clcmat(nclcmat)=clcbuff(i)
            if (kechocalc.ne.0) print*,trim(cline)
            read(cline,*) nmat
            do lmat=1,nmat
              nclcmat=nclcmat+1
              i=i+1
              clcmat(nclcmat)=clcbuff(i)
              if (kechocalc.ne.0) print*,trim(clcbuff(i))
            enddo !nmat

          else if (cline(ipos(1,2):ipos(2,2)).eq.'Coil') then

            ncoils_t=ncoils_t+1
            nclccoil=nclccoil+1
            clccoil(nclccoil)='Coil'
            i=i+1
            cline=clcbuff(i)
            if (kechocalc.ne.0) print*,trim(cline)

            call util_string_split(cline,1000,nwords,ipos,istat)
            nclccoil=nclccoil+1

            if (nwords.gt.1) then
              clccoil(nclccoil)=adjustl(trim(cline(ipos(1,1):ipos(2,2))))
            else
              clccoil(nclccoil)=adjustl(trim(cline(ipos(1,1):ipos(2,1))))
            endif

            cword=adjustl(trim(cline(ipos(1,1):ipos(2,1))))

            i=i+1
            nclccoil=nclccoil+1
            clccoil(nclccoil)=clcbuff(i)
            cline=clcbuff(i)
            if (kechocalc.ne.0) print*,trim(cline)

            if (cword.eq.'Filaments') then
              read(cline,*)nfila
              do l=1,nfila
                nclccoil=nclccoil+1
                i=i+1
                clccoil(nclccoil)=clcbuff(i)
              enddo
              ncwires=ncwires+nfila

            else if (cword.eq.'File') then

              call util_string_split(cline,1000,nwords,ipos,istat)
              cword=adjustl(trim(cline(ipos(1,1):ipos(2,1))))
              call util_unquote(cword)
              clccoil(nclccoil)=adjustl(trim(cword))
              open(newunit=lunf,file=trim(cword),status='old')
              nfila=0
              do while (.true.)
                call util_skip_comment_empty_end(lunf,ieof)
                if (ieof.ne.0) exit
                read(lunf,'(a)')cline
                nfila=nfila+1
              enddo
              close(lunf)
              ncwires=ncwires+nfila

            else if (
     &          cword.eq.'RectArc'.or.
     &          cword.eq.'CircArc'.or.
     &          cword.eq.'RectBar'.or.
     &          cword.eq.'ThickWire'
     &          ) then
              i=i+1
              nclccoil=nclccoil+1
              clccoil(nclccoil)=clcbuff(i)

            else if (
     &          cword.ne.'Rectangular'.and.
     &          cword.ne.'RectangCirc'.and.
     &          cword.ne.'RectWindings'
     &          ) then
              print*,""
              print*,"*** Error in undumag_read_clc: Unknown coil type in line"
              print*,trim(cline)
              stop
            endif

          else
            print*,""
            print*,"*** Error in undumag_read_clc: Unknown key-word in line"
            print*,trim(cline)
            stop
         endif ! &

        else if (cline(1:1).eq.'$') then

          !if (kechocalc.ne.0) print*,trim(cline)
          do j=2,len_trim(cline)-1
            if (cline(j:j).eq.'=') then
              nclcvar=nclcvar+1
              clcvar(nclcvar)=trim(cline(2:))
              exit
            endif
          enddo
        endif !c1
      enddo

      return
      end
