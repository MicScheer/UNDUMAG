*CMZ :  2.04/03 03/03/2023  11.23.27  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  14.26.12  by  Michael Scheer
*CMZ :  2.02/02 03/07/2022  15.34.20  by  Michael Scheer
*CMZ :  2.02/01 10/11/2021  10.13.49  by  Michael Scheer
*CMZ :  2.02/00 30/03/2021  15.24.45  by  Michael Scheer
*-- Author :    Michael Scheer   10/09/2020
      subroutine undumag_drop_zero_magnets(kfoundplanes,kecho,tiny)

      use commandlinef90m
      use commandlinef90m

      implicit none

      ! Drop zero magnetig items and apply coating to permanent magnets,
      ! i.e. shrinking of the magnet to the effective size

c loop over magnets, modules, and arrays {

      double precision coating,bc,xm,ym,zm,rmin,rmax,polhei,ang,r,dx,dy,dz,
     &  xbuff(1000),ybuff(1000),zbuff(1000),cen(3),chamf,tiny

      integer lunbpe, lunscr,ieof,nlines,iline,imat,istat,iend,ianf,i,npoints,
     &  ndl,itreat,nmodules,narrays,ipos(2,1000),ncom,nwords,nfirst,nlast,icomment,
     &  l,nbuff,materials(2,1000),nmat,n9999,kmat,lundat,nread,kcorn,
     &  kfoundplanes,kblock,kblockch,kecho,icomread,iplan,nplan,icorn,ncorn,ios

      integer nshrink
      character(2) cshrink

      integer :: nmag=0

      character(512) cline,clinelower,cline1,cline2,cline3,cdum,cbuff(1000),
     &  cplan,comdum
      character(64) c64,chmag,chmoth

*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.


      coating=0.0d0
      open(newunit=lunbpe,file=trim(Fclc))

      ieof=0

      cline=''
      cline1=''
      cline2=''
      cline3=''

      do while (ieof.eq.0)

        read(lunbpe,'(a)')cline
        cline=trim(adjustl(cline))
c        c64='*EndCalc'
        if (cline(1:8).eq.'*EndCalc') exit
c        call util_string_substring(cline,trim(c64),ianf,iend,istat)
c        if (istat.eq.0) exit

        c64='mcoating'
        clinelower=cline
        call util_lower_case(clinelower)
        call util_string_substring(clinelower,trim(c64),ianf,iend,istat)
c        write(lun6,*)trim(cline),istat
        if (istat.eq.0) then
          l=len_trim(cline)
          do i=2,l
            if (cline(i:i).eq.'=') exit
          enddo
          read(cline(i+1:l),*) coating
          if (coating.ne.0.0d0.and.kfoundplanes.ne.0) then
            write(lun6,*)" "
            write(lun6,*)"*** Warning: Coating option is not available for magnetic items defined by planes ***"
            write(lun6,*)" "
            call sleep(3)
          endif
          exit
        endif
      enddo

      close(lunbpe)


      open(newunit=lunbpe,file="undumag.in")
      open(newunit=lunscr,file='undumag.scr')

      nlines=0
      ieof=0
      n9999=0
      icomment=0
      imat=0

      do while (ieof.eq.0)

        read(lunbpe,'(a)',end=9,err=9)cline
        cline=trim(adjustl(cline))
        nlines=nlines+1
        write(lunscr,'(a)')trim(cline)

        if (cline3(1:1).eq.'{') then
          icomment=1
          cycle
        endif

        if (cline(1:1).eq.'*'.or.cline(1:1).eq.'!'.or.icomment.eq.1) cycle

        if (cline(1:1).eq.'}') then
          icomment=0
          cycle
        endif

        c64='File'
        call util_string_substring(cline,trim(c64),ianf,iend,istat)
        if (istat.eq.0) then
          read(lunbpe,'(a)',end=9,err=9)cline
          cline=trim(adjustl(cline))
          nread=0
          open(newunit=lundat,file=trim(cline),status='old')
          do while (istat.eq.0)
            call util_skip_comment_end(lundat,istat)
            if (istat.ne.0) then
              close(lundat)
              exit
            else
              nread=nread+1
              read(lundat,'(a)')cbuff(nread)
            endif
          enddo
          write(cline,*) nread
          call util_string_trim(cline,nfirst,nlast)
          write(lunscr,'(a)')cline(nfirst:nlast)
          nlines=nlines+1
          do i=1,nread
            nlines=nlines+1
            write(lunscr,'(a)')trim(cbuff(i))
          enddo
          cycle
        endif

        c64='-9999.'
        call util_string_substring(cline,trim(c64),ianf,iend,istat)
        if (istat.eq.0) then
          n9999=n9999+1
          cycle
        endif

        if (n9999.eq.2) then
          read(cline,*) nmat
          n9999=3
          cycle
        endif

        if (n9999.eq.3.and.imat.lt.nmat) then
          imat=imat+1
          read(cline,*)materials(1:2,imat)
        endif

      enddo

9     continue
      flush(lunscr)
      close(lunscr)

      open(newunit=lunscr,file='undumag.scr')
      close(lunbpe)

      open(newunit=lunbpe,file="undumag.in")

      itreat=0
      icomment=0
      icomread=-1

      do iline=1,nlines

        if (icomread.eq.0) icomread=1

        if (
     &    cline2(1:1).ne.'*'.and.cline2(1:1).ne.'!'.and.
     &    cline2(1:1).ne.'{'.and.cline2(1:1).ne.'}'
     &    ) cline1=cline2

        if (
     &    cline3(1:1).ne.'*'.and.cline3(1:1).ne.'!'.and.
     &    cline3(1:1).ne.'{'.and.cline3(1:1).ne.'}'
     &    ) cline2=cline3

        read(lunscr,'(a)',end=9999)cline3
        cline3=trim(adjustl(cline3))

        if (kecho.ne.0) print*,trim(cline3)

c        write(lun6,*)iline
c        write(lun6,*)trim(cline3)

        c64='*EndCalc'
        call util_string_substring(cline3,trim(c64),ianf,iend,istat)

        if (istat.eq.0) then
          itreat=1 !EndCalc marker found
          write(lunbpe,'(a)')trim(cline3)
          do while (cline3(1:1).eq.'*'.or.cline3(1:1).eq.'!')
            read(lunscr,'(a)')cline3
            cline3=trim(adjustl(cline3))
            write(lunbpe,'(a)')trim(cline3)
            icomread=0
          enddo
c          cycle
        endif

        if (cline3(1:1).eq.'{') icomment=1

        if (cline3(1:1).eq.'*'.or.cline3(1:1).eq.'!'.or.icomment.eq.1) then
          write(lunbpe,'(a)')trim(cline3)
          if (cline3(1:1).eq.'}') icomment=0
          cycle
        endif

        cdum=''

        if (itreat.eq.1.or.itreat.eq.6) then
          c64='-9999.'
          call util_string_substring(cline3,trim(c64),ianf,iend,istat)
          if (icomread.eq.1.and.istat.eq.0) then
            read(cline3,*,err=99)xm,ym,zm
            if (xm.eq.-9999.0d0.and.ym.eq.-9999.0d0.and.zm.eq.-9999.0d0) then
              itreat=itreat+1
              write(lunbpe,'(a)')trim(cline3)
              cycle
            endif
          endif !istat
99        continue
        else if (itreat.eq.2) then !read number of module blocks
          read(cline3,*)nmodules
          write(lunbpe,'(a)')trim(cline3)
          itreat=3
          cycle
        else if (itreat.eq.3) then !read module blocks
          backspace(lunscr)
          ndl=1
          do while (ndl.le.nmodules*4)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).ne.'*'.and.cline(1:1).ne.'!') then
              ndl=ndl+1
            endif
            write(lunbpe,'(a)') trim(cline)
          enddo
          itreat=4
          cycle
        else if (itreat.eq.4) then !read number of arrays
          read(cline3,*)narrays
          itreat=5
          write(lunbpe,'(a)')trim(cline3)
          cycle
        else if (itreat.eq.5) then !read arrays
          backspace(lunscr)
          ndl=1
          do while (ndl.le.2)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).ne.'*'.and.cline(1:1).ne.'!') then
              ndl=ndl+1
            endif
            write(lunbpe,'(a)') trim(cline)
          enddo
          itreat=6
          cycle
        endif !itreat


        !Old format with planes?
        if (icomread.eq.1.and.(itreat.eq.1.or.itreat.eq.6)) then
          read(cline3,*,iostat=ios) nplan
          if (ios.eq.0.and.nplan.gt.0) then
            read(cline2,*) bc
            if (bc.ne.0.0d0) then
              write(lunbpe,'(a)')'*'
              nmag=nmag+1
              call util_string_split_sep(cline1,1000,ncom,ipos,'!',istat)
              if (ncom.gt.1) then
                comdum=cline1(ipos(1,2):ipos(2,2))
              endif
              cdum=cline1(ipos(1,1):ipos(2,1))
              call util_string_split_sep(cdum,1000,nwords,ipos,' ',istat)
              if (nwords.ne.5) then
                write(chmag,*) nmag
                chmag="mp_" // trim(adjustl(chmag))
                chmoth=chmag
                chmoth(1:1)="M"
                cline1=trim(cdum)//" "//adjustl(trim(chmag))//" "//adjustl(trim(chmoth))
                if (ncom.gt.1) cline1=trim(cline1) // " " // " "//trim(comdum)
              endif
              write(lunbpe,'(a)')trim(cline1)
              write(lunbpe,'(a)')trim(cline2)
              write(lunbpe,'(a)') trim(cline3)
            endif !bc
            do iplan=1,nplan
2             read(lunscr,'(a)') cplan
              if (cplan(1:1).eq.'{') then
                do while(cplan(1:1).ne.'}')
                  read(lunscr,'(a)') cplan
                enddo
              endif
              if (bc.ne.0.0d0) write(lunbpe,'(a)') trim(cplan)
              if (cplan(1:1).eq.'*') goto 2
              read(cplan,*) ncorn
              do icorn=1,ncorn
3               read(lunscr,'(a)') cplan
                if (cplan(1:1).eq.'{') then
                  do while(cplan(1:1).ne.'}')
                    read(lunscr,'(a)') cplan
                  enddo
                endif
                if (cplan(1:1).eq.'*') goto 3
                if (bc.ne.0.0d0) write(lunbpe,'(a)') trim(cplan)
              enddo !ncorn
            enddo !nplan
          endif !have planes
        endif

        c64='Cyl'
c        call util_string_substring(cline3,trim(c64),ianf,iend,istat)

        if (icomread.eq.1.and.cline3(1:3).eq.'Cyl') then

          read(cline1,*)cen
          read(cline2,*)bc,xm,ym,zm,imat

          if (imat.eq.0) then
            bc=0.0d0
            kmat=0
          else
            kmat=materials(2,imat)
          endif

          if (bc.ne.0.0d0) then
            write(lunbpe,'(a)')'*'
            nmag=nmag+1
            call util_string_split_sep(cline1,1000,ncom,ipos,'!',istat)
            if (ncom.gt.1) then
              comdum=cline1(ipos(1,2):ipos(2,2))
            endif
            cdum=cline1(ipos(1,1):ipos(2,1))
            call util_string_split_sep(cdum,1000,nwords,ipos,' ',istat)
            if (nwords.ne.5) then
              write(chmag,*) nmag
              chmag="mp_" // trim(adjustl(chmag))
              chmoth=chmag
              chmoth(1:1)="M"
              cline1=trim(cdum)//" "//adjustl(trim(chmag))//" "//adjustl(trim(chmoth))
              if (ncom.gt.1) cline1=trim(cline1) // " " // " "//trim(comdum)
            endif
            write(lunbpe,'(a)')trim(cline1)
            write(lunbpe,'(a)')trim(cline2)
            write(lunbpe,'(a)') trim(cline3)
          endif !bc

          ndl=1
          do while (ndl.le.4)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).eq.'*'.or.cline(1:1).eq.'!') then
              cycle
            else
              ndl=ndl+1
            endif
            if (bc.ne.0.0d0.and.kmat.ne.0) then
              if (coating.ne.0.0d0.and.kmat.ne.2) then
                call util_string_split(cline,1000,nwords,ipos,istat)
                if (nwords.gt.4) cdum=cline(ipos(1,5):len_trim(cline))
                read(cline,*)rmin,rmax,polhei,ang
                r=(rmax-rmin)/2.0d0
                rmin=rmin+coating
                rmax=rmax-coating
                if (r.gt.0.0d0.and.abs(ang-twopi1).gt.2.0d0*coating/r)
     &            ang=ang-2.0d0*coating/r
                write(cline,*) rmin,rmax,polhei,ang
                write(lunbpe,'(a)') trim(cline)
                call util_string_trim(cline,nfirst,nlast)
                cline=cline(nfirst:nlast) // " " // trim(cdum)
              endif !coat
              write(lunbpe,'(a)') trim(cline)
            endif !bc
          enddo
          cycle
        endif

        kblockch=0

        c64='BlockUsChamf'
        call util_string_substring(cline3,trim(c64),ianf,iend,istat)
        if (icomread.eq.1.and.istat.eq.0) then
          kblockch=-1
          cline3='Corners ' // cline3(iend+1:len_trim(cline3))
        endif

        c64='BlockDsChamf'
        call util_string_substring(cline3,trim(c64),ianf,iend,istat)
        if (icomread.eq.1.and.istat.eq.0) then
          kblockch=1
          cline3='Corners ' // cline3(iend+1:len_trim(cline3))
        endif

        if (kblockch.ne.0) then

          read(cline1,*)cen
          read(cline2,*)bc,xm,ym,zm,imat

          if (imat.eq.0) then
            bc=0.0d0
            kmat=0
          else
            kmat=materials(2,imat)
          endif

          if (bc.ne.0.0d0) then
            write(lunbpe,'(a)')'*'
            nmag=nmag+1
            call util_string_split_sep(cline1,1000,ncom,ipos,'!',istat)
            if (ncom.gt.1) then
              comdum=cline1(ipos(1,2):ipos(2,2))
            endif
            cdum=cline1(ipos(1,1):ipos(2,1))
            call util_string_split_sep(cdum,1000,nwords,ipos,' ',istat)
            if (nwords.ne.5) then
              write(chmag,*) nmag
              chmag="mp_" // trim(adjustl(chmag))
              chmoth=chmag
              chmoth(1:1)="M"
              cline1=trim(cdum)//" "//adjustl(trim(chmag))//" "//adjustl(trim(chmoth))
              if (ncom.gt.1) cline1=trim(cline1) // " " // " "//trim(comdum)
            endif
            write(lunbpe,'(a)')trim(cline1)
            write(lunbpe,'(a)')trim(cline2)
            write(lunbpe,'(a)')trim(cline3)
          endif !bc

          ndl=1
          do while (ndl.le.1)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).eq.'*'.or.cline(1:1).eq.'!') then
              cycle
            else
              ndl=ndl+1
            endif
            if (bc.ne.0.0d0) then
                call util_string_split(cline,1000,nwords,ipos,istat)
                if (nwords.gt.4) cdum=cline(ipos(1,5):len_trim(cline))
                read(cline,*)dx,dy,dz,chamf
                if (kmat.eq.2) then
                  call util_shrink_blockchamf(dx,dy,dz,chamf,kblockch,
     &              coating*0.0d0,nshrink,xbuff,ybuff,zbuff)
                else
                  call util_shrink_blockchamf(dx,dy,dz,chamf,kblockch,
     &              coating,nshrink,xbuff,ybuff,zbuff)
                endif
                write(cshrink,*) nshrink
                write(lunbpe,'(a)') "'" // trim(adjustl(cshrink)) // "'"
                do i=1,nshrink
                  write(lunbpe,*)xbuff(i),ybuff(i),zbuff(i)
                enddo
            endif !bc
          enddo !ndl
          cycle
        endif

        c64='BlockChamf'
        call util_string_substring(cline3,trim(c64),ianf,iend,istat)

        if (icomread.eq.1.and.istat.eq.0) then

          read(cline1,*)cen
          read(cline2,*)bc,xm,ym,zm,imat

          if (imat.eq.0) then
            bc=0.0d0
            kmat=0
          else
            kmat=materials(2,imat)
          endif

          if (bc.ne.0.0d0) then
            write(lunbpe,'(a)')'*'
            nmag=nmag+1
            call util_string_split_sep(cline1,1000,ncom,ipos,'!',istat)
            if (ncom.gt.1) then
              comdum=cline1(ipos(1,2):ipos(2,2))
            endif
            cdum=cline1(ipos(1,1):ipos(2,1))
            call util_string_split_sep(cdum,1000,nwords,ipos,' ',istat)
            if (nwords.ne.5) then
              write(chmag,*) nmag
              chmag="mp_" // trim(adjustl(chmag))
              chmoth=chmag
              chmoth(1:1)="M"
              cline1=trim(cdum)//" "//adjustl(trim(chmag))//" "//adjustl(trim(chmoth))
              if (ncom.gt.1) cline1=trim(cline1) // " " // " "//trim(comdum)
            endif
            write(lunbpe,'(a)')trim(cline1)
            write(lunbpe,'(a)')trim(cline2)
            cline3='Corners ' // cline3(iend+1:len_trim(cline3))
            write(lunbpe,'(a)')trim(cline3)
          endif !bc

          ndl=1
          do while (ndl.le.1)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).eq.'*'.or.cline(1:1).eq.'!') then
              cycle
            else
              ndl=ndl+1
            endif
            if (bc.ne.0.0d0) then
                call util_string_split(cline,1000,nwords,ipos,istat)
                if (nwords.gt.4) cdum=cline(ipos(1,5):len_trim(cline))
                read(cline,*)dx,dy,dz,chamf
                if (kmat.eq.2) then
                  call util_shrink_blockchamf(dx,dy,dz,chamf,kblockch,
     &              coating*0.0d0,nshrink,xbuff,ybuff,zbuff)
                else
                  call util_shrink_blockchamf(dx,dy,dz,chamf,kblockch,
     &              coating,nshrink,xbuff,ybuff,zbuff)
                endif
                write(cshrink,*) nshrink
                write(lunbpe,'(a)') "'" // trim(adjustl(cshrink)) // "'"
                do i=1,nshrink
                  write(lunbpe,*)xbuff(i),ybuff(i),zbuff(i)
                enddo
            endif !bc
          enddo !ndl
          cycle
        endif

        kblock=0
        c64='Block'
        call util_string_substring(cline3,trim(c64),ianf,iend,istat)
        if (icomread.eq.1.and.(istat.eq.0.or.cline3(1:3).eq.'-6 ')) kblock=kblock+1
        call util_string_trim(cline3,nfirst,nlast)

        if (kblock.ne.0) then

          read(cline1,*)cen
          read(cline2,*)bc,xm,ym,zm,imat

          if (imat.eq.0) then
            bc=0.0d0
            kmat=0
          else
            kmat=materials(2,imat)
          endif

          if (bc.ne.0.0d0) then
            write(lunbpe,'(a)')'*'
            nmag=nmag+1
            call util_string_split_sep(cline1,1000,ncom,ipos,'!',istat)
            if (ncom.gt.1) then
              comdum=cline1(ipos(1,2):ipos(2,2))
            endif
            cdum=cline1(ipos(1,1):ipos(2,1))
            call util_string_split_sep(cdum,1000,nwords,ipos,' ',istat)
            if (nwords.ne.5) then
              write(chmag,*) nmag
              chmag="mp_" // trim(adjustl(chmag))
              chmoth=chmag
              chmoth(1:1)="M"
              cline1=trim(cdum)//" "//adjustl(trim(chmag))//" "//adjustl(trim(chmoth))
              if (ncom.gt.1) cline1=trim(cline1) // " " // " "//trim(comdum)
            endif
            write(lunbpe,'(a)')trim(cline1)
            write(lunbpe,'(a)')trim(cline2)
            write(lunbpe,'(a)') trim(cline3)
          endif !bc

          ndl=1
          do while (ndl.le.1)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).eq.'*'.or.cline(1:1).eq.'!') then
              cycle
            else
              ndl=ndl+1
            endif
            if (bc.ne.0.0d0) then
              if (coating.ne.0.0d0.and.kmat.ne.2) then
                call util_string_split(cline,1000,nwords,ipos,istat)
                if (nwords.gt.3) cdum=cline(ipos(1,4):len_trim(cline))
                read(cline,*)dx,dy,dz
                write(cline,*)dx-2.0*coating,dy-2.0*coating,dz+2.0*coating
                call util_string_trim(cline,nfirst,nlast)
                cline=cline(nfirst:nlast) // " " // trim(cdum)
              endif !coat
              write(lunbpe,'(a)') trim(cline)
            endif !bc
          enddo !ndl
          cycle
        endif

        kcorn=0

        c64='Corners'
        call util_string_substring(cline3,trim(c64),ianf,iend,istat)
        if (icomread.eq.1.and.istat.eq.0) kcorn=1

        c64='File'
        call util_string_substring(cline3,trim(c64),ianf,iend,istat)
        if (icomread.eq.1.and.istat.eq.0) then
          kcorn=1
          cline3='Corners ' // cline3(iend+1:len_trim(cline3))
        endif

        if (kcorn.ne.0) then

          read(cline2,*)bc,xm,ym,zm,imat
          read(cline1,*)cen

          if (imat.eq.0) then
            bc=0.0d0
            kmat=0
          else
            kmat=materials(2,imat)
          endif

          if (bc.ne.0.0d0.and.imat.ne.0) then
            write(lunbpe,'(a)')'*'
            nmag=nmag+1
            call util_string_split_sep(cline1,1000,ncom,ipos,'!',istat)
            if (ncom.gt.1) then
              comdum=cline1(ipos(1,2):ipos(2,2))
            endif
            cdum=cline1(ipos(1,1):ipos(2,1))
            call util_string_split_sep(cdum,1000,nwords,ipos,' ',istat)
            if (nwords.ne.5) then
              write(chmag,*) nmag
              chmag="mp_" // trim(adjustl(chmag))
              chmoth=chmag
              chmoth(1:1)="M"
              cline1=trim(cdum)//" "//adjustl(trim(chmag))//" "//adjustl(trim(chmoth))
              if (ncom.gt.1) cline1=trim(cline1) // " " // " "//trim(comdum)
            endif
            write(lunbpe,'(a)')trim(cline1)
            write(lunbpe,'(a)')trim(cline2)
            write(lunbpe,'(a)') trim(cline3)
          endif !bc

          ndl=1
          nbuff=0
          do while (ndl.le.1)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).eq.'*'.or.cline(1:1).eq.'!') then
              cycle
            else
              ndl=ndl+1
              read(cline,*)npoints
            endif
          enddo

          ndl=1
          do while (ndl.le.npoints)
            read(lunscr,'(a)')cline
            cline=trim(adjustl(cline))
            if (cline(1:1).eq.'*'.or.cline(1:1).eq.'!') then
              cycle
            else
              ndl=ndl+1
            endif
            if (bc.ne.0.0d0) then
              if (coating.ne.0.0d0.and.kmat.ne.2) then
                call util_string_split(cline,1000,nwords,ipos,istat)
                cbuff(ndl-1)=cline(1:ipos(2,3))
                read(cline,*)dx,dy,dz
                nbuff=nbuff+1
                xbuff(nbuff)=dx
                ybuff(nbuff)=dy
                zbuff(nbuff)=dz
                call util_string_trim(cline,nfirst,nlast)
                cline=cline(nfirst:nlast) // " " // trim(cdum)
              else !coating
                if (ndl.eq.2) then
                  write(cdum,*) npoints
                  call util_string_trim(cdum,nfirst,nlast)
                  write(lunbpe,'(a)') cdum(nfirst:nlast)
                endif
                write(lunbpe,'(a)') trim(cline)
              endif !coat
            endif
          enddo

          if (imat.ne.0.and.coating.ne.0.0d0.and.kmat.ne.2) then
            call util_shrink_xyz(nbuff,xbuff,ybuff,zbuff,cen,coating,tiny,
     &        nbuff,xbuff,ybuff,zbuff,istat)
            write(cdum,*) nbuff
            call util_string_trim(cdum,nfirst,nlast)
            write(lunbpe,'(a)') cdum(nfirst:nlast)
            do i=1,nbuff
              write(cline,*)xbuff(i),ybuff(i),zbuff(i)
              call util_string_trim(cline,nfirst,nlast)
              cline=cline(nfirst:nlast)
              write(lunbpe,'(a)') trim(cline)
            enddo
          endif !coating
          cycle
        endif

        if (itreat.eq.0.or.itreat.eq.7) write(lunbpe,'(a)') trim(cline3)

      enddo !nlines

9999  close(lunscr)

      flush(lunbpe)
      close(lunbpe)

      return
      end
