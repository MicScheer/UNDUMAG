*CMZ :          26/08/2023  10.27.59  by  Michael Scheer
*CMZ :  2.04/10 23/08/2023  08.02.18  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  16.11.22  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  12.23.20  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  16.43.57  by  Michael Scheer
*CMZ :  2.04/01 22/01/2023  13.04.45  by  Michael Scheer
*CMZ :  2.04/00 17/01/2023  09.24.20  by  Michael Scheer
*CMZ :  2.03/00 22/08/2022  12.31.51  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  15.17.16  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcbuff_to_magnets

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

*KEEP,grarad,T=F77.
c-----------------------------------------------------------------------
c     grarad.cmn
c-----------------------------------------------------------------------
      double precision, parameter ::
     &  PI1=3.141592653589793D0,
     &  TWOPI1=2.0D0*PI1,HALFPI1=PI1/2.0D0,
     &  GRARAD1=PI1/180.0d0,RADGRA1=180.0d0/PI1
*KEND.

      character(2048) cline,cbuff(5),cfile,cline1
      character(128) cword,ckey

      double precision, dimension (:), allocatable :: xp,yp,zp,xpc,ypc,zpc

      double precision undumag_variable_getval,size(3),dphi

      double precision x,dx,dy,dz,Br(5),xc,yc,zc,gcen(3),chamf,
     &  r,h,phi,radin,radout,height,angle,xyz(3),vol

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      integer i,k,l,m,kb,ib,ip,npoi,ir,ih,iphi,limit,
     &  nxdiv,nydiv,nzdiv,nhull,nface,nedge,kfacelast,kblockch

      integer ipos(2,1000),jpos(2,1000),nwords,istat,ibrn,ifound

      integer ifailhull,lun,ieof,kfail
      logical lexist

      !xhull,yhull,zhull are absolute at the end of this routine

      !call util_break
      nmag=nmag_t+nspecmag_t+nclccop_t

      allocate(t_magnets(nmag),t_magnets_copy(nmag))
      allocate(brnmat(2,nmag_t+nspecmag_t))

      nbrnmat=0

      nmag=0
      niron=0
      ncornmax=12
      nplanmax=8

      allocate(
     &  xpuffer1(ncornmax),ypuffer1(ncornmax),zpuffer1(ncornmax),
     &  xpuffer2(ncornmax),ypuffer2(ncornmax),zpuffer2(ncornmax),
     &  xpuffer3(ncornmax),ypuffer3(ncornmax),zpuffer3(ncornmax),
     &  xp(ncornmax),yp(ncornmax),zp(ncornmax),
     &  xpc(ncornmax),ypc(ncornmax),zpc(ncornmax),
     &  kface((ncornmax+1)*ncornmax),kedge(4,2*ncornmax-2),khull(ncornmax))

      ib=0
      kb=0

      limit=nclcmag+nclcspec

      do while (ib.lt.limit)

        if (kb.eq.0.and.ib.ge.nclcmag) then
          ib=0
          kb=1
          limit=nclcspec
        endif

        ib=ib+1

        if (kb.eq.0) then
          cline = clcmag(ib)
        else
          cline = clcspec(ib)
        endif

        call util_string_split(cline,1000,nwords,ipos,istat)

        if (cline(ipos(1,1):ipos(2,1)).eq.'Magnet'.or.
     &      cline(ipos(1,1):ipos(2,1)).eq.'Pole') then
          nmag=nmag+1
          t_magnets(nmag)%kmodule=magmodule(nmag)
          t_magnets(nmag)%IsSpecial=0
          cycle
        else if (cline(ipos(1,1):ipos(2,1)).eq.'Special_Magnet'.or.
     &      cline(ipos(1,1):ipos(2,1)).eq.'Special_Pole') then
          nspecmag=nspecmag+1
          nmag=nmag+1
          t_magnets(nmag)%kmodule=0
          t_magnets(nmag)%IsSpecial=1
          cycle
        endif

        ckey=cline(ipos(1,1):ipos(2,1))

        if (kb.eq.0) then
          cbuff=clcmag(ib:ib+4)
        else
          cbuff=clcspec(ib:ib+4)
        endif

        ib=ib+4

        t_magnets(nmag)%cnam=cline(ipos(1,2):ipos(2,2))
        t_magnets(nmag)%cmoth=cline(ipos(1,3):ipos(2,3))

        do m=1,nmag-1
          if (t_magnets(m)%cnam.eq.t_magnets(nmag)%cnam) then
            print*,""
            print*,"*** Error in clcbuff_to_magnets: Duplicate magnet ",
     &        t_magnets(m)%cnam
            print*,""
            stop "*** Program UNDUMAG aborted ***"
          endif
        enddo

        t_magnets(nmag)%cfile=''

        cword=cline(ipos(1,4):ipos(2,4))

        if (cword(1:1).eq.'$') then
          t_magnets(nmag)%icol=nint(undumag_variable_getval(cword))
        else
          read(cword,*)t_magnets(nmag)%icol
        endif

        cline=cbuff(2)
        call util_string_split(cline,1000,nwords,ipos,istat)

        do i=1,3
          cword=cline(ipos(1,i):ipos(2,i))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%xyz(i)=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%xyz(i)
          endif
        enddo

        xc=t_magnets(nmag)%xyz(1)
        yc=t_magnets(nmag)%xyz(2)
        zc=t_magnets(nmag)%xyz(3)

        cline=cbuff(3)
        call util_string_split(cline,1000,nwords,ipos,istat)

        if (kb.eq.0) then
          cline1=clcmag(ib-5)
        else
          cline1=clcspec(ib-5)
        endif

        if (cline1.eq.'Pole'.or.cline1.eq. 'Special_Pole') then
          niron=niron+1
          cword = cline(ipos(1,1):ipos(2,1))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%imat=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%imat
          endif
          t_magnets(nmag)%IsPole=1
        else
          do i=1,5
            cword=cline(ipos(1,i):ipos(2,i))
            if (cword(1:1).eq.'$') then
              cword=cline(ipos(1,i):ipos(2,i))
              Br(i)=undumag_variable_getval(cword)
            else
              read(cword,*)br(i)
            endif
          enddo
          t_magnets(nmag)%IsPole=0
          if (norm2(br(2:4)).ne.0.0d0) br(2:4)=br(2:4)/norm2(br(2:4))
          if (br(5).eq.0.0d0) br=0.0d0

          t_magnets(nmag)%brn=br(1)
          t_magnets(nmag)%br=br(2:4)*br(1)
          t_magnets(nmag)%imat=nint(br(5))

          ifound=0
          do ibrn=1,nbrnmat
            if (br(1).eq.brnmat(2,ibrn).and.br(5).eq.brnmat(1,ibrn)) then
              ifound=1
              exit
            endif
          enddo
          if (ifound.eq.0) then
            nbrnmat=nbrnmat+1
            brnmat(1,nbrnmat)=br(5) !material index
            brnmat(2,nbrnmat)=br(1) !Br
          endif
        endif  !Pole

        t_magnets(nmag)%ctype=ckey(1:32)
        t_magnets(nmag)%IsBlock=0
        t_magnets(nmag)%IsRotated=0

        if (ckey.eq.'Block') then

          if (irecrepl.ne.0) then
            t_magnets(nmag)%IsBlock=1
          else
            t_magnets(nmag)%IsBlock=-1
          endif

          cline=cbuff(4)
          call util_string_split(cline,1000,nwords,jpos,istat)

          do i=1,3
            cword=cline(jpos(1,i):jpos(2,i))
            if (cword(1:1).eq.'$') then
              t_magnets(nmag)%size(i)=undumag_variable_getval(cword)
            else
              read(cword,*)t_magnets(nmag)%size(i)
            endif
            if (t_magnets(nmag)%IsPole.eq.0.and.coating.ne.0.0d0) then
              t_magnets(nmag)%size(i)=t_magnets(nmag)%size(i)-2.0d0*coating
            endif
            if (t_magnets(nmag)%size(i).le.0.0d0) then
              t_magnets(nmag)%brn=0.0d0
              t_magnets(nmag)%br=0.0d0
            endif
          enddo

          cline=cbuff(5)
          call util_string_split(cline,1000,nwords,jpos,istat)

          cword=cline(jpos(1,1):jpos(2,1))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nxdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nxdiv
          endif

          cword=cline(jpos(1,2):jpos(2,2))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nydiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nydiv
          endif

          cword=cline(jpos(1,3):jpos(2,3))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nzdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nzdiv
          endif

          cword=cline(jpos(1,4):jpos(2,4))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%yfracdiv=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%yfracdiv
          endif

          cword=cline(jpos(1,5):jpos(2,5))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%zfracdiv=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%zfracdiv
          endif

          xc=t_magnets(nmag)%xyz(1)
          yc=t_magnets(nmag)%xyz(2)
          zc=t_magnets(nmag)%xyz(3)

          npoi=8

          xp(1)=xc-t_magnets(nmag)%size(1)/2.0d0
          xp(2)=xc+t_magnets(nmag)%size(1)/2.0d0
          xp(3)=xp(2)
          xp(4)=xp(1)
          xp(5:8)=xp(1:4)

          yp(1:4)=yc-t_magnets(nmag)%size(2)/2.0d0
          yp(5:8)=yc+t_magnets(nmag)%size(2)/2.0d0

          zp(1:2)=zc-t_magnets(nmag)%size(3)/2.0d0
          zp(3:4)=zc+t_magnets(nmag)%size(3)/2.0d0
          zp(5:8)=zp(1:4)

          else if (
     &        ckey.eq.'BlockChamf'.or.
     &        ckey.eq.'BlockUsChamf'.or.
     &        ckey.eq.'BlockDsChamf'
     &        ) then

          cline=cbuff(4)
          call util_string_split(cline,1000,nwords,jpos,istat)

          do i=1,3
            cword=cline(jpos(1,i):jpos(2,i))
            if (cword(1:1).eq.'$') then
              t_magnets(nmag)%size(i)=undumag_variable_getval(cword)
            else
              read(cword,*)t_magnets(nmag)%size(i)
            endif
            if (t_magnets(nmag)%size(i).le.0.0d0) then
              t_magnets(nmag)%brn=0.0d0
              t_magnets(nmag)%br=0.0d0
            endif
          enddo

          cword=cline(jpos(1,4):jpos(2,4))
          if (cword(1:1).eq.'$') then
            chamf=undumag_variable_getval(cword)
          else
            read(cword,*) chamf
          endif

          kblockch=0
          t_magnets(nmag)%UsChamf=chamf
          t_magnets(nmag)%DsChamf=chamf

          if (ckey.eq.'BlockUsChamf') then
            kblockch=-1
            t_magnets(nmag)%DsChamf=0.0d0
          else if (ckey.eq.'BlockDsChamf') then
            kblockch=1
            t_magnets(nmag)%UsChamf=0.0d0
          endif

          npoi=12

          dx=t_magnets(nmag)%size(1)
          dy=t_magnets(nmag)%size(2)
          dz=t_magnets(nmag)%size(3)

          if (dx.le.0.0d0.or.dy.le.0.0d0.or.dz.le.0.0d0) then
            t_magnets(nmag)%brn=0.0d0
            t_magnets(nmag)%br=0.0d0
          endif

          if (t_magnets(nmag)%IsPole.ne.0.) then
            call util_shrink_blockchamf(dx,dy,dz,chamf,kblockch,
     &        coating*0.0d0,npoi,xp,yp,zp)
          else
            call util_shrink_blockchamf(dx,dy,dz,chamf,kblockch,
     &        coating,npoi,xp,yp,zp)
          endif

          xp=xp+xc
          yp=yp+yc
          zp=zp+zc

          cline=cbuff(5)
          call util_string_split(cline,1000,nwords,jpos,istat)

          cword=cline(jpos(1,1):jpos(2,1))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nxdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nxdiv
          endif

          cword=cline(jpos(1,2):jpos(2,2))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nydiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nydiv
          endif

          cword=cline(jpos(1,3):jpos(2,3))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nzdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nzdiv
          endif

          cword=cline(jpos(1,4):jpos(2,4))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%yfracdiv=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%yfracdiv
          endif

          cword=cline(jpos(1,5):jpos(2,5))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%zfracdiv=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%zfracdiv
          endif

        else if (ckey.eq.'Cylinder') then

          cline=cbuff(4)
          call util_string_split(cline,1000,nwords,jpos,istat)

          do i=1,3
            cword=cline(jpos(1,i):jpos(2,i))
            if (cword(1:1).eq.'$') then
              t_magnets(nmag)%size(i)=undumag_variable_getval(cword)
            else
              read(cword,*)t_magnets(nmag)%size(i)
            endif
            if (t_magnets(nmag)%size(i).le.0.0d0) then
              t_magnets(nmag)%brn=0.0d0
              t_magnets(nmag)%br=0.0d0
            endif
          enddo

          cword=cline(jpos(1,4):jpos(2,4))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%size(i)=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%cylphi
          endif

          cline=cbuff(5)
          call util_string_split(cline,1000,nwords,jpos,istat)

          cword=cline(jpos(1,1):jpos(2,1))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nxdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nxdiv
          endif

          cword=cline(jpos(1,2):jpos(2,2))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nydiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)nydiv
            phi=t_magnets(nmag)%cylphi
            nydiv=max(nydiv,int(phi/45.0))+2
            t_magnets(nmag)%nydiv=nydiv
          endif

          cword=cline(jpos(1,3):jpos(2,3))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nzdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nzdiv
          endif

          radin=t_magnets(nmag)%size(1)
          radout=t_magnets(nmag)%size(2)
          height=t_magnets(nmag)%size(3)
          angle=t_magnets(nmag)%cylphi

          xyz=t_magnets(nmag)%xyz

          if (radin.lt.tiny) radin=tiny

          npoi=4*(nydiv+1)

          if(npoi.gt.ncornmax) then
            ncornmax=npoi
            deallocate(xp,yp,zp,xpc,ypc,zpc,kface,kedge,khull,
     &        xpuffer1,ypuffer1,zpuffer1,
     &        xpuffer2,ypuffer2,zpuffer2,
     &        xpuffer3,ypuffer3,zpuffer3
     &        )
            allocate(
     &        xpuffer1(ncornmax),ypuffer1(ncornmax),zpuffer1(ncornmax),
     &        xpuffer2(ncornmax),ypuffer2(ncornmax),zpuffer2(ncornmax),
     &        xpuffer3(ncornmax),ypuffer3(ncornmax),zpuffer3(ncornmax),
     &        xp(ncornmax),yp(ncornmax),zp(ncornmax),
     &        xpc(ncornmax),ypc(ncornmax),zpc(ncornmax),
     &        kface((npoi+1)*npoi),kedge(4,2*npoi-2),khull(ncornmax))
          endif

          ip=0
          r=radin
          npoi=0
          dphi=angle/dble(nydiv)*grarad1
          do ir=1,2
            h=-height/2.0d0
            do ih=1,2
              phi=-angle/2.0d0*grarad1
              do iphi=1,nydiv+1
                ip=ip+1
                xp(ip)=r*sin(phi)+xc
                yp(ip)=h+yc
                zp(ip)=r*cos(phi)+zc
                phi=phi+dphi
              enddo
              h=height/2.0d0
            enddo
            r=radout
          enddo

          npoi=ip

          call clcmag_cut_cyl(nmag)

        else if (ckey.eq.'Corners'.or.ckey.eq.'File') then

          cline=cbuff(4)
          call util_string_split(cline,1000,nwords,jpos,istat)

          cword=cline(jpos(1,1):jpos(2,1))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nxdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nxdiv
          endif

          cword=cline(jpos(1,2):jpos(2,2))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nydiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nydiv
          endif

          cword=cline(jpos(1,3):jpos(2,3))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%nzdiv=nint(undumag_variable_getval(cword))
          else
            read(cword,*)t_magnets(nmag)%nzdiv
          endif

          cword=cline(jpos(1,4):jpos(2,4))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%yfracdiv=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%yfracdiv
          endif

          cword=cline(jpos(1,5):jpos(2,5))
          if (cword(1:1).eq.'$') then
            t_magnets(nmag)%zfracdiv=undumag_variable_getval(cword)
          else
            read(cword,*)t_magnets(nmag)%zfracdiv
          endif

          if (ckey.eq.'Corners') then

            read(cbuff(5),*) npoi

            if(npoi.gt.ncornmax) then
              ncornmax=npoi
              deallocate(xp,yp,zp,xpc,ypc,zpc,kface,kedge,khull,
     &          xpuffer1,ypuffer1,zpuffer1,
     &          xpuffer2,ypuffer2,zpuffer2,
     &          xpuffer3,ypuffer3,zpuffer3
     &          )
              allocate(
     &          xpuffer1(ncornmax),ypuffer1(ncornmax),zpuffer1(ncornmax),
     &          xpuffer2(ncornmax),ypuffer2(ncornmax),zpuffer2(ncornmax),
     &          xpuffer3(ncornmax),ypuffer3(ncornmax),zpuffer3(ncornmax),
     &          xp(ncornmax),yp(ncornmax),zp(ncornmax),
     &          xpc(ncornmax),ypc(ncornmax),zpc(ncornmax),
     &          kface((npoi+1)*npoi),kedge(4,2*npoi-2),khull(ncornmax))
            endif

            do i=1,npoi

              ib=ib+1
              if (kb.eq.0) then
                cline=clcmag(ib)
              else
                cline=clcspec(ib)
              endif

              call util_string_split(cline,1000,nwords,ipos,istat)

              do k=1,3
                cword=cline(ipos(1,k):ipos(2,k))
                if (cword(1:1).eq.'$') then
                  x=undumag_variable_getval(cword)
                else
                  read(cword,*)x
                endif
                if (k.eq.1) then
                  xp(i)=x+xc
                else if (k.eq.2) then
                  yp(i)=x+yc
                else if (k.eq.3) then
                  zp(i)=x+zc
                endif
              enddo !k

            enddo !npoi

          else !ckey

            cfile=cbuff(5)
            cline=trim(cfile)
            l=len_trim(cfile)
            if (cfile(1:1).eq."'") then
              cline=cfile(2:l-1)
            endif
            cfile=cline
            t_magnets(nmag)%cfile=trim(cfile)

            inquire(file=trim(cfile),exist=lexist)
            if (lexist.eqv..false.) then
              write(lun6,*)"*** Error in clcbuff_to_magnets: File"
              write(lun6,*)trim(cfile)
              write(lun6,*)"not found ***"
              stop
            endif

            open(newunit=lun,file=trim(cfile),status='old')
            npoi=0
            do while(.true.)
              call util_skip_comment_empty_end(lun,ieof)
              if (ieof.ne.0) exit
              read(lun,'(a)')cline
              npoi=npoi+1
            enddo
            rewind(lun)

            if(npoi.gt.ncornmax) then
              ncornmax=npoi
              deallocate(xp,yp,zp,kface,kedge,khull,
     &          xpuffer1,ypuffer1,zpuffer1,
     &          xpuffer2,ypuffer2,zpuffer2,
     &          xpuffer3,ypuffer3,zpuffer3)
              allocate(xp(ncornmax),yp(ncornmax),zp(ncornmax),
     &          kface((npoi+1)*npoi),kedge(4,2*npoi-2),khull(npoi),
     &          xpuffer1(ncornmax),ypuffer1(ncornmax),zpuffer1(ncornmax),
     &          xpuffer2(ncornmax),ypuffer2(ncornmax),zpuffer2(ncornmax),
     &          xpuffer3(ncornmax),ypuffer3(ncornmax),zpuffer3(ncornmax)
     &          )
            endif

            do i=1,npoi
              call util_skip_comment_empty_end(lun,ieof)
              read(lun,'(a)')cline
              call util_string_split(cline,1000,nwords,ipos,istat)
              do k=1,3
                cword=cline(ipos(1,k):ipos(2,k))
                if (cword(1:1).eq.'$') then
                  x=undumag_variable_getval(cword)
                else
                  read(cword,*)x
                endif
                if (k.eq.1) then
                  xp(i)=x+xc
                else if (k.eq.2) then
                  yp(i)=x+yc
                else if (k.eq.3) then
                  zp(i)=x+zc
                endif
              enddo !k
            enddo !npoi

          endif !Corners and File

        else

          write(lun6,*)"*** Error in clcbuff_to_magnets: Unknown magnet type:"
          write(lun6,*)trim(ckey)
          stop

        endif


        if (t_magnets(nmag)%IsPole.eq.0.and.t_magnets(nmag)%brn.eq.0.0d0) then
          !print*,"Test:",nmag,t_magnets(nmag)%brn
          nmag=nmag-1
          cycle
        endif

        call util_convex_hull_3d_overwrite(npoi,xp,yp,zp,khull,kedge,kface,
     &    nhull,nedge,nface,kfacelast,hulltiny,ifailhull)

        if (ifailhull.ne.0.or.nhull.lt.4) then
          write(lun6,*)"*** Error in clcbuff_to_magnets: Subroutine util_convex_hull_3d failed for ",
     &      trim(t_magnets(nmag)%cnam)
          stop
        endif

        if (ifailhull.ne.0.or.nhull.lt.4) then
          write(lun6,*)"*** Error in clcbuff_to_magnets: Subroutine util_convex_hull_3d failed for ",
     &      trim(t_magnets(nmag)%cnam)
          stop
        endif

        npoi=nhull

        allocate(t_magnets(nmag)%xhull0(npoi))
        allocate(t_magnets(nmag)%yhull0(npoi))
        allocate(t_magnets(nmag)%zhull0(npoi))
        allocate(t_magnets(nmag)%xhull(npoi))
        allocate(t_magnets(nmag)%yhull(npoi))
        allocate(t_magnets(nmag)%zhull(npoi))
        allocate(t_magnets(nmag)%khull(npoi))
        allocate(t_magnets(nmag)%kface(kfacelast))
        allocate(t_magnets(nmag)%kedge(4,nedge))

        t_magnets(nmag)%nhull=npoi
        t_magnets(nmag)%khull(1:npoi)=khull(1:npoi)

        gcen=0.0d0
        do i=1,npoi
          t_magnets(nmag)%xhull0(i)=xp(i)
          t_magnets(nmag)%yhull0(i)=yp(i)
          t_magnets(nmag)%zhull0(i)=zp(i)
          t_magnets(nmag)%xhull(i)=xp(i)
          t_magnets(nmag)%yhull(i)=yp(i)
          t_magnets(nmag)%zhull(i)=zp(i)
          gcen=gcen+[xp(i),yp(i),zp(i)]
        enddo

        if (t_magnets(nmag)%ctype.eq.'Cylinder') then
          size=t_magnets(nmag)%size
          vol=(size(2)**2-size(1)**2)*size(3)*t_magnets(nmag)%cylphi/360.0d0*pi1
        else
          call util_volume(npoi,xp,yp,zp,hulltiny,vol,kfail)
          if (kfail.ne.0) then
            write(lun6,*)"*** Error in clcbuff_to_magnets: Subroutine util_volume failed for ",
     &        trim(t_magnets(nmag)%cnam)
            stop
          endif
        endif

        t_magnets(nmag)%volume=vol

        gcen=gcen/npoi
        t_magnets(nmag)%gcen=gcen

        t_magnets(nmag)%kedge(:,1:nedge)=kedge(:,1:nedge)
        t_magnets(nmag)%nedge=nedge
        t_magnets(nmag)%kedge(:,1:nedge)=kedge(:,1:nedge)
        t_magnets(nmag)%nface=nface
        t_magnets(nmag)%kfacelast=kfacelast
        t_magnets(nmag)%kface(1:kfacelast)=kface(1:kfacelast)

        nplanmax=max(nplanmax,nface)

        nxdiv=t_magnets(nmag)%nxdiv
        nydiv=t_magnets(nmag)%nydiv
        nzdiv=t_magnets(nmag)%nzdiv

        if (t_magnets(nmag)%ctype.ne.'Cylinder') then
          t_magnets(nmag)%nvoxels=0
        endif

        allocate(t_magnets(nmag)%kvoxels(nxdiv,nydiv,nzdiv))
        t_magnets(nmag)%kvoxels=0
        allocate(t_magnets(nmag)%t_xyzcuts(nxdiv,nydiv,nzdiv))
        allocate(t_magnets(nmag)%t_xycuts(nxdiv,nydiv))
        allocate(t_magnets(nmag)%t_xcuts(nxdiv))

      enddo !nclcmag

      deallocate(xp,yp,zp,xpc,ypc,zpc,kface,kedge,khull)

      !call util_break


      nmag_t=0
      nspecmag_t=0
      do i=1,nmag
        if (t_magnets(i)%IsSpecial.eq.1) then
          nspecmag_t=nspecmag_t+1
        else
          nmag_t=nmag_t+1
        endif
      enddo

      nplanmax_t=nplanmax
      ncornmax_t=ncornmax

      nmagtot_t=nmag_t+nspecmag_t

      deallocate(magmodule)

      return
      end
