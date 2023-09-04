*CMZ :  2.04/13 31/08/2023  13.05.39  by  Michael Scheer
*CMZ :  2.03/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/02 03/03/2022  12.09.38  by  Michael Scheer
*CMZ :  2.02/01 19/10/2021  13.27.41  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clccoil_to_coils

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      character(2048) cline,cline1
      character(128) cword,ckey

      double precision curr,x1,y1,z1,x2,y2,z2,
     &  wind19(19),bx,by,bz,p(100)

      integer i,ib,nw,icolor
      integer ipos(2,1000),nwords,nc,nwc,istat,nf,nfila

      integer lunf,ieof

      character(32) c32

      if (ncoils_t.le.0) return

      allocate(t_coils(ncoils_t))

      nrace=0
      nwind=0
      ncrace=0
      narc=0
      ncarc=0
      nrbar=0
      nthwir=0
      ncwires=0

      nw=0
      nwc=0
      nc=0
      ib=0

      if (kechocalc.ne.0) then
        print*,""
        print*,"Entered clcoil_to_coils:"
        print*,""
      endif

      do while (ib.lt.nclccoil-1)

        ib=ib+1
        cline=clccoil(ib)

        if (kechocalc.ne.0) print*,trim(cline)

        call clcstring_to_vars(cline)
        call util_string_split(cline,1000,nwords,ipos,istat)

        if (cline(ipos(1,1):ipos(2,1)).eq.'Coil') cycle

        ckey=trim(adjustl(cline(ipos(1,1):ipos(2,1))))
        nc=nc+1

        if (nwords.gt.1) then
          t_coils(nc)%cnam=cline(ipos(1,2):ipos(2,2))
        else
          write(c32,*)nc
          t_coils(nc)%cnam="Coil_" // adjustl(trim(c32))
        endif

        t_coils(nc)%ctype=trim(ckey)

        ib=ib+1
        cline=clccoil(ib)
        if (kechocalc.ne.0) print*,trim(cline)
        call clcstring_to_vars(cline)
        call util_string_split(cline,1000,nwords,ipos,istat)
        cword=cline(ipos(1,1):ipos(2,1))

        if (ckey.eq.'Filaments') then

          read(cword,*)nf
          t_coils(nc)%iibuff=ib

          nfila=0
          do i=1,nf
            ib=ib+1
            cline=clccoil(ib)
            if (kechocalc.ne.0) print*,trim(cline)
            call clcstring_to_vars(cline)
            call util_string_split(cline,1000,nwords,ipos,istat)
            cword=cline(ipos(1,1):ipos(2,1))
            read(cword,*)curr
c            if (curr.eq.0.0d0) cycle
            cword=cline(ipos(1,2):ipos(2,2))
            read(cword,*)x1
            cword=cline(ipos(1,3):ipos(2,3))
            read(cword,*)y1
            cword=cline(ipos(1,4):ipos(2,4))
            read(cword,*)z1
            cword=cline(ipos(1,5):ipos(2,5))
            read(cword,*)x2
            cword=cline(ipos(1,6):ipos(2,6))
            read(cword,*)y2
            cword=cline(ipos(1,7):ipos(2,7))
            read(cword,*)z2
            if ((x1-x2)**2+(y2-y1)**2+(z2-z1)**2.eq.0.0d0) cycle
            nfila=nfila+1
          enddo

          if (nfila.eq.0) then
            nc=nc-1
          endif

          ncwires=ncwires+nfila
          t_coils(nc)%iebuff=ib
          t_coils(nc)%params(1)=nfila

        else if (ckey.eq.'File') then

          nfila=0
          cline=trim(adjustl(clccoil(ib)))
          t_coils(nc)%cparams=trim(cline)
          open(newunit=lunf,file=cline,status='old')

          do while (.true.)
            call util_skip_comment_empty_end(lunf,ieof)
            if (ieof.ne.0) exit
            read(lunf,'(a)') cline
            if (kechocalc.ne.0) print*,trim(cline)
            call clcstring_to_vars(cline)
            read(cline,*)curr,x1,y1,z1,x2,y2,z2,icolor
c            if (curr.eq.0.0d0.or.(x1-x2)**2+(y2-y1)**2+(z2-z1)**2.eq.0.0d0) cycle
            if ((x1-x2)**2+(y2-y1)**2+(z2-z1)**2.eq.0.0d0) cycle
            nfila=nfila+1
          enddo
          close(lunf)

          if (nfila.eq.0) then
            nc=nc-1
          endif

          ncwires=ncwires+nfila
          t_coils(nc)%params(1)=nfila

        else if (ckey.eq.'RectWindings') then
          read(cline,*)t_coils(nc)%params(1:19)
c          if (t_coils(nc)%params(1).eq.0.0d0) then
c            nc=nc-1
c            cycle
c          endif
          nwind=nwind+1
        else if (ckey.eq.'Rectangular') then
          read(cline,*)t_coils(nc)%params(1:17)
c          if (t_coils(nc)%params(1).eq.0.0d0) then
c            nc=nc-1
c            cycle
c          endif
          nrace=nrace+1
        else if (ckey.eq.'RectangCirc') then
          read(cline,*)t_coils(nc)%params(1:17)
c          if (t_coils(nc)%params(1).eq.0.0d0) then
c            nc=nc-1
c            cycle
c          endif
          ncrace=ncrace+1
        else if (ckey.eq.'RectArc') then
          ib=ib+1
          cline1=clccoil(ib)
          if (kechocalc.ne.0) print*,trim(cline1)
          call clcstring_to_vars(cline1)
          cline=trim(adjustl(cline)) // " " // trim(adjustl(cline1))
          read(cline,*)t_coils(nc)%params(1:21)
c          if (t_coils(nc)%params(1).eq.0.0d0) then
c            nc=nc-1
c            cycle
c          endif
          narc=narc+1
        else if (ckey.eq.'CircArc') then
          ib=ib+1
          cline1=clccoil(ib)
          if (kechocalc.ne.0) print*,trim(cline1)
          call clcstring_to_vars(cline1)
          cline=trim(adjustl(cline)) // " " // trim(adjustl(cline1))
          read(cline,*)t_coils(nc)%params(1:20)
c          if (t_coils(nc)%params(1).eq.0.0d0) then
c            nc=nc-1
c            cycle
c          endif
          ncarc=ncarc+1
        else if (ckey.eq.'RectBar') then
          ib=ib+1
          cline1=clccoil(ib)
          if (kechocalc.ne.0) print*,trim(cline1)
          cline1=clccoil(ib)
          call clcstring_to_vars(cline1)
          cline=trim(adjustl(cline)) // " " // trim(adjustl(cline1))
          read(cline,*)t_coils(nc)%params(1:19)
c          if (t_coils(nc)%params(1).eq.0.0d0) then
c            nc=nc-1
c            cycle
c          endif
          nrbar=nrbar+1
        else if (ckey.eq.'ThickWire') then
          ib=ib+1
          cline1=clccoil(ib)
          if (kechocalc.ne.0) print*,trim(cline1)
          call clcstring_to_vars(cline1)
          cline=trim(adjustl(cline)) // " " // trim(adjustl(cline1))
          read(cline,*)t_coils(nc)%params(1:18)
c          if (t_coils(nc)%params(1).eq.0.0d0) then
c            nc=nc-1
c            cycle
c          endif
          nthwir=nthwir+1
        endif !ckey

      enddo !nclccoil

      if (ncwires.gt.0) then
        allocate(wire(nwitems,ncwires))
        wire=0.0d0
      endif

      if (nwind.gt.0) then
        allocate(wind(19,nwind))
        wind=0.0d0
      endif

      if (nrace.gt.0) then
        allocate(race(17,nrace))
        race=0.0d0
      endif

      if (ncrace.gt.0) then
        allocate(crace(17,ncrace))
        crace=0.0d0
      endif

      if (narc.gt.0) then
        allocate(arc(21,narc))
        arc=0.0d0
      endif

      if (ncarc.gt.0) then
        allocate(carc(20,ncarc))
        carc=0.0d0
      endif

      if (nrbar.gt.0) then
        allocate(rectbar(19,nrbar))
        rectbar=0.0d0
      endif

      if (nthwir.gt.0) then
        allocate(thickwire(18,nthwir))
        thickwire=0.0d0
      endif

      ncoils_t=nc

      ncwires=0
      nrace=0
      nwind=0
      ncrace=0
      narc=0
      ncarc=0
      nrbar=0
      nthwir=0

      nw=0
      nc=0
      ib=0

      do nc=1,ncoils_t

        p(1:100)=t_coils(nc)%params(1:100)
        ckey=trim(adjustl(t_coils(nc)%ctype))

        if (ckey.eq.'Filaments') then

          nwc=nwc+1

          do ib=t_coils(nc)%iibuff+1,t_coils(nc)%iebuff
            cline=clccoil(ib)
            call clcstring_to_vars(cline)
            call util_string_split(cline,1000,nwords,ipos,istat)
            cword=cline(ipos(1,1):ipos(2,1))
            read(cword,*)curr
c            if (curr.eq.0.0d0) cycle
            cword=cline(ipos(1,2):ipos(2,2))
            read(cword,*)x1
            cword=cline(ipos(1,3):ipos(2,3))
            read(cword,*)y1
            cword=cline(ipos(1,4):ipos(2,4))
            read(cword,*)z1
            cword=cline(ipos(1,5):ipos(2,5))
            read(cword,*)x2
            cword=cline(ipos(1,6):ipos(2,6))
            read(cword,*)y2
            cword=cline(ipos(1,7):ipos(2,7))
            read(cword,*)z2
            if ((x1-x2)**2+(y2-y1)**2+(z2-z1)**2.eq.0.0d0) cycle
            ncwires=ncwires+1
            cword=cline(ipos(1,8):ipos(2,8))
            read(cword,*)icolor
            wire(1,ncwires)=1 !type
            wire(2,ncwires)=curr
            wire(3,ncwires)=x1
            wire(4,ncwires)=y1
            wire(5,ncwires)=z1
            wire(6,ncwires)=x2
            wire(7,ncwires)=y2
            wire(8,ncwires)=z2
            wire(9,ncwires)=icolor
            wire(10,ncwires)=nc
            wire(11,ncwires)=nwc
          enddo

        else if (ckey.eq.'File') then
          cline=t_coils(nc)%cparams
          open(newunit=lunf,file=cline,status='old')
          nwc=nwc+1
          do i=1,nint(t_coils(nc)%params(1))
            call util_skip_comment_empty_end(lunf,ieof)
            read(lunf,'(a)') cline
            call clcstring_to_vars(cline)
            read(cline,*)curr,x1,y1,z1,x2,y2,z2,icolor
            if ((x1-x2)**2+(y2-y1)**2+(z2-z1)**2.eq.0.0d0) cycle
c            if (curr.eq.0.0d0.or.(x1-x2)**2+(y2-y1)**2+(z2-z1)**2.eq.0.0d0) cycle
            ncwires=ncwires+1
            wire(1,ncwires)=1 !type
            wire(2,ncwires)=curr
            wire(3,ncwires)=x1
            wire(4,ncwires)=y1
            wire(5,ncwires)=z1
            wire(6,ncwires)=x2
            wire(7,ncwires)=y2
            wire(8,ncwires)=z2
            wire(9,ncwires)=icolor
            wire(10,ncwires)=nc
            wire(11,ncwires)=nwc
          enddo
          close(lunf)
        else if (ckey.eq.'RectWindings') then
          wind19=t_coils(nc)%params(1:19)
          nwind=nwind+1
          wind(1,nwind)=wind19(1)
          wind(2:17,nwind)=wind19(4:19)
          wind(18:19,nwind)=wind19(2:3)
        else if (ckey.eq.'Rectangular') then
          nrace=nrace+1
          race(1:17,nrace)=t_coils(nc)%params(1:17)
        else if (ckey.eq.'RectangCirc') then
          ncrace=ncrace+1
          crace(1:17,ncrace)=t_coils(nc)%params(1:17)
        else if (ckey.eq.'RectArc') then
          narc=narc+1
          arc(1:21,narc)=t_coils(nc)%params(1:21)
        else if (ckey.eq.'CircArc') then
          ncarc=ncarc+1
          carc(1:20,ncarc)=t_coils(nc)%params(1:20)
        else if (ckey.eq.'RectBar') then
          nrbar=nrbar+1
          rectbar(1:19,nrbar)=t_coils(nc)%params(1:19)
        else if (ckey.eq.'ThickWire') then
          nthwir=nthwir+1
          thickwire(1:18,nthwir)=t_coils(nc)%params(1:18)
        endif !ckey

      enddo !nclccoil

      call undumag_bcoils(0.0d0,0.0d0,0.0d0,bx,by,bz,istat)

      return
      end
