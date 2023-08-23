*CMZ :  2.04/07 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/06 02/05/2023  10.37.14  by  Michael Scheer
*CMZ :  2.04/04 06/03/2023  16.27.00  by  Michael Scheer
*CMZ :  2.03/00 31/08/2022  08.24.49  by  Michael Scheer
*CMZ :  2.02/02 17/02/2022  11.05.25  by  Michael Scheer
*CMZ :  2.02/01 10/02/2022  21.54.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_sym

      use undumagf90m
      use bpolyederf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision xsymmm,xlen,dx,dxcon(100),
     &  xmin,xmax,ymin,ymax,zmin,zmax,tiny2

      real g(3)

      integer i

      if (ixsym.lt.0.or.iysym.lt.0.or.izsym.lt.0) then
        print*,"*** Error in clcmag_sym: Hard symmetry operations, i.e. duplication of magnets not allowed ***"
        stop
      endif

      xcentershift=0.0d0

      xcwmin= 1.0d30
      xcwmax=-1.0d30
      ycwmin= 1.0d30
      ycwmax=-1.0d30
      zcwmin= 1.0d30
      zcwmax=-1.0d30

      if (ncwires.gt.0) then
        do i=1,ncwires
          if (wire(3,i).lt.xcwmin) xcwmin=wire(3,i)
          if (wire(6,i).lt.xcwmin) xcwmin=wire(6,i)
          if (wire(3,i).gt.xcwmax) xcwmax=wire(3,i)
          if (wire(6,i).gt.xcwmax) xcwmax=wire(6,i)
          if (wire(4,i).lt.ycwmin) ycwmin=wire(4,i)
          if (wire(7,i).lt.ycwmin) ycwmin=wire(7,i)
          if (wire(4,i).gt.ycwmax) ycwmax=wire(4,i)
          if (wire(7,i).gt.ycwmax) ycwmax=wire(7,i)
          if (wire(5,i).lt.zcwmin) zcwmin=wire(5,i)
          if (wire(8,i).lt.zcwmin) zcwmin=wire(8,i)
          if (wire(5,i).gt.zcwmax) zcwmax=wire(5,i)
          if (wire(8,i).gt.zcwmax) zcwmax=wire(8,i)
        enddo
      endif

      xmin_t=min(xmin_t,xcwmin)
      xmax_t=max(xmax_t,xcwmax)
      ymin_t=min(ymin_t,ycwmin)
      ymax_t=max(ymax_t,ycwmax)
      zmin_t=min(zmin_t,zcwmin)
      zmax_t=max(zmax_t,zcwmax)

      xmin=xmin_t
      ymin=ymin_t
      zmin=zmin_t

      xmax=xmax_t
      ymax=ymax_t
      zmax=zmax_t

      xsymmm=xsym

      if (xsymmm.eq.9999.0d0) xsymmm=xcenter
      if (xsym.ne.9999.0d0) xsym=xsym/1000.0d0

      xsymmm_t=xsymmm

      if (kxcenter.ne.0) then

        if (ixsym.eq.0) then
          xcentershift=xcenter-(xmax_t+xmin_t)/2.0d0
        else
          xcentershift=xcenter-xsymmm
        endif

        xmin_t=xmin_t+xcentershift
        xmax_t=xmax_t+xcentershift

        do i=1,ncwires
          wire(3,i)=wire(3,i)+xcentershift
          wire(6,i)=wire(6,i)+xcentershift
        enddo

        write(lun6,*)
        write(lun6,*) "xcenter, shift:",xcenter,xcentershift
        write(lun6,*)

      endif !kxcenter

      if (nmag_t+nspecmag_t.eq.0.or.(nxconv.eq.0.and.dxconv.eq.0.0d0)) then
        nxconv=1
        xconv(1)=0.0d0
        goto 1234
      endif

      nxconv=abs(nxconv)

      if (xconvmin.eq.9999.0d0) then
        xconvmin=xmin_t+mod(xmin_t,perlen)-1.0d0*perlen
        if (xconvmin.gt.xcwmin) xconvmin=xcwmin
      endif !(xconvmin.eq.9999.0d0) then

      if (xconvmax.eq.9999.0d0) then
        if (kxcenter.ne.0) then
          xconvmax=xcenter
        else
          xconvmax=xmax_t-mod(xmax_t,perlen)+2.0d0*perlen
        endif
        if (xconvmax.lt.xcwmax) xconvmax=xcwmax
      endif !(xconvmax.eq.9999.0d0) then

      if (xconvmax.le.xconvmin) xconvmin=xconvmax-perlen/2.0d0

      if (nxconv.eq.9999) then
        nxconv=min(100,nint((xconvmax-xconvmin)/perlen*2)+1)
      else if (nxconv.gt.100) then
        write(lun6,*)"*** Warning in clcmag_sym: nxconv changed to limit of 100 ***"
        nxconv=100
      endif

      if (dxconv.eq.0.0d0) then
        dxconv=(xconvmax-xconvmin)/max(1,nxconv-1)

      else if (dxconv.eq.9999.0d0) then
        dxconv=perlen/2.0d0
        if (perlen.le.0.0d0) then
          stop "*** Error in clcmag_sym: Negative period-length ***"
        endif
        nxconv=nint((xconvmax-xconvmin)/dxconv)+1
        if (nxconv.gt.100) then
          write(lun6,*)"*** Warning in clcmag_sym: dxconv changed to limit nxconv to 100 ***"
          nxconv=100
        endif
        dxconv=(xconvmax-xconvmin)/max(1,nxconv-1)

      else if (dxconv.lt.0.0d0) then
        dxcon(1)=0.0d0
        dx=1.0d0
        do i=2,nxconv
          dxcon(i)=dxcon(i-1) + dx
          dx=dx*abs(dxconv)
        enddo
        xlen=dxcon(nxconv)
        do i=2,nxconv
          xconv(i)=xconv(i-1) + (dxcon(i)-dxcon(i-1))*(xconvmax-xconvmin)/xlen
        enddo
      else
        do i=1,nxconv
          xconv(i)=xconvmin+dxconv*(i-1)
        enddo
      endif !(dxconv.lt.0.0d0) then

      do i=1,nxconv
        ! to avoid boundary effects:
        call util_random(2,g)
        g=g-0.5
        if (abs(g(1)).lt.randox10) then
          if (g(1).gt.0.0d0) then
            g(1)=g(1)+randox10
          else
            g(1)=g(1)-randox10
          endif
        endif
        xconv(i)=(xconv(i)+g(1)*randoxa)/1000.0d0
      enddo

      yconv=yconv/1000.0d0

      if (abs(g(2)).lt.randoz10) then
        if (g(2).gt.0.0d0) then
          g(2)=g(2)+randoz10
        else
          g(2)=g(2)-randoz10
        endif
      endif

      if (zconv.eq.0.0d0) then
        if (randoza.gt.0.0d0) then
          zconv=(zconv+g(2)*randoza)/1000.0d0
        else
          zconv=(zconv+randoza)/1000.0d0
        endif
      else
        zconv=zconv/1000.0d0
      endif

1234  continue

      window=uwindow
      kdebug=kudebug

      tiny=1.1d-6
      tiny=corrtiny

      if (nmag_t+nspecmag_t.eq.0.and.ncwires+nrace.eq.0.and.kbextern.eq.0) then
        xmapmin=0.1
        xmapmax=0.1
      endif

      if (xmapmin.eq.9999.0d0) then
        xmapmin=xmin_t+mod(xmin_t,perlen)-9.0d0*perlen
        if (xmapmin.gt.xcwmin) xmapmin=xcwmin
      else if (xmapmin.eq.9000.0d0) then
        xmapmin=xcenter-perlen/2.0d0
        if (xmapmin.gt.xcwmin) xmapmin=xcwmin
      else if (xmapmin.eq.-9000.0d0) then
        xmapmin=xcenter-perlen/4.0d0
        if (xmapmin.gt.xcwmin) xmapmin=xcwmin
      endif

      if (xmapmax.eq.9999.0d0) then
        xmapmax=xmax_t-mod(xmax_t,perlen)+10.0d0*perlen
        if (xmapmax.lt.xcwmax) xmapmax=xcwmax
      else if (xmapmax.eq.9000.0d0) then
        xmapmax=xcenter+perlen/2.0d0
        if (xmapmax.lt.xcwmax) xmapmax=xcwmax
      else if (xmapmax.eq.-9000.0d0) then
        xmapmax=xcenter+perlen/4.0d0
        if (xmapmax.lt.xcwmax) xmapmax=xcwmax
      endif

      if (dxmap.eq.0.0d0.and.nxmap.eq.0.0d0) then
        nxmap=1
      endif

      if (dxmap.eq.9999.0d0) then
        if (nxbeff.gt.1) then
          dxmap=perlen/(nxbeff-1)
        else
          dxmap=perlen
        endif
      endif

      if (dxmap.ne.0.0d0) nxmap=nint((xmapmax-xmapmin)/dxmap)+1

      if (nxmap.le.0) then
        write(lun6,*)"*** Warning in clcmag_sym: nxmap.le.0 ***"
      endif

      if (nymap.le.0) nymap=1
      if (nzmap.le.0) nzmap=1

      if (ixsym.gt.0) then
        if (xmin.lt.xsymmm-tiny2.and.xmax.gt.xsymmm+tiny2) then
          write(lun6,*)"*** Error in input: Option ixsym is set, but not all x-values are on the same side of xsymm!"
          write(lun6,*)"*** Program undumag aborted ***"
          stop
        endif
        dx=xsymmm-xmin
        xmax=xsymmm+dx
        xsym=xsymmm/1000.0d0
        xmapmax=-xmapmin
      else
        xmin=xmin_t
        xmax=xmax_t
      endif

      if (iysym.ne.0) then
        if (ymin.lt.-tiny2.and.ymax.gt.tiny2) then
          write(lun6,*)"*** Error in input: Option iysym is set, but not all y-values have the same sign!"
          if (iysym.gt.0) then
            write(lun6,*)"*** Program undumag aborted ***"
            stop
          endif
        endif
        ymax=max(abs(ymax),abs(ymin))
        ymin=-ymax
      else
        ymin=ymin_t
        ymax=ymax_t
      endif

      if (izsym.ne.0) then
        if (zmin.lt.-tiny2.and.zmax.gt.tiny2) then
          if (izsym.gt.0) then
            write(lun6,*)"*** Error in input: Option izsym is set, but not all z-values have the same sign!"
            write(lun6,*)"*** Program undumag aborted ***"
            stop
          else if (izsym.lt.0) then
            write(lun6,*)"*** Warning in input: Option izsym is set, but not all z-values have the same sign!"
          endif
        endif
        zmax=max(abs(zmax),abs(zmin))
        zmin=-zmax
      else
        zmin=zmin_t
        zmax=zmax_t
      endif

      nxmap=nint((xmapmax-xmapmin)/dxmap)+1

      xmin_t=xmin
      ymin_t=ymin
      zmin_t=zmin

      xmax_t=xmax
      ymax_t=ymax
      zmax_t=zmax

      return
      end
