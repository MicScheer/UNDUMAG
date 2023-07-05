*CMZ :  2.04/01 18/01/2023  17.45.21  by  Michael Scheer
*CMZ :  2.02/01 07/02/2022  13.13.34  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.01/03 15/02/2019  13.07.52  by  Michael Scheer
*CMZ :  2.01/02 25/04/2018  11.48.35  by  Michael Scheer
*CMZ :  1.25/01 19/03/2018  16.23.08  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  21.02.34  by  Michael Scheer
*CMZ :  1.19/00 20/06/2017  12.20.09  by  Michael Scheer
*CMZ :  1.18/02 13/06/2017  12.45.15  by  Michael Scheer
*CMZ :  1.17/08 27/05/2017  10.32.42  by  Michael Scheer
*CMZ :  1.11/00 04/01/2017  14.33.44  by  Michael Scheer
*CMZ :  1.10/00 18/11/2016  09.09.14  by  Michael Scheer
*CMZ :  1.09/01 06/10/2016  14.12.50  by  Michael Scheer
*CMZ :  1.07/02 25/09/2016  13.39.32  by  Michael Scheer
*CMZ :  1.07/01 25/09/2016  11.47.10  by  Michael Scheer
*CMZ :  1.07/00 24/09/2016  14.49.25  by  Michael Scheer
*CMZ :  1.06/01 21/09/2016  15.46.16  by  Michael Scheer
*CMZ :  1.06/00 21/09/2016  13.01.00  by  Michael Scheer
*CMZ :  1.04/00 14/09/2016  13.31.06  by  Michael Scheer
*CMZ :  1.02/01 08/09/2016  15.25.02  by  Michael Scheer
*CMZ :  1.02/00 23/08/2016  12.01.08  by  Michael Scheer
*CMZ :  0.00/13 31/07/2016  16.05.11  by  Michael Scheer
*CMZ :  0.00/01 25/04/2016  15.07.20  by  Michael Scheer
*CMZ :  1.17/06 01/04/2016  12.27.40  by  Michael Scheer
*CMZ :  1.17/02 09/10/2014  14.52.20  by  Michael Scheer
*CMZ :  1.17/01 03/10/2014  11.25.46  by  Michael Scheer
*CMZ :  1.17/00 25/08/2014  15.14.09  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.34.26  by  Michael Scheer
*CMZ :  1.16/01 14/03/2014  12.26.15  by  Michael Scheer
*CMZ :  1.12/16 01/06/2007  11.17.50  by  Michael Scheer
*CMZ :  1.12/08 02/08/2006  17.03.41  by  Michael Scheer
*CMZ :  1.11/03 21/06/2005  12.56.55  by  Michael Scheer
*CMZ :  1.11/01 01/03/2005  09.45.36  by  Michael Scheer
*CMZ :  1.10/04 25/02/2005  11.55.05  by  Michael Scheer
*CMZ :  2.52/05 17/08/2004  08.54.30  by  Michael Scheer
*CMZ :  1.01/01 11/08/2004  13.30.53  by  Michael Scheer
*CMZ :  1.01/00 02/03/2004  17.00.13  by  Michael Scheer
*CMZ :  1.00/01 27/02/2004  14.29.35  by  Michael Scheer
*CMZ :  1.00/00 26/02/2004  17.21.29  by  Michael Scheer
*CMZ :  0.99/13 26/02/2004  16.14.57  by  Michael Scheer
*CMZ :  0.99/12 26/02/2004  12.02.34  by  Michael Scheer
*CMZ :  0.99/11 25/02/2004  15.21.06  by  Michael Scheer
*CMZ :  0.99/10 25/02/2004  13.42.35  by  Michael Scheer
*CMZ :  0.99/09 20/02/2004  17.26.48  by  Michael Scheer
*CMZ :  0.99/08 20/02/2004  16.32.55  by  Michael Scheer
*CMZ :  0.99/07 16/02/2004  15.21.29  by  Michael Scheer
*CMZ :  0.99/03 12/02/2004  13.55.05  by  Michael Scheer
*CMZ :  0.99/00 26/01/2004  17.03.49  by  Michael Scheer
*CMZ :  0.00/08 23/01/2004  12.52.23  by  Michael Scheer
*CMZ :  0.00/07 16/01/2004  11.05.44  by  Michael Scheer
*CMZ :  0.00/06 09/01/2004  15.55.17  by  Michael Scheer
*CMZ :  0.00/05 23/12/2003  14.52.54  by  Michael Scheer
*CMZ :  0.00/04 23/12/2003  10.15.07  by  Michael Scheer
*CMZ :  0.00/02 15/12/2003  12.43.34  by  Michael Scheer
*CMZ :  0.00/01 10/12/2003  17.56.52  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003
      subroutine undumag_bpolyplot_old(iplot,xmin,xmax,ymin,ymax,zmin,zmax,
     &  theta,phi,nwitems,ncwires,wire)

*KEEP,bpolyederf90u.
      include 'bpolyederf90u.cmn'
*KEND.

      use commandlinef90m

      implicit none

*KEEP,mshplt.
      include 'mshplt.cmn'
*KEND.

      integer npawp
      parameter (npawp=10000)

      integer nwitems,ncwires
      double precision wire(nwitems,ncwires)

      real, dimension (:), allocatable :: xpl,ypl,zpl,zplm,xmpl,ympl,zmpl

      real hpaw(npawp),
     &  xplb(2),yplb(2),zplb(2),
     &  xplbo(2),yplbo(2),zplbo(2),rmtyp31,rmtyp24,rmtyp20,rlwidth,rlwidtho

      real xmin,xmax,ymin,ymax,zmin,zmax,theta,phi,
     &  x,y,z,bx,by,bz,dx,dy,dz,bxo,byo,bzo,bo,eps,
     &  xmn,xmx,ymn,ymx,zmn,zmx,
     &  xmmn,xmmx,ymmn,ymmx,zmmn,zmmx,
     &  xplmin,xplmax,yplmin,yplmax,zplmin,zplmax,
     &  rmoth,rmag,rcol,rcolo,rplan,rcorn,rmat,rmato,
     &  xc,yc,zc,xmc(1),ymc(1),zmc(1),dot0,circ0,pscal,vn,vnx,vny,vnz

c      integer ibatch
      integer i,iplot,iplot1,iplot10,iplot100,idev,
     &  imoth,imag,icol,iplan,icorn,
     &  iplano,ncorno,iline,nline,iallo,
     &  ncorn,nplanmax,ncornmax,idx,igird,imago,impl,izero,nfirst,nlast

      integer icolor,ired,igreen,iblue,luncnf,lunmag

      character(20) cdx
      character(23) cdxmm
      character(64) ctitle,cline

      common/pawc/hpaw

      data dot0/25./
      data circ0/5./
      data rmtyp20/20./
      data rmtyp24/-9999./
      data rmtyp31/31./

      data eps/0.01/

      iallo=0

      write(lun6,*)
      write(lun6,*) "Reading file undumag.mag and writing geometry to plot file undumag.eps"
      write(lun6,*)

      open(newunit=luncnf,file='.mshplt.cnf')

      write(luncnf,'(a)')"0 !idev, 0: viewer is not used"
      write(luncnf,'(a)')"-20. -20. !plot size in cm, negative values indicate HIGZ compatible mode"
      write(luncnf,'(a)')"0.8 !rescaling factor; if not one, plot files are copied and rescaled"
      write(luncnf,'(a)')"0 0 800 800 !bounding box"
      write(luncnf,'(a)')"undumag.eps !base name of plotfiles"
      write(luncnf,'(a)')"$UNDUMAG/shell/undumag_viewer.sh"
      write(luncnf,'(a)')"$UNDUMAG/shell/undumag_kill_viewer.sh"

      close(luncnf)

      nplanmax=0
      ncornmax=0

      iplano=0
      nline=0

      xmn=1.e10
      xmx=-1.e10
      ymn=1.e10
      ymx=-1.e10
      zmn=1.e10
      zmx=-1.e10

      if (jrunnum.ne.0) then
        write(ctitle,*)kundurun
        call util_string_trim(ctitle,nfirst,nlast)
        ctitle=trim(usercom)//", Run: "//ctitle(nfirst:nlast)
      else
        ctitle=trim(usercom)
      endif

      if (ncwires.gt.0) then
        do i=1,ncwires
          x=wire(3,i)
          y=wire(4,i)
          z=wire(5,i)
          if (x.lt.xmn) xmn=x
          if (x.gt.xmx) xmx=x
          if (y.lt.ymn) ymn=y
          if (y.gt.ymx) ymx=y
          if (z.lt.zmn) zmn=z
          if (z.gt.zmx) zmx=z
          x=wire(6,i)
          y=wire(7,i)
          z=wire(8,i)
          if (x.lt.xmn) xmn=x
          if (x.gt.xmx) xmx=x
          if (y.lt.ymn) ymn=y
          if (y.gt.ymx) ymx=y
          if (z.lt.zmn) zmn=z
          if (z.gt.zmx) zmx=z
        enddo
        ncornmax=2
      endif

      open(newunit=lunmag,file='undumag.mag',status='old')

      read(lunmag,'(a)')cline

1     read(lunmag,*,end=9) rmoth,rmag,rcol,rplan,rcorn,x,y,z,bx,by,bz,rmat

      if (bx**2+by**2+bz**2.eq.0.0d0) goto 1

      nline=nline+1

      imag=nint(rmag)
      icol=nint(rcol)
      iplan=nint(rplan)
      icorn=nint(rcorn)

      if (iplan.gt.nplanmax) nplanmax=iplan
      if (abs(icorn).gt.ncornmax) ncornmax=abs(icorn)

      if (x.lt.xmn) xmn=x
      if (x.gt.xmx) xmx=x
      if (y.lt.ymn) ymn=y
      if (y.gt.ymx) ymx=y
      if (z.lt.zmn) zmn=z
      if (z.gt.zmx) zmx=z

      goto 1

9     rewind (lunmag)

      if (ncornmax.eq.0) then
        write(lun6,*)"*** Warning in undumag_bpolyplot_old: Nothing to plot!?"
        goto 9999
      endif

      allocate(xpl(ncornmax))
      allocate(ypl(ncornmax))
      allocate(zpl(ncornmax))
      allocate(zplm(ncornmax))

      allocate(xmpl(ncornmax*nplanmax))
      allocate(ympl(ncornmax*nplanmax))
      allocate(zmpl(ncornmax*nplanmax))

      iallo=1

      call mlimit(npawp)
      if (jdate.ne.0) then
        call mplopt('DATE',1)
      else
        call mplopt('NDAT',1)
      endif
      call mplint(idev)
      if (jdate.ne.0) then
        call mplopt('DATE',1)
      else
        call mplopt('NDAT',1)
      endif
      call mplset('YGTI',0.)
      call mplset('GSIZ',0.3)
      call mgset('TXCI',1.)

      iplot100=abs(iplot)/100
      iplot10=(abs(iplot)-iplot100*100)/10
      iplot1=abs(iplot)-iplot100*100-iplot10*10

      call mshplt_get_line_width(rlwidtho)
      rlwidth=rlwidtho
      call mshplt_set_line_width(rlwidtho/2.)

c--- Open plotfiles {

      if (iplot.gt.0) then

        if (idev.ne.0) then
          call mgmeta(98,-111)    !seite 22
        else
          call mgmeta(-98,-111)    !seite 22
        endif

      else if (iplot.lt.0) then

        if (idev.ne.0) then
          call mgmeta(98,-113)    !seite 22
        else
          call mgmeta(-98,-113)    !seite 22
        endif

      endif !iplot

c--- Open plotfiles }

      iplano=1

      if (xmin.eq.9999.) then
        xplmin=xmn
      else
        xplmin=xmin
      endif

      if (xmax.eq.9999.) then
        xplmax=xmx
      else
        xplmax=xmax
      endif

      if (ymin.eq.9999.) then
        yplmin=ymn
      else
        yplmin=ymin
      endif

      if (ymax.eq.9999.) then
        yplmax=ymx
      else
        yplmax=ymax
      endif

      if (zmin.eq.9999.) then
        zplmin=zmn
      else
        zplmin=zmin
      endif

      if (zmax.eq.9999.) then
        zplmax=zmx
      else
        zplmax=zmax
      endif

      if (nbforcx*nbforcy*nbforcy.ne.0) then
        dx=(outbox(2,1)-outbox(1,1))*0.1
        if (xplmin.ge.outbox(1,1)) xplmin=outbox(1,1)-dx
        if (xplmax.le.outbox(2,1)) xplmax=outbox(2,1)+dx
        dy=(outbox(2,2)-outbox(1,2))*0.1
        if (yplmin.ge.outbox(1,2)) yplmin=outbox(1,2)-dy
        if (yplmax.le.outbox(2,2)) yplmax=outbox(2,2)+dy
        dz=(outbox(2,3)-outbox(1,3))*0.1
        if (zplmin.ge.outbox(1,3)) zplmin=outbox(1,3)-dz
        if (zplmax.le.outbox(2,3)) zplmax=outbox(2,3)+dz
      endif

      dx=(xplmax-xplmin)*0.05
      if (xmin.eq.9999.) then
        xplmin=xplmin-dx
      endif

      if (xmax.eq.9999.) then
        xplmax=xplmax+dx
      endif

      dy=(yplmax-yplmin)*0.05
      if (ymin.eq.9999.) then
        yplmin=yplmin-dy
      endif

      if (ymax.eq.9999.) then
        yplmax=yplmax+dy
      endif

      dz=(zplmax-zplmin)*0.05
      if (zmin.eq.9999.) then
        zplmin=zplmin-dz
      endif

      if (zmax.eq.9999.) then
        zplmax=zplmax+dz
      endif

      if (dx.eq.0.0d0) then
        dx=1.05d0
        xplmin=xplmin-dx
        xplmax=xplmax+dx
      endif

      if (dy.eq.0.0d0) then
        dy=1.05d0
        yplmin=yplmin-dy
        yplmax=yplmax+dy
      endif

      if (dz.eq.0.0d0) then
        dz=1.05d0
        zplmin=zplmin-dz
        zplmax=zplmax+dz
      endif

      if (xplmax.le.xplmin.or.zplmax.le.zplmin.or.zplmax.le.zplmin) then
        write(lun6,*)'*** Warning in undumag_bpolyplot_old: Bad coordinate system for plotting '
        goto 9999
      endif

      pscal=min(3.,sqrt(10000./((xplmax-xplmin)*(zplmax-zplmin))))

c--- 3D, top and side views {

      if (jcomment.ne.0) call mtitle(trim(ctitle))

      call mplset('YMGL',0.5)
      call mplzon(1,1,1,' ')
      call mplfra(0.,10.,0.,10.,'AB')
      call mgset('CHHE',0.4)
      call mtx(3.4,2.6,'upper magnets')
      call mtx(3.5,0.15,'lower magnets')
      call mgset('CHHE',0.3)
      call mplset('YMGL',2.)
      call muwk(0,0)

      call mplzon(2,2,1,'S')
      rewind(lunmag)
      read(lunmag,'(a)')cline
      iplano=1

c y is vertical (WAVE-system)

      if (theta.eq.0.0.and.phi.eq.0.0) then

        call mplfra(xplmin,xplmax,yplmin,yplmax,' ')
        call mplax('x (mm)', 'y (mm)')
        if (nbforcx*nbforcy*nbforcz.ne.0) call undumag_bpolypl2(forxpl,forypl,forcol,12)

        do iline=1,nline

311       read(lunmag,*) rmoth,rmag,rcol,rplan,rcorn,x,y,z,bx,by,bz,rmat
          if (bx**2+by**2+bz**2.eq.0.0d0) goto 311

          imag=nint(rmag)
          icol=nint(rcol)
          iplan=nint(rplan)
          icorn=nint(rcorn)

          ncorn=abs(icorn)

          if (iline.eq.nline) then

            xpl(ncorn)=x
            ypl(ncorn)=y
            zpl(ncorn)=z

            iplano=iplan
            ncorno=ncorn
            rcolo=rcol

          endif !(iline.eq.nline

          if(iplan.ne.iplano.or.iline.eq.nline) then

            do i=1,ncorno
              if (
     &          xpl(i).lt.xplmin.or.
     &          xpl(i).gt.xplmax.or.
     &          ypl(i).lt.yplmin.or.
     &          ypl(i).gt.yplmax.or.
     &          zpl(i).lt.zplmin.or.
     &          zpl(i).gt.zplmax
     &          ) goto 81
            enddo

            do i=1,ncorno
              zplm(i)=-zpl(i)
            enddo

            call mshplt_set_line_width(rlwidth/5.)
            call mgset('PLCI',1.)
            call mpl(ncorno,xpl,ypl)

            xc=0.
            yc=0.
            zc=0.

            do i=1,ncorno-1
              xc=xc+xpl(i)
              yc=yc+ypl(i)
              zc=zc+zpl(i)
            enddo

            xc=xc/(ncorno-1)
            yc=yc/(ncorno-1)
            zc=zc/(ncorno-1)

            do i=1,ncorno
              xpl(i)=xpl(i)+(xc-xpl(i))*5.0*rlwidth
              ypl(i)=ypl(i)+(yc-ypl(i))*5.0*rlwidth
              zpl(i)=zpl(i)+(zc-zpl(i))*5.0*rlwidth
              zplm(i)=-zpl(i)
            enddo

            call mshplt_set_line_width(rlwidth/2.)
            call mgset('PLCI',rcolo)
            call mpl(ncorno,xpl,ypl)

81          continue

          endif !iplano

          xpl(ncorn)=x
          ypl(ncorn)=y
          zpl(ncorn)=z

          iplano=iplan
          ncorno=ncorn
          rcolo=rcol

        enddo !nline

        call mshplt_set_line_width(rlwidth*2.)
        do iline=1,ncwires
          rcolo=wire(9,iline)
          call mgset('PLCI',rcolo)
          xpl(1)=wire(3,iline)
          xpl(2)=wire(6,iline)
          ypl(1)=wire(4,iline)
          ypl(2)=wire(7,iline)
          call mpl(2,xpl,ypl)
        enddo
        call mshplt_set_line_width(rlwidth/2.)

      else !:if (theta.eq.0.0.and.phi.eq.0.0) then

        call mplfr3(xplmin,xplmax,-zplmax,-zplmin,yplmin,yplmax,theta,phi,'W')

        do iline=1,nline

31        read(lunmag,*) rmoth,rmag,rcol,rplan,rcorn,x,y,z,bx,by,bz,rmat
          if (bx**2+by**2+bz**2.eq.0.0d0) goto 31

          imag=nint(rmag)
          icol=nint(rcol)
          iplan=nint(rplan)
          icorn=nint(rcorn)

          ncorn=abs(icorn)

          if (iline.eq.nline) then

            xpl(ncorn)=x
            ypl(ncorn)=y
            zpl(ncorn)=z

            iplano=iplan
            ncorno=ncorn
            rcolo=rcol

          endif !(iline.eq.nline

          if(iplan.ne.iplano.or.iline.eq.nline) then

            do i=1,ncorno
              if (
     &          xpl(i).lt.xplmin.or.
     &          xpl(i).gt.xplmax.or.
     &          ypl(i).lt.yplmin.or.
     &          ypl(i).gt.yplmax.or.
     &          zpl(i).lt.zplmin.or.
     &          zpl(i).gt.zplmax
     &          ) goto 8
            enddo

            do i=1,ncorno
              zplm(i)=-zpl(i)
            enddo

            call mgset('PLCI',1.)
            call mpl3(ncorno,xpl,zplm,ypl)

            xc=0.
            yc=0.
            zc=0.

            do i=1,ncorno-1
              xc=xc+xpl(i)
              yc=yc+ypl(i)
              zc=zc+zpl(i)
            enddo

            xc=xc/(ncorno-1)
            yc=yc/(ncorno-1)
            zc=zc/(ncorno-1)

            do i=1,ncorno
              xpl(i)=xpl(i)+(xc-xpl(i))*0.015*rcolo
              ypl(i)=ypl(i)+(yc-ypl(i))*0.015*rcolo
              zpl(i)=zpl(i)+(zc-zpl(i))*0.015*rcolo
              zplm(i)=-zpl(i)
            enddo

            call mgset('PLCI',rcolo)
            call mpl3(ncorno,xpl,zplm,ypl)

8           continue

          endif !iplano

          xpl(ncorn)=x
          ypl(ncorn)=y
          zpl(ncorn)=z

          iplano=iplan
          ncorno=ncorn
          rcolo=rcol

        enddo !nline

        call mshplt_set_line_width(rlwidth*2.)
        do iline=1,ncwires
          rcolo=wire(9,iline)
          call mgset('PLCI',rcolo)
          xpl(1)=wire(3,iline)
          xpl(2)=wire(6,iline)
          ypl(1)=wire(4,iline)
          ypl(2)=wire(7,iline)
          zpl(1)=-wire(5,iline)
          zpl(2)=-wire(8,iline)
          call mpl3(2,xpl,zpl,ypl)
        enddo
        call mshplt_set_line_width(rlwidth/2.)

      endif !(theta.eq.0.0.and.phi.eq.0.0) then

c--- 3D }

c--- y vs z or z vs y {

      call mplfra(zplmin,zplmax,yplmin,yplmax,' ')
      call mplax('z (mm)', 'y (mm)')
      if (nbforcx*nbforcy*nbforcz.ne.0) call undumag_bpolypl2(forzpl,forypl,forcol,23)

      rewind(lunmag)
      read(lunmag,'(a)')cline
      iplano=1

      do iline=1,nline

51      read(lunmag,*) rmoth,rmag,rcol,rplan,rcorn,x,y,z,bx,by,bz,rmat
        if (bx**2+by**2+bz**2.eq.0.0d0) goto 51

        imag=nint(rmag)
        icol=nint(rcol)
        iplan=nint(rplan)
        icorn=nint(rcorn)

        ncorn=abs(icorn)

        if (iline.eq.nline) then

          xpl(ncorn)=x
          ypl(ncorn)=y
          zpl(ncorn)=z

          iplano=iplan
          ncorno=ncorn
          rcolo=rcol

        endif !(iline.eq.nline

        if(iplan.ne.iplano.or.iline.eq.nline) then

          do i=1,ncorno
            zplm(i)=-zpl(i)
          enddo

          call mshplt_set_line_width(rlwidth/5.)
          call mgset('PLCI',1.)
          call mpl(ncorno,zpl,ypl)

          xc=0.
          yc=0.
          zc=0.

          do i=1,ncorno-1
            xc=xc+xpl(i)
            yc=yc+ypl(i)
            zc=zc+zpl(i)
          enddo

          xc=xc/(ncorno-1)
          yc=yc/(ncorno-1)
          zc=zc/(ncorno-1)

          izero=0
          do i=1,ncorno
            if (
     &        abs(yc-ypl(i)).gt.1.0e-6 .and. abs(zc-zpl(i)).gt.1.0e-6
     &        ) izero=1
            xpl(i)=xpl(i)+(xc-xpl(i))*2.0*rlwidth
            ypl(i)=ypl(i)+(yc-ypl(i))*2.0*rlwidth
            zpl(i)=zpl(i)+(zc-zpl(i))*2.0*rlwidth
            zplm(i)=-zpl(i)
          enddo

          call mgset('PLCI',rcolo)

          if (izero.ne.0) call mpl(ncorno,zpl,ypl)

        endif !iplano

        xpl(ncorn)=x
        ypl(ncorn)=y
        zpl(ncorn)=z

        iplano=iplan
        ncorno=ncorn
        rcolo=rcol

      enddo !nline

      call mshplt_set_line_width(rlwidth*2.)
      do iline=1,ncwires
        rcolo=wire(9,iline)
        call mgset('PLCI',rcolo)
        xpl(1)=wire(3,iline)
        xpl(2)=wire(6,iline)
        ypl(1)=wire(4,iline)
        ypl(2)=wire(7,iline)
        zpl(1)=wire(5,iline)
        zpl(2)=wire(8,iline)
        call mpl(2,zpl,ypl)
      enddo
      call mshplt_set_line_width(rlwidth/2.)

      call mshplt_set_line_width(rlwidtho)

      if (nbforcx*nbforcy*nbforcz.ne.0) call undumag_bpolypl2(forzpl,forypl,forcol,23)

c--- y vs z or z vs y }

c--- top views of girder {
      call muwk(0,0)
      call mplzon(1,4,3,'S')

      do igird=1,2

c--- z vs x, y is vertical coordinate {
        call mplfra(xplmin,xplmax,zplmin,zplmax,' ')
        call mplax('x (mm)', 'z (mm)')

        if (nbforcx*nbforcy*nbforcz.ne.0)
     &    call undumag_bpolypl2(forxpl,forzpl,forcol,13)

        rewind(lunmag)
        read(lunmag,'(a)')cline
        if (nline.gt.0) then
61        read(lunmag,*) rmoth,rmag,rcol,rplan,rcorn,x,y,z,bx,by,bz,rmat
          if (bx**2+by**2+bz**2.eq.0.0d0) goto 61
          backspace(lunmag)
        endif
        imago=int(rmag)
        rmato=rmat
        iplano=1
        impl=0

        do iline=1,nline

71        read(lunmag,*) rmoth,rmag,rcol,rplan,rcorn,x,y,z,bx,by,bz,rmat
          if (bx**2+by**2+bz**2.eq.0.0d0) goto 71

          imag=nint(rmag)
          icol=nint(rcol)
          iplan=nint(rplan)
          icorn=nint(rcorn)

          ncorn=abs(icorn)

          if (iline.eq.nline) then

            xpl(ncorn)=x
            ypl(ncorn)=y
            zpl(ncorn)=z

            iplano=iplan
            ncorno=ncorn
            rcolo=rcol

          endif !(iline.eq.nline

          if (imag.ne.imago.or.iline.eq.nline) then

            xmc(1)=0.
            ymc(1)=0.
            zmc(1)=0.

            xmmx=-1.0e30
            xmmn= 1.0e30
            ymmx=-1.0e30
            ymmn= 1.0e30
            zmmx=-1.0e30
            zmmn= 1.0e30

            do i=1,impl-1
              xmc(1)=xmc(1)+xmpl(i)
              ymc(1)=ymc(1)+ympl(i)
              zmc(1)=zmc(1)+zmpl(i)
              if (xmpl(i).gt.xmmx) xmmx=xmpl(i)
              if (xmpl(i).lt.xmmn) xmmn=xmpl(i)
              if (ympl(i).gt.ymmx) ymmx=ympl(i)
              if (ympl(i).lt.ymmn) ymmn=ympl(i)
              if (zmpl(i).gt.zmmx) zmmx=zmpl(i)
              if (zmpl(i).lt.zmmn) zmmn=zmpl(i)
            enddo

            xmc(1)=xmc(1)/(impl-1)
            ymc(1)=ymc(1)/(impl-1)
            zmc(1)=zmc(1)/(impl-1)

            dx=xmmx-xmmn
            dy=ymmx-ymmn
            dz=zmmx-zmmn

            impl=0

          endif !imag.ne.imago

          impl=impl+1

          if (iline.eq.nline) then

            xpl(ncorn)=x
            ypl(ncorn)=y
            zpl(ncorn)=z

            xmpl(impl)=x
            ympl(impl)=y
            zmpl(impl)=z

            bxo=bx
            byo=by
            bzo=bz

            iplano=iplan
            ncorno=ncorn
            rcolo=rcol

          endif !(iline.eq.nline

          if(iplan.ne.iplano.or.iline.eq.nline) then

            xc=0.
            yc=0.
            zc=0.

            do i=1,ncorno-1
              xc=xc+xpl(i)
              yc=yc+ypl(i)
              zc=zc+zpl(i)
            enddo

            xc=xc/(ncorno-1)
            yc=yc/(ncorno-1)
            zc=zc/(ncorno-1)

            if (igird.eq.1.and.yc.ge.0.0) then

              do i=1,ncorno
                zplm(i)=-zpl(i)
              enddo

              call mgset('PLCI',1.)
              call mpl(ncorno,xpl,zpl)

              izero=0
              do i=1,ncorno
                if (
     &            abs(xc-xpl(i)).gt.1.0e-6 .and. abs(zc-zpl(i)).gt.1.0e-6
     &            ) izero=1
                xpl(i)=xpl(i)+(xc-xpl(i))*0.03*rcolo
                ypl(i)=ypl(i)+(yc-ypl(i))*0.03*rcolo
                zpl(i)=zpl(i)+(zc-zpl(i))*0.03*rcolo
                zplm(i)=-zpl(i)
              enddo

              call mgset('PLCI',rcolo)
              if (izero.ne.0) call mpl(ncorno,xpl,zpl)

              if (imag.ne.imago.or.iline.eq.nline) then

                bo=sqrt(bxo*bxo+byo*byo+bzo*bzo)

                if (abs(bxo).lt.bo*eps) bxo=0.0
                if (abs(byo).lt.bo*eps) byo=0.0
                if (abs(bzo).lt.bo*eps) bzo=0.0

                xplb(1)=xmc(1)-2.*bxo/bo*dx/6.
                xplb(2)=xmc(1)+2.*bxo/bo*dx/6.
                yplb(1)=ymc(1)-2.*byo/bo*dy/8.
                yplb(2)=ymc(1)+2.*byo/bo*dy/8.
                zplb(1)=zmc(1)-2.*bzo/bo*dz/5.
                zplb(2)=zmc(1)+2.*bzo/bo*dz/5.

                xplbo(1)=xplb(1)
                xplbo(2)=xplb(2)
                yplbo(1)=yplb(1)
                yplbo(2)=yplb(2)
                zplbo(1)=zplb(1)
                zplbo(2)=zplb(2)

                if (rmato.eq.1) call mpl(2,xplbo,zplbo)

                vn=sqrt((xplbo(2)-xplbo(1))**2+(zplbo(2)-zplbo(1))**2)

                if (vn.ne.0.0d0) then

                  vnx=(xplbo(2)-xplbo(1))/vn
                  vnz=(zplbo(2)-zplbo(1))/vn

                  xplb(1)=xplbo(2)+vnz*dx/10.0-vnx*dx/10.0
                  zplb(1)=zplbo(2)-vnx*dz/10.0-vnz*dz/10.0

                  !call mpl(2,xplb,zplb)

                  xplb(1)=xplbo(2)-vnz*dx/10.0-vnx*dx/10.0
                  zplb(1)=zplbo(2)+vnx*dz/10.0-vnz*dz/10.0

                  if (rmato.eq.1) call mpl(2,xplb,zplb)

                endif !vn

                if (byo.gt.1.e-9) then
                  call mgset('MTYP',rmtyp24)
                  call mgset('MSCF',circ0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                  call mgset('MTYP',rmtyp20)
                  call mgset('MSCF',dot0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                else if (byo.lt.0.0) then
                  call mgset('MTYP',rmtyp24)
                  call mgset('MSCF',circ0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                  call mgset('MTYP',rmtyp31)
                  call mgset('MSCF',dot0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                endif

              endif !imago

            else if (igird.eq.2.and.yc.le.0.0) then

              do i=1,ncorno
                zplm(i)=-zpl(i)
              enddo

              call mgset('PLCI',1.)
              call mpl(ncorno,xpl,zpl)

              izero=0
              do i=1,ncorno
                if (
     &            abs(xc-xpl(i)).gt.1.0e-6 .and. abs(zc-zpl(i)).gt.1.0e-6
     &            ) izero=1
                xpl(i)=xpl(i)+(xc-xpl(i))*0.03*rcolo
                ypl(i)=ypl(i)+(yc-ypl(i))*0.03*rcolo
                zpl(i)=zpl(i)+(zc-zpl(i))*0.03*rcolo
                zplm(i)=-zpl(i)
              enddo

              call mgset('PLCI',rcolo)
              if (izero.ne.0) call mpl(ncorno,xpl,zpl)

              if (imag.ne.imago.or.iline.eq.nline) then

                bo=sqrt(bxo*bxo+byo*byo+bzo*bzo)
                if (abs(bxo).lt.bo*eps) bxo=0.0
                if (abs(byo).lt.bo*eps) byo=0.0
                if (abs(bzo).lt.bo*eps) bzo=0.0

                xplb(1)=xmc(1)-2.*bxo/bo*dx/6.
                xplb(2)=xmc(1)+2.*bxo/bo*dx/6.
                yplb(1)=ymc(1)-2.*byo/bo*dy/8.
                yplb(2)=ymc(1)+2.*byo/bo*dy/8.
                zplb(1)=zmc(1)-2.*bzo/bo*dz/5.
                zplb(2)=zmc(1)+2.*bzo/bo*dz/5.

                xplbo(1)=xplb(1)
                xplbo(2)=xplb(2)
                yplbo(1)=yplb(1)
                yplbo(2)=yplb(2)
                zplbo(1)=zplb(1)
                zplbo(2)=zplb(2)

                if (rmato.eq.1) call mpl(2,xplbo,zplbo)

                vn=sqrt((xplbo(2)-xplbo(1))**2+(zplbo(2)-zplbo(1))**2)

                if (vn.ne.0.0d0) then

                  vnx=(xplbo(2)-xplbo(1))/vn
                  vnz=(zplbo(2)-zplbo(1))/vn

                  xplb(1)=xplbo(2)+vnz*dx/10.0-vnx*dx/10.0
                  zplb(1)=zplbo(2)-vnx*dz/10.0-vnz*dz/10.0

                  if (rmato.eq.1) call mpl(2,xplb,zplb)

                  xplb(1)=xplbo(2)-vnz*dx/10.0-vnx*dx/10.0
                  zplb(1)=zplbo(2)+vnx*dz/10.0-vnz*dz/10.0

                  if (rmato.eq.1)call mpl(2,xplb,zplb)

                endif !vn

                if (byo.gt.1.e-9) then
                  call mgset('MTYP',rmtyp24)
                  call mgset('MSCF',circ0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                  call mgset('MTYP',rmtyp20)
                  call mgset('MSCF',dot0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                else if (byo.lt.0.0) then
                  call mgset('MTYP',rmtyp24)
                  call mgset('MSCF',circ0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                  call mgset('MTYP',rmtyp31)
                  call mgset('MSCF',dot0*pscal/5.)
                  call mpm(1,xmc(1),zmc(1))
                endif

              endif !imago

            endif !yc

          endif !iplano

          xpl(ncorn)=x
          ypl(ncorn)=y
          zpl(ncorn)=z

          xmpl(impl)=x
          ympl(impl)=y
          zmpl(impl)=z

          bxo=bx
          byo=by
          bzo=bz

          iplano=iplan
          ncorno=ncorn
          rcolo=rcol
          imago=imag
          rmato=rmat

        enddo !nline

        if (nbforcx*nbforcy*nbforcz.ne.0) call undumag_bpolypl2(forxpl,forzpl,forcol,13)

        if (igird.eq.1) then
          call mshplt_set_line_width(rlwidth*2.)
          do iline=1,ncwires
            rcolo=wire(9,iline)
            call mgset('PLCI',rcolo)
            xpl(1)=wire(3,iline)
            xpl(2)=wire(6,iline)
            ypl(1)=wire(4,iline)
            ypl(2)=wire(7,iline)
            zpl(1)=wire(5,iline)
            zpl(2)=wire(8,iline)
            if (ypl(1).gt.0.0.or.ypl(2).gt.0.0) call mpl(2,xpl,zpl)
          enddo
          call mshplt_set_line_width(rlwidth/2.)
        else
          call mshplt_set_line_width(rlwidth*2.)
          do iline=1,ncwires
            rcolo=wire(9,iline)
            call mgset('PLCI',rcolo)
            xpl(1)=wire(3,iline)
            xpl(2)=wire(6,iline)
            ypl(1)=wire(4,iline)
            ypl(2)=wire(7,iline)
            zpl(1)=wire(5,iline)
            zpl(2)=wire(8,iline)
            if (ypl(1).lt.0.0.or.ypl(2).lt.0.0) call mpl(2,xpl,zpl)
          enddo
          call mshplt_set_line_width(rlwidth/2.)
        endif

      enddo !igird

      call muwk(0,0)

c--- y vs x }

c--- top views of girder}

c--- 3D, top and side views }

9999  close (lunmag)

      if (iallo.eq.1) then
        deallocate(xpl)
        deallocate(ypl)
        deallocate(zpl)
        deallocate(zplm)
        deallocate(xmpl)
        deallocate(ympl)
        deallocate(zmpl)
      endif

c      call mgmeta(0,0)
c      call mplend

      call mshplt_end

      write(lun6,*)"Done"
      write(lun6,*)

      return
      end
