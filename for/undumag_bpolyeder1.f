*CMZ :  2.02/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.00/00 11/04/2018  11.32.22  by  Michael Scheer
*CMZ :  1.25/00 16/03/2018  13.34.14  by  Michael Scheer
*CMZ :  1.22/01 20/07/2017  14.46.06  by  Michael Scheer
*CMZ :  1.20/02 22/06/2017  16.10.27  by  Michael Scheer
*CMZ :  1.15/11 24/04/2017  16.58.01  by  Michael Scheer
*CMZ :  1.15/10 07/04/2017  15.04.31  by  Michael Scheer
*CMZ :  1.15/07 05/04/2017  08.32.51  by  Michael Scheer
*CMZ :  1.15/05 03/04/2017  14.06.28  by  Michael Scheer
*CMZ :  1.15/02 02/04/2017  08.06.45  by  Michael Scheer
*CMZ :  1.15/01 28/03/2017  13.54.17  by  Michael Scheer
*CMZ :  1.14/00 17/03/2017  14.15.41  by  Michael Scheer
*CMZ :  1.13/03 16/03/2017  16.49.26  by  Michael Scheer
*CMZ :  1.13/02 08/03/2017  17.13.43  by  Michael Scheer
*CMZ :  1.13/01 08/03/2017  14.04.22  by  Michael Scheer
*CMZ :  1.11/03 16/01/2017  12.22.22  by  Michael Scheer
*CMZ :  1.11/00 13/12/2016  10.43.26  by  Michael Scheer
*CMZ :  1.10/02 30/11/2016  16.46.22  by  Michael Scheer
*CMZ :  1.10/01 18/11/2016  15.02.58  by  Michael Scheer
*CMZ :  1.04/01 14/09/2016  15.11.09  by  Michael Scheer
*CMZ :  0.00/13 08/08/2016  09.34.27  by  Michael Scheer
*CMZ :  0.00/06 16/06/2016  14.14.37  by  Michael Scheer
*CMZ :  0.00/04 13/05/2016  12.57.31  by  Michael Scheer
*CMZ :  0.00/02 29/04/2016  09.17.27  by  Michael Scheer
*CMZ :  0.00/00 20/04/2016  12.41.34  by  Michael Scheer
*CMZ :  1.17/11 06/04/2016  09.11.57  by  Michael Scheer
*CMZ :  1.17/07 03/04/2016  19.34.26  by  Michael Scheer
*CMZ :  1.17/06 01/04/2016  13.01.49  by  Michael Scheer
*CMZ :  1.17/03 21/03/2016  12.17.41  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003
      subroutine undumag_bpolyeder1(imag,xin,yin,zin,bxout,byout,bzout,ifail)
c
c      Calculation of magnetic field of polyhedron according to
c      Oleb Chubar, Pascal Elleaume and Joel Chavanne
c      J. Synchrotron Rad. (1998) 5, 481-484
c
c Paper contains an error: The rotation matrix is wrong, since for nz=-1 the
c                          determinant is -1, which yields to errors??.


*KEEP,bpolyederf90u.

      use bpolyederf90m

*KEND.
      use undumagf90m
      use commandlinef90m

      implicit none
*KEEP,seqdebug.
      integer iseqdebug
      common/seqdebugc/iseqdebug
*KEND.
      double precision xin,yin,zin
     &  ,bxout,byout,bzout
      double precision r1(3),r2(3),dlab(3),blab(3)
      double precision ts(3,3),tsinv(3,3),bplan(3),bcvn,vnormlab(3)
      double precision xx,yy,zz,xxrot,yyrot,zzrot,xx00,xxsh
      double precision a,b,z,qx,qy,qz,qxp,qyp,qzp,qxm,qym,qzm,
     &  pi4inv,reverse,tiny2,
     &  bxm,bym,bzm,bxp,byp,bzp,xxm,yym,zzm,xxp,yyp,zzp,xx0,yy0,zz0,
     &  rr0,rrm
      double precision q(3,3),vmagrot(3),vmaglab(3),h(3),
     &  xr(2),yr(2),zr(2),dum,dume,pow,sqr

      double precision xmin,xmax,ymin,ymax,zmin,zmax,bx,by,bz

      parameter (pi4inv=0.0795774715459477d0)

      integer ical
      integer iout,itiny,iwtiny,jtiny
      integer imag,iplan,icorn,i,j,k,ip2,kwarni,kwarn
      integer nx,ny,nz,ifail,ifailm,ifailp,ishim,ishima,iimag,nmag1,nmag2,
     &  istat

      data ical/0/

      tiny2=tiny*tiny

      ifail=0
      ifailm=0
      ifailp=0

c      kwarncom=0

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

c      if (
c     &    xin.ge.outbox(1,1).and.xin.le.outbox(2,1)
c     &    .and.
c     &    yin.ge.outbox(1,2).and.yin.le.outbox(2,2)
c     &    .and.
c     &    zin.ge.outbox(1,3).and.zin.le.outbox(2,3)
c     &    ) return
			
      ifail=0
      kwarni=0

c calculate field at (xin,yin,zin)

      if (imag.eq.0) then
        write(lun6,*)'*** Error 1: undumag_bpolyeder1 called with imag=0! '
        stop
      endif !magmag.le.0

      if (bpebc(17,imag).lt.0.0d0) return

      xx=xin*1000.0d0
      xx00=xx
      yy=yin*1000.0d0
      zz=zin*1000.0d0

1     continue
      xxp=xx
      yyp=yy
      zzp=zz

      xx0=xx
      yy0=yy
      zz0=zz

      xxm=xx
      yym=yy
      zzm=zz

      itiny=0
      jtiny=0
      iwtiny=0

      q=0.0d0

      if (abs(xx00-bpebc(1,imag)).gt.window) goto 9999

      !non-zero magnetization and no virgin shim
      if (
     &    bpebc(7,imag).ne.0.0d0
     &    .and.bpebc(7,imag).ne.9999.0d0
     &    ) then

        if(bpebc(8,imag).eq.1) then !not a rectangular magnet

c check, if we are inside of magnet; we assume convex shape

          iout=-1
          inside=imag

          do iplan=1,ibpeplan(imag)

            dlab(1)=xx-bpemag(1,1,iplan,imag)
            dlab(2)=yy-bpemag(2,1,iplan,imag)
            dlab(3)=zz-bpemag(3,1,iplan,imag)

            vnormlab(1)=bpetm(1,8,iplan,imag)
            vnormlab(2)=bpetm(2,8,iplan,imag)
            vnormlab(3)=bpetm(3,8,iplan,imag)

            if( dlab(1)*vnormlab(1)+dlab(2)*vnormlab(2)+
     &          dlab(3)*vnormlab(3).gt.0.d0) then
              iout=1
              inside=0
              goto 97
            endif

          enddo !iplan

          if (iout.eq.-1) then
            kinside=imag
          endif !iout

97        continue

          do iplan=1,ibpeplan(imag)

            bcvn=-bpetm(1,7,iplan,imag)*pi4inv

            bplan(1)=0.0d0
            bplan(2)=0.0d0
            bplan(3)=0.0d0

c transform everything to the nz=(0,0,1) system

            if (bcvn.eq.0.0d0) cycle

            if (ibpecorn(iplan,imag).gt.0) then

              ts(1:3,1:3)=bpetm(1:3,1:3,iplan,imag)
              tsinv(1:3,1:3)=bpetm(1:3,4:6,iplan,imag)

              xxrot=ts(1,1)*xx+ts(1,2)*yy+ts(1,3)*zz
              yyrot=ts(2,1)*xx+ts(2,2)*yy+ts(2,3)*zz
              zzrot=ts(3,1)*xx+ts(3,2)*yy+ts(3,3)*zz

              do icorn=1,ibpecorn(iplan,imag)-1

                ip2=icorn+1

                r1(1)=bperot(1,icorn,iplan,imag)-xxrot
                r1(2)=bperot(2,icorn,iplan,imag)-yyrot
                r1(3)=bperot(3,icorn,iplan,imag)-zzrot

                r2(1)=bperot(1,ip2,iplan,imag)-xxrot
                r2(2)=bperot(2,ip2,iplan,imag)-yyrot
                r2(3)=bperot(3,ip2,iplan,imag)-zzrot

                if (abs(r1(1)-r2(1)).gt.tiny) then

                  a=(r2(2)-r1(2))/(r2(1)-r1(1))
                  b=r1(2)-a*r1(1)

                  if (abs(a).lt.tiny2) then
                    a=0.0d0
                    b=r1(2)
                  endif

                  z=r1(3)

                  call undumag_bpeq(r1(1),r2(1),a,b,z,qx,qy,qz,
     &              tiny,reverse,kwarn)

                  if (kwarn.eq.1) kwarn=0

                  if (kwarn.ne.0) bpebc(16,imag)=-kwarn

                  if (kwarn.ne.0) kwarni=kwarn

                  if (
     &                qx.ne.qx.or.qy.ne.qy.or.qz.ne.qz
     &                .or.
     &                (kwarn.ne.0.and.kwarn.ne.6)) then
                    ifail=2
                    goto 799
                  endif !qx,qy,qz,kwarn

                  bplan(1)=bplan(1)-qx*bcvn
                  bplan(2)=bplan(2)-qy*bcvn
                  bplan(3)=bplan(3)-qz*bcvn

                endif ! r1(1)
              enddo !icorn=1,ncorn

              blab(1)=tsinv(1,1)*bplan(1)+tsinv(1,2)*bplan(2)+tsinv(1,3)*bplan(3)
              blab(2)=tsinv(2,1)*bplan(1)+tsinv(2,2)*bplan(2)+tsinv(2,3)*bplan(3)
              blab(3)=tsinv(3,1)*bplan(1)+tsinv(3,2)*bplan(2)+tsinv(3,3)*bplan(3)

              if (
     &            blab(1).ne.blab(1)
     &            .or.
     &            blab(2).ne.blab(2)
     &            .or.
     &            blab(3).ne.blab(3)
     &            ) then
                write(lun6,*)"*** Error 3 in undumag_bpolyeder1: blab is not a number (NaN) ***"
                write(lun6,*)
     &            "imag,iplan,xin,yin,zin:",imag,iplan,xin,yin,zin
                write(lun6,*)"blab",blab
                write(lun6,*)"tsinv",tsinv
                stop
              endif

              bxout=bxout+blab(1)
              byout=byout+blab(2)
              bzout=bzout+blab(3)

            endif !ncorn

          enddo ! iplan=1,nplan

          if (iout.eq.-1) then
            kinside=imag
          endif !iout

        else !bpebc(8,imag) .eq. 1

c rectangular or cylindrical magnet
c check, if we are inside of magnet; we assume convex shape

          iout=-1
          inside=imag

          do iplan=1,ibpeplan(imag)

            dlab(1)=xx-bpemag(1,1,iplan,imag)
            dlab(2)=yy-bpemag(2,1,iplan,imag)
            dlab(3)=zz-bpemag(3,1,iplan,imag)

            vnormlab(1)=bpetm(1,8,iplan,imag)
            vnormlab(2)=bpetm(2,8,iplan,imag)
            vnormlab(3)=bpetm(3,8,iplan,imag)

            if( dlab(1)*vnormlab(1)+dlab(2)*vnormlab(2)+
     &          dlab(3)*vnormlab(3).gt.0.d0) then
              iout=1
              inside=0
              goto 911
            endif

          enddo !iplan

          if (iout.eq.-1) then
            kinside=imag
          endif !iout

911       continue

          vmaglab(1)=bpebc(4,imag)
          vmaglab(2)=bpebc(5,imag)
          vmaglab(3)=bpebc(6,imag)

c transform everything to the nz=(0,0,1) system and rotate it parallel to x-axis

          ts(1:3,1:3)=bpetm(1:3,1:3,1,imag)
          tsinv(1:3,1:3)=bpetm(1:3,4:6,1,imag)

          xxrot=ts(1,1)*xx+ts(1,2)*yy+ts(1,3)*zz
          yyrot=ts(2,1)*xx+ts(2,2)*yy+ts(2,3)*zz
          zzrot=ts(3,1)*xx+ts(3,2)*yy+ts(3,3)*zz

          vmagrot(1)=
     &      ts(1,1)*vmaglab(1)+ts(1,2)*vmaglab(2)+ts(1,3)*vmaglab(3)
          vmagrot(2)=
     &      ts(2,1)*vmaglab(1)+ts(2,2)*vmaglab(2)+ts(2,3)*vmaglab(3)
          vmagrot(3)=
     &      ts(3,1)*vmaglab(1)+ts(3,2)*vmaglab(2)+ts(3,3)*vmaglab(3)

          xr(1)=bperot(1,1,1,imag)-xxrot
          xr(2)=bperot(1,2,1,imag)-xxrot
          yr(1)=bperot(2,1,1,imag)-yyrot
          yr(2)=bperot(2,3,1,imag)-yyrot
          zr(1)=bperot(3,1,1,imag)-zzrot
          zr(2)=bperot(3,1,3,imag)-zzrot

          if (abs(xr(2)-xr(1)).lt.tiny) then
c            write(lun6,*)'*** Error 4 in undumag_bpolyeder1: abs(xr(2)-xr(1)).lt.tiny'
c            write(lun6,*)imag,xx,yy,zz
c            stop
            kwarni=7
          endif

          if (abs(yr(2)-yr(1)).lt.tiny) then
            write(lun6,*)'*** Error 5 in undumag_bpolyeder1 abs(yr(2)-yr(1)).lt.tiny'
            write(lun6,*)imag,xx,yy,zz
            stop
          endif

          if (abs(zr(2)-zr(1)).lt.tiny) then
c            write(lun6,*)'*** Error 6 in undumag_bpolyeder1 abs(zr(2)-zr(1)).lt.tiny'
c            write(lun6,*)imag,xx,yy,zz
c            stop
            kwarni=8
          endif

          q(1,1)=0.0d0
          q(2,2)=0.0d0
          q(3,3)=0.0d0

          q(1,2)=1.0d0
          q(1,3)=1.0d0
          q(2,3)=1.0d0

          if (xr(1).eq.0.0d0) xr(1)=1.0d-15
          if (xr(2).eq.0.0d0) xr(2)=1.0d-15
          if (yr(1).eq.0.0d0) yr(1)=1.0d-15
          if (yr(2).eq.0.0d0) yr(2)=1.0d-15
          if (zr(1).eq.0.0d0) zr(1)=1.0d-15
          if (zr(2).eq.0.0d0) zr(2)=1.0d-15

          do i=1,2
            do j=1,2
              do k=1,2

                pow=dble((-1)**(i+j+k+1))
                sqr=sqrt(xr(i)**2+yr(j)**2+zr(k)**2)

                if (sqr.eq.0.0d0) then
                  write(lun6,*)"*** Error 7 in undumag_bpolyeder1: sqrt 0 "
                  stop
                endif

                q(1,1)=q(1,1)+pow*
c     &            (-1)**(i+j+k+1)*
     &            atan(
     &            yr(j)/xr(i)*zr(k)/sqr
c     &            sqrt(xr(i)**2+yr(j)**2+zr(k)**2)
     &            )

                sqr=sqrt(yr(i)**2+xr(j)**2+zr(k)**2)

                if (sqr.eq.0.0d0) then
                  write(lun6,*) "*** Error 8 in undumag_bpolyeder1: sqr 0"
                  stop
                endif

                q(2,2)=q(2,2)+pow*
c     &            (-1)**(i+j+k+1)*
     &            atan(
     &            xr(j)/yr(i)*zr(k)/sqr
c     &            sqrt(yr(i)**2+xr(j)**2+zr(k)**2)
     &            )

                sqr=sqrt(zr(i)**2+yr(j)**2+xr(k)**2)

                if (sqr.eq.0.0d0) then
                  write(lun6,*)"*** Error 9 in undumag_bpolyeder1: sqr 0"
                  stop
                endif

                q(3,3)=q(3,3)+pow*
c     &            (-1)**(i+j+k+1)*
     &            atan(
     &            yr(j)/zr(i)*xr(k)/sqr
c     &            sqrt(zr(i)**2+yr(j)**2+xr(k)**2)
     &            )

                dum=zr(k)+sqrt(xr(i)**2+yr(j)**2+zr(k)**2)
                dume=(-1.0d0)**(i+j+k)

                if (dum.ne.0.0d0) then
                  if (dume.gt.0.0d0) then
                    q(1,2)=q(1,2)*dum
                  else
                    q(1,2)=q(1,2)/dum
                  endif
                endif

                dum=yr(k)+sqrt(xr(i)**2+zr(j)**2+yr(k)**2)

                if (dum.ne.0.0d0) then
                  if (dume.gt.0.0d0) then
                    q(1,3)=q(1,3)*dum
                  else
                    q(1,3)=q(1,3)/dum
                  endif
                endif

                dum=xr(k)+sqrt(zr(i)**2+yr(j)**2+xr(k)**2)

                if (dum.ne.0.0d0) then
                  if (dume.gt.0.0d0) then
                    q(2,3)=q(2,3)*dum
                  else
                    q(2,3)=q(2,3)/dum
                  endif
                endif

              enddo !k
            enddo !j
          enddo !i

          q(1,2)=log(q(1,2))
          q(1,3)=log(q(1,3))
          q(2,3)=log(q(2,3))

          q(2,1)=q(1,2)
          q(3,1)=q(1,3)
          q(3,2)=q(2,3)

          h(1)=
     &      -(q(1,1)*vmagrot(1)+q(1,2)*vmagrot(2)+q(1,3)*vmagrot(3))*
     &      pi4inv
          h(2)=
     &      -(q(2,1)*vmagrot(1)+q(2,2)*vmagrot(2)+q(2,3)*vmagrot(3))*
     &      pi4inv
          h(3)=
     &      -(q(3,1)*vmagrot(1)+q(3,2)*vmagrot(2)+q(3,3)*vmagrot(3))*
     &      pi4inv

          bplan(1)=h(1)
          bplan(2)=h(2)
          bplan(3)=h(3)

          if (
     &        bplan(1).ne.bplan(1)
     &        .or.
     &        bplan(2).ne.bplan(2)
     &        .or.
     &        bplan(3).ne.bplan(3)
     &        ) then
            write(lun6,*)"*** Error 10 in undumag_bpolyeder1: bplan is not a number (NaN) ***"
            write(lun6,*)
     &        "imag,xin,yin,zin:",imag,xin,yin,zin
            stop
          endif

          blab(1)=tsinv(1,1)*bplan(1)+tsinv(1,2)*bplan(2)+tsinv(1,3)*bplan(3)
          blab(2)=tsinv(2,1)*bplan(1)+tsinv(2,2)*bplan(2)+tsinv(2,3)*bplan(3)
          blab(3)=tsinv(3,1)*bplan(1)+tsinv(3,2)*bplan(2)+tsinv(3,3)*bplan(3)

          if (
     &        blab(1).ne.blab(1)
     &        .or.
     &        blab(2).ne.blab(2)
     &        .or.
     &        blab(3).ne.blab(3)
     &        ) then
            write(lun6,*)"*** Error 11 in undumag_bpolyeder1: blab is not a number (NaN) ***"
            write(lun6,*)
     &        "imag,xin,yin,zin:",imag,xin,yin,zin
            stop
          endif

          bxout=bxout+blab(1)
          byout=byout+blab(2)
          bzout=bzout+blab(3)

          if (iout.eq.-1) then
            kinside=imag
          endif !iout

        endif !(bpebc(8,imag).eq.1)

      endif !non-zero magnetization


9999  continue

      if (kwarni.ne.0) goto 799

      goto 787

799   continue

      call undumag_bpolyeder11(imag,xin-1.0d0*corrtiny,yin-1.0d0*corrtiny,zin-1.0d0*corrtiny,
     &  bxm,bym,bzm,ifailm)
      call undumag_bpolyeder11(imag,xin+1.0d0*corrtiny,yin+1.0d0*corrtiny,zin+1.0d0*corrtiny,
     &  bxp,byp,bzp,ifailp)

      if (ifailm.eq.0.and.ifailp.eq.0) then
        bx=(bxm+bxp)/2.0d0
        by=(bym+byp)/2.0d0
        bz=(bzm+bzp)/2.0d0
c          write(lun6,*)"Recovered, bx,by,bz:",sngl(bx),sngl(by),sngl(bz)
c          write(lun6,*)"Uncertainty:",sngl(bxout-bx),sngl(byout-by),sngl(bzout-bz)
        bxout=bx
        byout=by
        bzout=bz
        ifail=-kwarn
      else if (ifailm.eq.0) then
        bxout=bxm
        byout=bym
        bzout=bzm
        ifail=kwarn+10*ifailm
      else if (ifailp.eq.0) then
        bxout=bxp
        byout=byp
        bzout=bzp
        ifail=kwarn+100*ifailp
      endif

c      write(lun6,*)

      if (ifailm.eq.6.or.ifailp.eq.6) then
          write(lun6,*)
          write(lun6,*)"2pi jump: mag, x,y,z,bx,by,bz:",imag,xin*1000,yin*1000,zin*1000,
     &      bxout,byout,bzout
c        else
c          write(lun6,*)"--- Warning in undumag_bpolyeder1:", kwarni
c          print '(a,i7,6e12.4)',"mag, x,y,z,bx,by,bz:",imag,xin*1000,yin*1000,zin*1000,
c     &      bxout,byout,bzout
      endif

787   continue

c      if (ncwires+nrace.gt.0) then
c        call undumag_bcoils(xin,yin,zin,bx,by,bz,istat)
c        bxout=bxout+bx
c        byout=byout+by
c        bzout=bzout+bz
c        if (istat.ne.0) ifail=ifail+1000
c      endif

      return
      end
