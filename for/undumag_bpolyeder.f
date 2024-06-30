*CMZ :          30/06/2024  15.40.49  by  Michael Scheer
*CMZ :  2.05/02 02/11/2023  14.14.41  by  Michael Scheer
*CMZ :  2.03/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/00 26/10/2020  14.56.48  by  Michael Scheer
*CMZ :  2.01/08 13/08/2020  12.32.35  by  Michael Scheer
*CMZ :  2.01/03 15/07/2019  15.03.51  by  Michael Scheer
*CMZ :  2.01/02 27/04/2018  16.29.19  by  Michael Scheer
*CMZ :  1.25/00 16/03/2018  14.11.34  by  Michael Scheer
*CMZ :  1.23/03 19/09/2017  19.25.01  by  Michael Scheer
*CMZ :  1.23/02 30/08/2017  13.27.12  by  Michael Scheer
*CMZ :  1.22/02 31/07/2017  10.32.51  by  Michael Scheer
*CMZ :  1.22/01 20/07/2017  14.46.06  by  Michael Scheer
*CMZ :  1.22/00 05/07/2017  09.55.55  by  Michael Scheer
*CMZ :  1.20/03 29/06/2017  09.17.17  by  Michael Scheer
*CMZ :  1.20/01 22/06/2017  13.26.26  by  Michael Scheer
*CMZ :  1.20/00 22/06/2017  11.26.04  by  Michael Scheer
*CMZ :  1.15/11 24/04/2017  16.58.30  by  Michael Scheer
*CMZ :  1.15/10 12/04/2017  14.53.10  by  Michael Scheer
*CMZ :  1.15/04 03/04/2017  12.30.25  by  Michael Scheer
*CMZ :  1.15/03 03/04/2017  10.59.22  by  Michael Scheer
*CMZ :  1.15/02 02/04/2017  07.35.42  by  Michael Scheer
*CMZ :  1.15/01 28/03/2017  13.53.21  by  Michael Scheer
*CMZ :  1.13/01 08/03/2017  16.31.38  by  Michael Scheer
*CMZ :  1.11/03 16/01/2017  12.22.22  by  Michael Scheer
*CMZ :  1.10/02 24/11/2016  09.47.59  by  Michael Scheer
*CMZ :  1.10/01 18/11/2016  15.02.58  by  Michael Scheer
*CMZ :  1.07/00 23/09/2016  09.19.06  by  Michael Scheer
*CMZ :  1.04/01 14/09/2016  15.10.51  by  Michael Scheer
*CMZ :  1.00/00 19/08/2016  18.27.23  by  Michael Scheer
*CMZ :  0.00/13 28/07/2016  16.09.29  by  Michael Scheer
*CMZ :  0.00/09 06/07/2016  08.42.18  by  Michael Scheer
*CMZ :  0.00/06 16/06/2016  14.14.37  by  Michael Scheer
*CMZ :  0.00/04 13/05/2016  13.18.24  by  Michael Scheer
*CMZ :  0.00/02 29/04/2016  09.17.13  by  Michael Scheer
*CMZ :  0.00/01 25/04/2016  16.03.15  by  Michael Scheer
*CMZ :  0.00/00 20/04/2016  12.41.34  by  Michael Scheer
*CMZ :  1.17/14 13/04/2016  09.46.51  by  Michael Scheer
*CMZ :  1.17/11 05/04/2016  13.27.16  by  Michael Scheer
*CMZ :  1.17/08 04/04/2016  08.57.43  by  Michael Scheer
*CMZ :  1.17/07 04/04/2016  08.31.31  by  Michael Scheer
*CMZ :  1.17/06 01/04/2016  13.53.25  by  Michael Scheer
*CMZ :  1.17/05 27/03/2016  10.43.50  by  Michael Scheer
*CMZ :  1.17/03 21/03/2016  18.38.48  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003
      subroutine undumag_bpolyeder(xin,yin,zin,bxout,byout,bzout,ifail)
c
c      Calculation of magnetic field of polyhedron according to
c      Oleb Chubar, Pascal Elleaume and Joel Chavanne
c      J. Synchrotron Rad. (1998) 5, 481-484
c
c Paper contains an error: The rotation matrix is wrong, since for nz=-1 the
c                          determinant is -1, which yields to errors??.
c Private notice:
c Einige Terme sind unklar, Notizen finden sich im Ordner RADIA/POLYMAG
c Siehe auch Notebooks: rec_int.nb, qx_rect.nb etc.
c oder Reduce olegqz.red, qxqyqz.red, rec_int.red etc.

      use omp_lib
      use bpolyederf90m
      use undumagf90m
      use commandlinef90m

      implicit none
*KEEP,DEBUGUTIL.
      double precision x_debug,y_debug,z_debug,a_debug(100)
      integer i_debug,k_debug
      character(64) c64_debug

      common/c_debug/x_debug,y_debug,z_debug,a_debug,i_debug,k_debug,
     &  c64_debug
*KEEP,seqdebug.
      integer iseqdebug
      common/seqdebugc/iseqdebug
*KEND.

      double precision xin,yin,zin,bxout,byout,bzout
      double precision r1(3),r2(3),dlab(3),blab(3)
      double precision ts(3,3),tsinv(3,3),bplan(3),bcvn,vnormlab(3)
      double precision xx,yy,zz,xxrot,yyrot,zzrot
      double precision a,b,z,qx,qy,qz,
     &  pi4inv,reverse,tiny2,
     &  bxm,bym,bzm,bxp,byp,bzp
      double precision q(3,3),vmagrot(3),vmaglab(3),h(3),
     &  xr(2),yr(2),zr(2),dum,dume,bo(3,nthreadp)

c      double precision xmin,xmax,ymin,ymax,zmin,zmax,bx,by,bz

      parameter (pi4inv=0.0795774715459477d0)

      integer ical,ifound
      integer itiny,iwtiny,jtiny
      integer imag,iplan,ncorn,icorn,i,j,k,ip2,kwarn,kwarni,ic,moth
      integer ifailin,ifail,ifailm,ifailp,iout,linside,
     &  kfail(nthreadp),kinsidelocal(nthreadp)

      integer nmaxth,ith

      save bo,nmaxth,ith,ical,kinsidelocal

      data ical/0/

      kwarncom=0

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      tiny2=tiny*tiny

      ifailin=ifail
c      if (ifailin.eq.-1) then
c        print*,"undumag_bpolyeder: ifailin:",ifailin
c      endif
c      if (ifailin.ne.0) then
c        print*,"undumag_bpolyeder: kinside:",kinside
c      endif
      ifail=0
      ifailm=0
      ifailp=0

c calculate field at (xin,yin,zin)

      if (magmag.lt.0) then
        return
      endif !magmag.le.0

c      if (xin.eq.0.0d0) return
      xx=xin*1000.0d0
c      if (abs(xx+15.6).lt.0.05) then
c        iseqdebug=1
c      else
c        iseqdebug=0
c      endif
      yy=yin*1000.0d0
      zz=zin*1000.0d0

      itiny=0
      jtiny=0
      iwtiny=0

      if (ical.eq.0) then
        nmaxth=1
        ith=1
        nmaxth=nthreads
        nmaxth=OMP_GET_MAX_THREADS()
        if (nthreads.gt.0) nmaxth=min(nmaxth,nthreads,nthreadp)
c        write(lun6,*)"Number of CPU cores used:",nmaxth
        bo=0.0d0
        ical=1
      endif

      bo=0.0d0
      kinsidelocal=kinside
      kwarni=0
      kwarn=0
      kfail=0

      iseqdebug=0

!$OMP PARALLEL NUM_THREADS(nmaxth) DEFAULT(PRIVATE)
!$OMP& SHARED(i_debug,kfail,iseqdebug,kinsidelocal,ical,bpetm,window,bpebc,bpemag,ibpeplan,ibpecorn,bperot,bo,iwarnbound,nwarnbound)
!$OMP& FIRSTPRIVATE(nmag,itiny,jtiny,iwtiny,tiny,xin,yin,zin,xx,yy,zz,kwarni,kwarn,lun6)
      ith=OMP_GET_THREAD_NUM()+1
      bo(1:3,ith)=0.0d0
!$OMP DO
      do imag=1,nmag

        if (bpebc(17,imag).lt.0.0d0) then
          cycle
        endif

        moth=nint(bpebc(15,imag))

        bcvn=0.0d0
c        if(iseqdebug.eq.-8) print*,"1:",ith,imag,kinsidelocal(ith)

        if (abs(xx-bpebc(1,imag)).le.window) then

          !non-zero magnetization and no virgin shim
          if (bpebc(7,imag).ne.0.0d0.and.bpebc(7,imag).ne.9999.) then

            if(bpebc(8,imag).eq.1) then !not rectangular nor cylindrical magnet

c check, if we are inside of magnet; we assume convex shape
              if (kinsidelocal(ith).ne.-1) then

                iout=-1

                do iplan=1,ibpeplan(imag)

                  dlab(1)=xx-bpemag(1,1,iplan,imag)
                  dlab(2)=yy-bpemag(2,1,iplan,imag)
                  dlab(3)=zz-bpemag(3,1,iplan,imag)

                  vnormlab(1)=bpetm(1,8,iplan,imag)
                  vnormlab(2)=bpetm(2,8,iplan,imag)
                  vnormlab(3)=bpetm(3,8,iplan,imag)

                  if( dlab(1)*vnormlab(1)+dlab(2)*vnormlab(2)+
     &                dlab(3)*vnormlab(3).gt.0.0d0) then
                    iout=1
                    goto 97
                  endif

                enddo !iplan

97              continue
              endif !inside?

              do iplan=1,ibpeplan(imag)

                bcvn=-bpetm(1,7,iplan,imag)*pi4inv

                bplan(1)=0.d0
                bplan(2)=0.d0
                bplan(3)=0.d0

c transform everything to the nz=(0,0,1) system

c                if (iseqdebug.ne.0) dum=bcvn
                if (bcvn.eq.0.0d0) cycle

                if (ibpecorn(iplan,imag).gt.0) then

                  ts(1:3,1:3)=bpetm(1:3,1:3,iplan,imag)
                  tsinv(1:3,1:3)=bpetm(1:3,4:6,iplan,imag)

                  xxrot=ts(1,1)*xx+ts(1,2)*yy+ts(1,3)*zz
                  yyrot=ts(2,1)*xx+ts(2,2)*yy+ts(2,3)*zz
                  zzrot=ts(3,1)*xx+ts(3,2)*yy+ts(3,3)*zz

                  ncorn=ibpecorn(iplan,imag)-1
                  do icorn=1,ncorn

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
                      kwarn=0

c                      if (iseqdebug.ne.0) iseqdebug=1
                      call undumag_bpeq(r1(1),r2(1),a,b,z,qx,qy,qz,
     &                  tiny,reverse,kwarn)
c                      if (iseqdebug.eq.2) then
c                        write(lun6,*)imag,iplan,icorn,kwarn
c                      endif

                      if (kwarn.ne.0) then
                        bpebc(16,imag)=kwarn
c                        write(lun6,*)"eder: kwarn,imag,xx,yy,zz",kwarn,imag,xx,yy,zz
                      endif
c                      if (kwarn.eq.1.or.kwarn.eq.6) kwarn=0
                      if (kwarn.eq.1.or.kwarn.eq.6.and.iwarn2pi.eq.0) kwarn=0
                      if (kwarn.eq.6) kwarni=6

c                      if (bcvn.ne.0.0d0) then
                      if (qx.ne.qx.or.qy.ne.qy.or.qz.ne.qz
     &                    .or.
     &                    (kwarn.ne.0)) then
                        kfail(ith)=imag
c                        if(iseqdebug.eq.-8) print*,"3:",ith,imag
c                        goto 799
                      endif !qx,qy,qz, kwarn

                      bplan(1)=bplan(1)-qx*bcvn
                      bplan(2)=bplan(2)-qy*bcvn
                      bplan(3)=bplan(3)-qz*bcvn
c                      endif !bcvn

c                      if (iseqdebug.ne.0) write(lun6,*)xin,iplan,icorn,bplan,kwarn
                    endif !r1(1)-r2(1)

                  enddo !icorn=1,ncorn

                  blab(1)=tsinv(1,1)*bplan(1)+tsinv(1,2)*bplan(2)+tsinv(1,3)*bplan(3)
                  blab(2)=tsinv(2,1)*bplan(1)+tsinv(2,2)*bplan(2)+tsinv(2,3)*bplan(3)
                  blab(3)=tsinv(3,1)*bplan(1)+tsinv(3,2)*bplan(2)+tsinv(3,3)*bplan(3)

                  if (
     &                blab(1).ne.blab(1)
     &                .or.
     &                blab(2).ne.blab(2)
     &                .or.
     &                blab(3).ne.blab(3)
     &                ) then
                    if (kudebug.eq.2) then
                      write(lun6,*)"*** Error 3 in undumag_bpolyeder: blab is not a number (NaN) ***"
                      write(lun6,*)
     &                  "imag,iplan,xin,yin,zin:",imag,iplan,xin,yin,zin,iseqdebug
c                    write(lun6,*)"blab",blab
c                    write(lun6,*)"tsinv",tsinv
                    endif
                    kfail(ith)=imag
c                    if(iseqdebug.eq.-8) print*,"4:",ith,imag
c                    stop
                  endif

                  bo(1,ith)=bo(1,ith)+blab(1)
                  bo(2,ith)=bo(2,ith)+blab(2)
                  bo(3,ith)=bo(3,ith)+blab(3)

                endif !ncorn

              enddo ! iplan=1,nplan

              if (iout.eq.-1) then
                if (kinsidelocal(ith).gt.0) then
                  write(lun6,*)"*** Warning in subroutine undumag_bpolyeder: Inside Magnet or colliding Magnets: ",imag,kinsidelocal(ith)
c                  print*,i_debug
c                  stop
                endif
                kinsidelocal(ith)=moth
              endif !iout

            else !bpebc(8,imag) .eq. 1

              if (kinsidelocal(ith).ne.-1) then

                iout=-1

                do iplan=1,ibpeplan(imag)

                  dlab(1)=xx-bpemag(1,1,iplan,imag)
                  dlab(2)=yy-bpemag(2,1,iplan,imag)
                  dlab(3)=zz-bpemag(3,1,iplan,imag)

                  vnormlab(1)=bpetm(1,8,iplan,imag)
                  vnormlab(2)=bpetm(2,8,iplan,imag)
                  vnormlab(3)=bpetm(3,8,iplan,imag)

                  if( dlab(1)*vnormlab(1)+dlab(2)*vnormlab(2)+
     &                dlab(3)*vnormlab(3).gt.0.d0) then
                    iout=1
                    goto 911
                  endif

                enddo !iplan

                if (iout.eq.-1) then
                  if (kinsidelocal(ith).gt.0) then
                    write(lun6,*)"*** Error 2 in subroutine undumag_bpolyeder: Colliding Magnets: ",imag,kinsidelocal(ith)
c                    stop
                  endif
                  kinsidelocal(ith)=moth
                endif !iout

911             continue
              endif !inside?

c rectangular or cylindrical magnet
              vmaglab(1:3)=bpebc(4:6,imag)

c transform everything to the nz=(0,0,1) system and rotate it parallel to x-axis

              ts(1:3,1:3)=bpetm(1:3,1:3,1,imag)
              tsinv(1:3,1:3)=bpetm(1:3,4:6,1,imag)

              xxrot=ts(1,1)*xx+ts(1,2)*yy+ts(1,3)*zz
              yyrot=ts(2,1)*xx+ts(2,2)*yy+ts(2,3)*zz
              zzrot=ts(3,1)*xx+ts(3,2)*yy+ts(3,3)*zz

              vmagrot(1)=
     &          ts(1,1)*vmaglab(1)+ts(1,2)*vmaglab(2)+ts(1,3)*vmaglab(3)
              vmagrot(2)=
     &          ts(2,1)*vmaglab(1)+ts(2,2)*vmaglab(2)+ts(2,3)*vmaglab(3)
              vmagrot(3)=
     &          ts(3,1)*vmaglab(1)+ts(3,2)*vmaglab(2)+ts(3,3)*vmaglab(3)

              xr(1)=bperot(1,1,1,imag)-xxrot
              xr(2)=bperot(1,2,1,imag)-xxrot
              yr(1)=bperot(2,1,1,imag)-yyrot
              yr(2)=bperot(2,3,1,imag)-yyrot
              zr(1)=bperot(3,1,1,imag)-zzrot
              zr(2)=bperot(3,1,3,imag)-zzrot

              if (xr(1).eq.0.0d0) xr(1)=1.0d-15
              if (xr(2).eq.0.0d0) xr(2)=1.0d-15
              if (yr(1).eq.0.0d0) yr(1)=1.0d-15
              if (yr(2).eq.0.0d0) yr(2)=1.0d-15
              if (zr(1).eq.0.0d0) zr(1)=1.0d-15
              if (zr(2).eq.0.0d0) zr(2)=1.0d-15

              q=0.0d0

              q(1,2)=1.0d0
              q(1,3)=1.0d0
              q(2,3)=1.0d0

              do i=1,2
                do j=1,2
                  do k=1,2
                    q(1,1)=q(1,1)+
     &                (-1)**(i+j+k+1)*
     &                atan(
     &                yr(j)/xr(i)*zr(k)/
     &                sqrt(xr(i)**2+yr(j)**2+zr(k)**2)
     &                )

                    q(2,2)=q(2,2)+
     &                (-1)**(i+j+k+1)*
     &                atan(
     &                xr(j)/yr(i)*zr(k)/
     &                sqrt(yr(i)**2+xr(j)**2+zr(k)**2)
     &                )

                     dum=
     &                (-1)**(i+j+k+1)*
     &                atan(
     &                yr(j)/zr(i)*xr(k)/
     &                sqrt(zr(i)**2+yr(j)**2+xr(k)**2)
     &                )
                    q(3,3)=q(3,3)+dum
c                    write(lun6,*)"--- imag,zzrot",imag,zzrot
c                    write(lun6,*)"i,j,k",i,j,k,xr(k),yr(j),zr(i)
c                    write(lun6,*)"xx,dum,q33",xx,dum,q(3,3)

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
     &          -(q(1,1)*vmagrot(1)+q(1,2)*vmagrot(2)+q(1,3)*vmagrot(3))*
     &          pi4inv
              h(2)=
     &          -(q(2,1)*vmagrot(1)+q(2,2)*vmagrot(2)+q(2,3)*vmagrot(3))*
     &          pi4inv
              h(3)=
     &          -(q(3,1)*vmagrot(1)+q(3,2)*vmagrot(2)+q(3,3)*vmagrot(3))*
     &          pi4inv

              bplan(1)=h(1)
              bplan(2)=h(2)
              bplan(3)=h(3)

c              write(lun6,*)"h============================:",h

              blab(1)=tsinv(1,1)*bplan(1)+tsinv(1,2)*bplan(2)+tsinv(1,3)*bplan(3)
              blab(2)=tsinv(2,1)*bplan(1)+tsinv(2,2)*bplan(2)+tsinv(2,3)*bplan(3)
              blab(3)=tsinv(3,1)*bplan(1)+tsinv(3,2)*bplan(2)+tsinv(3,3)*bplan(3)

              bo(1,ith)=bo(1,ith)+blab(1)
              bo(2,ith)=bo(2,ith)+blab(2)
              bo(3,ith)=bo(3,ith)+blab(3)

            endif !(bpebc(8,imag).eq.1)

          endif !non-zero magnetization

c        write(lun6,'(3g15.5)'),xin,imag,imag,bo(2,ith)

        endif !window
c799     continue
      enddo !imag=1,nmag
!$OMP END DO
!$OMP END PARALLEL

      do ic=1,nmaxth
        ifail=ifail+kfail(ic)
        bxout=bxout+bo(1,ic)
        byout=byout+bo(2,ic)
        bzout=bzout+bo(3,ic)
      enddo

      if (kinside.ne.-1) then
        kinside=0
        ifound=0
        do ic=1,nmaxth
          kinside=kinside+kinsidelocal(ic)
          if (kinsidelocal(ic).gt.0) ifound=ifound+1
          if (ifound.gt.0.and.i_debug.eq.0) then
            print*,ical,kinsidelocal(1:nmaxth)
            i_debug=1
          endif
          if (ifound.gt.1) then
            write(lun6,*)"*** Error 3 in subroutine undumag_bpolyeder: Colliding Magnet: ",kinsidelocal(ic)
c            stop
          endif
        enddo
      endif

      if (bxout.ne.bxout.or.byout.ne.byout.or.bzout.ne.bzout
     &  .or.bxout.gt.1.0d30
     &  .or.byout.gt.1.0d30
     &    .or.bzout.gt.1.0d30) then
        bxout=0.0d0
        byout=0.0d0
        bzout=0.0d0
        ifail=11
      endif

      if (abs(bxout).le.1.0d-15) bxout=0.0d0
      if (abs(byout).le.1.0d-15) byout=0.0d0
      if (abs(bzout).le.1.0d-15) bzout=0.0d0

c      if (iseqdebug.ne.0) write(lun6,*)"ifail:",ifail
      if (ifail.ne.0) goto 7799

      goto 7979

7799  continue

      if (corrtiny.eq.0.0) then
        ifail=-4
        goto 7979
      endif

      if (kudebug.eq.2) then
        write(lun6,*)"*** ifail,xin,yin,zin"
      endif

      kwarncom=1
      ifail=-3

C Not working for unknown reasons for OMP ?? still true 20.4.2017??

      if (kinside.ne.-1) then
        kinside=0
      endif

      call undumag_bpolyeder_corr(xin-corrtiny,yin-corrtiny,zin-corrtiny,
     &  bxm,bym,bzm,ifailm)

      if (kinside.ne.-1) then
        linside=kinside
        kinside=0
      endif

      call undumag_bpolyeder_corr(xin+corrtiny,yin+corrtiny,zin+corrtiny,
     &  bxp,byp,bzp,ifailp)

      if (kinside.ne.-1) then
        kinside=max(linside,kinside)
      endif

      if (ifailm.eq.0.and.ifailp.eq.0) then
        bxout=(bxm+bxp)/2.0d0
        byout=(bym+byp)/2.0d0
        bzout=(bzm+bzp)/2.0d0
        ifail=-1
        kwarncom=1
      else if (ifailm.eq.0) then
        bxout=bxm
        byout=bym
        bzout=bzm
        kwarncom=2
        ifail=2
      else if (ifailp.eq.0) then
        bxout=bxp
        byout=byp
        bzout=bzp
        kwarncom=2
        ifail=2
      else
        if (ifailin.ge.0) then
          write(lun6,*) "*** Warning in undumag_bpolyeder: Could not recover for x,y,z:",
     &      sngl(xin*1000.),sngl(yin*1000.0),sngl(zin*1000.)
          write(lun6,*)"Differences in Bx,By,Bz, abs. and. rel.:",
     &      sngl(abs(bxp-bxm)),sngl(abs(byp-bym)),sngl(abs(bzp-bzm))
        endif
        bxout=(bxm+bxp)/2.0d0
        byout=(bym+byp)/2.0d0
        bzout=(bzm+bzp)/2.0d0
        if (ifailin.ge.0) then
          print '(6e15.4)',
     &      abs((bxp-bxm)/(bxout+1.0d-15)),abs((byp-bym)/(byout+1.0d-15)),
     &      abs((bzp-bzm)/(bzout+1.0d-15))
        endif
        kwarncom=3
        ifail=3
      endif

7979  continue

c      if (ncwires+nrace.gt.0) then
c        call undumag_bcoils(xin,yin,zin,bx,by,bz,istat)
c        bxout=bxout+bx
c        byout=byout+by
c        bzout=bzout+bz
c        if (istat.ne.0) ifail=ifail+1000
c      endif

      return
      end
