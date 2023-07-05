*CMZ :  2.02/00 26/10/2020  14.56.48  by  Michael Scheer
*CMZ :  2.01/03 15/07/2019  11.55.46  by  Michael Scheer
*CMZ :  2.01/02 27/04/2018  13.35.13  by  Michael Scheer
*CMZ :  1.22/01 20/07/2017  14.55.33  by  Michael Scheer
*CMZ :  1.20/00 22/06/2017  08.44.13  by  Michael Scheer
*CMZ :  1.15/11 20/04/2017  16.01.40  by  Michael Scheer
*CMZ :  1.15/03 02/04/2017  15.55.59  by  Michael Scheer
*CMZ :  1.15/02 01/04/2017  17.59.10  by  Michael Scheer
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
      subroutine undumag_bpolyeder_corr(xin,yin,zin,bxout,byout,bzout,ifail)
c
c      Calculation of magnetic field of polyhedron according to
c      Oleb Chubar, Pascal Elleaume and Joel Chavanne
c      J. Synchrotron Rad. (1998) 5, 481-484
c
c Paper contains an error: The rotation matrix is wrong, since for nz=-1 the
c                          determinant is -1, which yields to errors??.
      use omp_lib
      use bpolyederf90m
      use undumagf90m

      use commandlinef90m

      implicit none
*KEEP,seqdebug.
      include 'seqdebug.cmn'
*KEND.

      double precision xin,yin,zin,bxout,byout,bzout
      double precision r1(3),r2(3),dlab(3),blab(3)
      double precision ts(3,3),tsinv(3,3),bplan(3),bcvn,vnormlab(3)
      double precision xx,yy,zz,xxrot,yyrot,zzrot,xx00,xxsh
      double precision a,b,z,qx,qy,qz,qxp,qyp,qzp,qxm,qym,qzm,
     &  pi4inv,reverse,tiny2,
     &  bxm,bym,bzm,bxp,byp,bzp,rr0,rrm
      double precision q(3,3),vmagrot(3),vmaglab(3),h(3),
     &  xr(2),yr(2),zr(2),dum,dume,bo(3,nthreadp)

      double precision xmin,xmax,ymin,ymax,zmin,zmax,bx,by,bz

      parameter (pi4inv=0.0795774715459477d0)

      integer ical,kc
      integer itiny,iwtiny,jtiny
      integer imag,iplan,icorn,i,j,k,ip2,kwarn,ic
      integer nx,ny,nz,ifail,ishim,ishima,iimag,nmag1,nmag2,iout,
     &  kfail(nthreadp),kinsidelocal(nthreadp)

      integer nmaxth,ith

      save bo,nmaxth,ith,ical,kinsidelocal

      data ical/0/

      if (magmag.gt.0) then
        write(lun6,*)"*** Error: Call to undumag_bpolyeder_corr in undumag_bpolyeder_corr!"
        stop
      endif

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      tiny2=tiny*tiny

      ifail=0

c calculate field at (xin,yin,zin)

      if (magmag.lt.0) then
        return
      endif !magmag.le.0

c      if (xin.eq.0.0d0) return
      xx=xin*1000.0d0
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
        write(lun6,*)"Number of CPU cores used:",nmaxth
        bo=0.0d0
        ical=1
      endif

c      ical=ical+1
c      write(lun6,*)"eder",ical,nmaxth
      bo=0.0d0
      kinsidelocal=kinside
      kfail=0

!$OMP PARALLEL NUM_THREADS(nmaxth) DEFAULT(PRIVATE) SHARED(kfail,iseqdebug,kinsidelocal,ical,bpetm,window,bpebc,bpemag,ibpeplan,ibpecorn,bperot,bo,iwarnbound,nwarnbound) FIRSTPRIVATE(nmag,itiny,jtiny,iwtiny,tiny,xx,yy,zz)
c      if (ical.eq.4) stop
      ith=OMP_GET_THREAD_NUM()+1
      bo(1:3,ith)=0.0d0
!$OMP DO
      do imag=1,nmag

        if (bpebc(17,imag).lt.0.0d0) then
          cycle
        endif

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
     &                dlab(3)*vnormlab(3).gt.0.d0) then
                    iout=1
                    goto 97
                  endif

                enddo !iplan

                if (iout.eq.-1) then
                  kinsidelocal(ith)=imag
                endif !iout

97              continue
              endif !inside?

              do iplan=1,ibpeplan(imag)

                bcvn=-bpetm(1,7,iplan,imag)*pi4inv
c                write(lun6,*)"undumag_bpolyeder_corr:",imag,iplan,bpetm(1,7,iplan,imag)

                bplan(1)=0.d0
                bplan(2)=0.d0
                bplan(3)=0.d0

c transform everything to the nz=(0,0,1) system

c                if (iseqdebug.ne.0) write(lun6,*)"bcvn:",bcvn
                if (bcvn.eq.0.0d0) cycle

                if (ibpecorn(iplan,imag).gt.0) then

                  ts(1:3,1:3)=bpetm(1:3,1:3,iplan,imag)
                  tsinv(1:3,1:3)=bpetm(1:3,4:6,iplan,imag)

c                  do i=1,3
c                    do j=1,3
c                      ts(i,j)=bpetm(i,j,iplan,imag)
c                      tsinv(i,j)=bpetm(i,j+3,iplan,imag)
c                    enddo
c                  enddo

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
                      kwarn=0

                      call undumag_bpeq(r1(1),r2(1),a,b,z,qx,qy,qz,
     &                  tiny,reverse,kwarn)

c                      if (bcvn.ne.0.0d0) then
                      if (qx.ne.qx.or.qy.ne.qy.or.qz.ne.qz
     &                    .or.
     &                    (kwarn.ne.0.and.kwarn.ne.6)) then
                        kfail(ith)=imag
                        goto 799
                      endif !qx,qy,qz, kwarn

                      bplan(1)=bplan(1)-qx*bcvn
                      bplan(2)=bplan(2)-qy*bcvn
                      bplan(3)=bplan(3)-qz*bcvn
c                      endif !bcvn

                    endif
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
                    write(lun6,*)"*** Error 3 in undumag_bpolyeder_corr: blab is not a number (NaN) ***"
                    write(lun6,*)
     &                "imag,iplan,xin,yin,zin:",imag,iplan,xin,yin,zin
                    write(lun6,*)"blab",blab
                    write(lun6,*)"tsinv",tsinv
                    stop
                  endif
                  bo(1,ith)=bo(1,ith)+blab(1)
                  bo(2,ith)=bo(2,ith)+blab(2)
                  bo(3,ith)=bo(3,ith)+blab(3)
                endif !ncorn

              enddo ! iplan=1,nplan

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

c                  write(lun6,*)"x,dlab:",xx,dlab
c                  write(lun6,*)"vnormlablab:",vnormlab
                  if( dlab(1)*vnormlab(1)+dlab(2)*vnormlab(2)+
     &                dlab(3)*vnormlab(3).gt.0.d0) then
                    iout=1
                    goto 911
                  endif

                enddo !iplan

                if (iout.eq.-1) then
                  kinsidelocal(ith)=imag
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

c              if (xr(1).eq.0.0d0.or.yr(1).eq.0.0d0.or.zr(1).eq.0.0d0
c     &            .or.xr(2).eq.0.0d0.or.yr(2).eq.0.0d0.or.zr(2).eq.0.0d0
c     &            ) then
c                xx=xx+tiny2
cc                yy=yy+tiny2
c                zz=zz+tiny2
c                goto 1
c              endif

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
799     continue
      enddo !imag=1,nmag
!$OMP END DO
!$OMP END PARALLEL

9999  continue


      do ic=1,nmaxth
        ifail=ifail+kfail(ic)
        bxout=bxout+bo(1,ic)
        byout=byout+bo(2,ic)
        bzout=bzout+bo(3,ic)
      enddo

      if (kinside.ne.-1) then
        kinside=0
        do ic=1,nmaxth
          kinside=kinside+kinsidelocal(ic)
          if (kinsidelocal(ic).ne.0) then
            do kc=1,nmaxth
              if (kc.eq.ic) cycle
              if (kinsidelocal(kc).ne.0) then
                write(lun6,*)kinsidelocal
              endif
            enddo
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
        ifail=-1
      endif

      if (abs(bxout).le.1.0d-15) bxout=0.0d0
      if (abs(byout).le.1.0d-15) byout=0.0d0
      if (abs(bzout).le.1.0d-15) bzout=0.0d0

c      if (ical.gt.2) stop

      return
      end
