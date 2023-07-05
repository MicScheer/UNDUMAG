*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.22/02 02/08/2017  09.10.57  by  Michael Scheer
*CMZ :  1.17/08 24/05/2017  15.33.47  by  Michael Scheer
*CMZ :  1.17/07 23/05/2017  15.39.11  by  Michael Scheer
*CMZ :  1.17/02 08/03/2016  16.00.02  by  Michael Scheer
*CMZ :  1.15/01 24/04/2008  11.51.05  by  Michael Scheer
*CMZ :  1.15/00 26/10/2007  13.11.55  by  Michael Scheer
*CMZ :  1.12/16 01/06/2007  11.17.50  by  Michael Scheer
*CMZ :  1.12/02 14/07/2005  10.19.00  by  Michael Scheer
*CMZ :  1.10/03 19/08/2004  15.32.19  by  Michael Scheer
*CMZ :  1.10/02 19/08/2004  14.03.28  by  Michael Scheer
*CMZ :  1.10/01 17/08/2004  14.15.54  by  Michael Scheer
*CMZ :  2.00/00 17/08/2004  09.38.52  by  Michael Scheer
*CMZ :  1.02/02 11/08/2004  09.13.49  by  Michael Scheer
*CMZ :  1.02/01 09/08/2004  14.45.40  by  Michael Scheer
*CMZ :  1.02/00 28/07/2004  16.57.47  by  Michael Scheer
*-- Author :    Michael Scheer   27/07/2004
      subroutine undumag_bpolyint(imag,xint,yint,zint,
     &  vxint,vyint,vzint,
     &  bxint,byint,bzint,ifail)

c Literature: Elleaume, Chubar, Chavanne PAC97
c             Computing 3D Magnetic Fields form Insertion Devices

c calculates first magnetic integral for line defined by point
c      (xint,yint,zint) and vector (vxint,vyint,vzint)
c restrictions:
c     rectangular magnets only
c     line of integrations parallel to axis of block and block aligned to
c     axis of coordinate system (paper gives formulas for general
c     case of integration direction (not coded here))

c xint,yint,zint given in meter
c bxint,byint,bzint given in Tm

*KEEP,bpolyederf90u.
      include 'bpolyederf90u.cmn'
*KEND.
      use undumagf90m

      use commandlinef90m

      implicit none


      double precision xint,yint,zint,vxint,vyint,vzint,vnx,vny,vnz,vn,
     &  bxint,byint,bzint,
     &  vnxrot,vnyrot,vnzrot,
     &  vnxrota,vnyrota,vnzrota,
     &  xr(2),yr(2),zr(2),ts(3,3),
     &  tsinv(3,3),w(3),x12,x22,z12,z22,y12,y22

      double precision g(3,3),vmagrot(3),vmaglab(3),bint1(3),xxrot,yyrot,zzrot,
     &  xxint,yyint,zzint,tiny2,pi2inv,pi4inv,
     &  xmin,xmax,ymin,ymax,zmin,zmax,
     &  dlab(3),vnormlab(3)

      parameter (pi2inv=0.159154943091895d0)
      parameter (pi4inv=0.0795774715459477d0)

      integer ical,iwarn1
      integer kmag,i,k,nx,ny,nz,ifail,iout,imag
      integer iplan,ishim,ishima,nmag1,nmag2

      data ical/0/
      data iwarn1/0/

      if (imag.le.0) then
        write(lun6,*)'*** Error in undumag_bpolyint: Called with imag=0 ***'
        stop
      endif

      tiny2=tiny*tiny

      bxint=0.d0
      byint=0.d0
      bzint=0.d0

      ifail=-1

      if (
     &    xint.ge.outbox(1,1).and.xint.le.outbox(2,1)
     &    .and.
     &    yint.ge.outbox(1,2).and.yint.le.outbox(2,2)
     &    .and.
     &    zint.ge.outbox(1,3).and.zint.le.outbox(2,3)
     &    ) return

      ifail=0

      xxint=xint*1000.0d0
      yyint=yint*1000.0d0
      zzint=zint*1000.0d0

      vn=sqrt(vxint*vxint+vyint*vyint+vzint*vzint)

      vnx=vxint/vn
      vny=vyint/vn
      vnz=vzint/vn

      if (bpebc(7,imag).eq.0.0d0) return

      if(bpebc(8,imag).ne.-6.and.bpebc(8,imag).ne.-7) then !not rectangular magnet

        if (iwarn1.eq.0) then
          iwarn1=1
          write(lun6,*)'*** Warning in undumag_bpolyint: Non-rectangular magnet!'
          write(lun6,*)'***                      not yet implemented!'
          write(lun6,*)'***                      zero result returned'
          write(lun6,*)'*** FURTHER WARNINGS SUPPRESSED!!'
        endif !iwarn1

        ifail=-11

        return

      else !bpebc(8,imag).ne.-6

        iout=-1
        inside=1

        do iplan=1,ibpeplan(imag)

          dlab(1)=xxint-bpemag(1,1,iplan,imag)
          dlab(2)=yyint-bpemag(2,1,iplan,imag)
          dlab(3)=zzint-bpemag(3,1,iplan,imag)

          vnormlab(1)=bpetm(1,8,iplan,imag)
          vnormlab(2)=bpetm(2,8,iplan,imag)
          vnormlab(3)=bpetm(3,8,iplan,imag)

          if( dlab(1)*vnormlab(1)+dlab(2)*vnormlab(2)+
     &        dlab(3)*vnormlab(3).gt.0.d0) then
            iout=1
            inside=0
            goto 91
          endif

        enddo !iplan

        if (iout.ne.1) then
          ifail=3
          return
        endif

91      vmaglab(1)=bpebc(4,imag)
        vmaglab(2)=bpebc(5,imag)
        vmaglab(3)=bpebc(6,imag)

c transform everything to the nz=(0,0,1) system and rotate it parallel to x-axis

        do i=1,3
          do k=1,3
            ts(i,k)=bpetm(i,k,1,imag)
            tsinv(i,k)=bpetm(i,k+3,1,imag)
          enddo
        enddo

        xxrot=ts(1,1)*xxint+ts(1,2)*yyint+ts(1,3)*zzint
        yyrot=ts(2,1)*xxint+ts(2,2)*yyint+ts(2,3)*zzint
        zzrot=ts(3,1)*xxint+ts(3,2)*yyint+ts(3,3)*zzint

        vnxrot=ts(1,1)*vnx+ts(1,2)*vny+ts(1,3)*vnz
        vnyrot=ts(2,1)*vnx+ts(2,2)*vny+ts(2,3)*vnz
        vnzrot=ts(3,1)*vnx+ts(3,2)*vny+ts(3,3)*vnz

        vnxrota=abs(vnxrot)
        vnyrota=abs(vnyrot)
        vnzrota=abs(vnzrot)

        vmagrot(1)=
     &    ts(1,1)*vmaglab(1)+ts(1,2)*vmaglab(2)+ts(1,3)*vmaglab(3)
        vmagrot(2)=
     &    ts(2,1)*vmaglab(1)+ts(2,2)*vmaglab(2)+ts(2,3)*vmaglab(3)
        vmagrot(3)=
     &    ts(3,1)*vmaglab(1)+ts(3,2)*vmaglab(2)+ts(3,3)*vmaglab(3)

c dimensions of magnet

        w(1)=bperot(1,1,1,imag)-bperot(1,2,1,imag)
        w(2)=bperot(2,1,1,imag)-bperot(2,3,1,imag)
        w(3)=bperot(3,1,1,imag)-bperot(3,1,3,imag)

c distances from considered point to corners of magnet

        xr(1)=bperot(1,1,1,imag)-xxrot
        xr(2)=bperot(1,2,1,imag)-xxrot
        yr(1)=bperot(2,1,1,imag)-yyrot
        yr(2)=bperot(2,3,1,imag)-yyrot

        zr(1)=bperot(3,1,1,imag)-zzrot
        zr(2)=bperot(3,1,3,imag)-zzrot

        g=0.0d0

        if (vnxrota.lt.tiny2.and.vnyrota.lt.tiny2) then

          if (abs(xr(1)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: xr(1) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'xr(1)',xr(1)
            if (xr(1).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              xr(1)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              xr(1)=tiny2
            endif
            ifail=1
          endif


          if (abs(xr(2)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: xr(2) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'xr(2)',xr(2)
            if (xr(2).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              xr(2)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              xr(2)=tiny2
            endif
            ifail=1
          endif

          if (abs(yr(1)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: yr(1) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'yr(1)',yr(1)
            if (yr(1).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              yr(1)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              yr(1)=tiny2
            endif
            ifail=1
          endif

          if (abs(yr(2)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: yr(2) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'yr(2)',yr(2)
            if (yr(2).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              yr(2)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              yr(2)=tiny2
            endif
            ifail=1
          endif

          do i=1,2
            do k=1,2
              g(1,1)=g(1,1)+(-1)**(i+k)*atan(xr(i)/yr(k))
              g(2,2)=g(2,2)+(-1)**(i+k)*atan(yr(k)/xr(i))
            enddo !k
          enddo !i

          x12=xr(1)*xr(1)
          x22=xr(2)*xr(2)
          y12=yr(1)*yr(1)
          y22=yr(2)*yr(2)

          g(1,2)=w(3)*pi4inv*log((x12+y22)*(x22+y12)/((x12+y12)*(x22+y22)))
     &          *0.001d0 !Tmm -> Tm
          g(2,1)=g(1,2)

          g(1,1)=g(1,1)*w(3)*pi2inv
     &          *0.001d0 !Tmm -> Tm
          g(2,2)=g(2,2)*w(3)*pi2inv
     &          *0.001d0 !Tmm -> Tm

        else if (vnxrota.lt.tiny2.and.vnzrota.lt.tiny2) then

          if (abs(xr(1)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: xr(1) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'xr(1)',xr(1)
            if (xr(1).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              xr(1)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              xr(1)=tiny2
            endif
            ifail=1
          endif

          if (abs(xr(2)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: xr(2) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'xr(2)',xr(2)
            if (xr(2).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              xr(2)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              xr(2)=tiny2
            endif
            ifail=1
          endif

          if (abs(zr(1)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: zr(1) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'zr(1)',zr(1)
            if (zr(1).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              zr(1)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              zr(1)=tiny2
            endif
            ifail=1
          endif

          if (abs(zr(2)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: zr(2) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'zr(2)',zr(2)
            if (zr(2).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              zr(2)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              zr(2)=tiny2
            endif
            ifail=1
          endif

          do i=1,2
            do k=1,2
              g(1,1)=g(1,1)+(-1)**(i+k)*atan(xr(i)/zr(k))
              g(3,3)=g(3,3)+(-1)**(i+k)*atan(zr(k)/xr(i))
            enddo !k
          enddo !i

          x12=xr(1)*xr(1)
          x22=xr(2)*xr(2)
          z12=zr(1)*zr(1)
          z22=zr(2)*zr(2)

          g(1,3)=w(2)*pi4inv*log((x12+z22)*(x22+z12)/((x12+z12)*(x22+z22)))
     &          *0.001d0 !Tmm -> Tm
          g(3,1)=g(1,3)

          g(1,1)=g(1,1)*w(2)*pi2inv
     &          *0.001d0 !Tmm -> Tm
          g(3,3)=g(3,3)*w(2)*pi2inv
     &          *0.001d0 !Tmm -> Tm

        else if (vnyrota.lt.tiny2.and.vnzrota.lt.tiny2) then

          if (abs(yr(1)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: yr(1) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'yr(1)',yr(1)
            if (yr(1).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              yr(1)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              yr(1)=tiny2
            endif
            ifail=1
          endif

          if (abs(yr(2)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: yr(2) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'yr(2)',yr(2)
            if (yr(2).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              yr(2)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              yr(2)=tiny2
            endif
            ifail=1
          endif

          if (abs(zr(1)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: zr(1) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'zr(1)',zr(1)
            if (zr(1).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              zr(1)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              zr(1)=tiny2
            endif
            ifail=1
          endif

          if (abs(zr(2)).lt.tiny2) then
            write(lun6,*)'Warning in undumag_bpolyint: zr(2) too small'
            write(lun6,*)'magnet:',imag
            write(lun6,*)'zr(2)',zr(2)
            if (zr(2).lt.0.0d0) then
              write(lun6,*)'Set to',-tiny2
              zr(2)=-tiny2
            else
              write(lun6,*)'Set to',tiny2
              zr(2)=tiny2
            endif
            ifail=1
          endif

          do i=1,2
            do k=1,2
              g(2,2)=g(2,2)+(-1)**(i+k)*atan(yr(i)/zr(k))
              g(3,3)=g(3,3)+(-1)**(i+k)*atan(zr(k)/yr(i))
            enddo !k
          enddo !i

          y12=yr(1)*yr(1)
          y22=yr(2)*yr(2)
          z12=zr(1)*zr(1)
          z22=zr(2)*zr(2)

          g(2,3)=w(1)*pi4inv*log((y12+z22)*(y22+z12)/((y12+z12)*(y22+z22)))
     &          *0.001d0 !Tmm -> Tm
          g(3,2)=g(2,3)

          g(2,2)=g(2,2)*w(1)*pi2inv
     &          *0.001d0 !Tmm -> Tm
          g(3,3)=g(3,3)*w(1)*pi2inv
     &          *0.001d0 !Tmm -> Tm

        else ! vnxrot,vnyrot,vnzot parallel to an axis

          write(lun6,*)'*** Warning in undumag_bpolyint:'
          write(lun6,*)
     &      '*** line of integration not parallel to axis of coord.-system!'
          write(lun6,*)'*** not yet implemented!'
          write(lun6,*)'*** zero result returned'
          ifail=2
          return

        endif ! vnxrot,vnyrot,vnzrot parallel to an axis

        bint1(1)=(g(1,1)*vmagrot(1)+g(1,2)*vmagrot(2)+g(1,3)*vmagrot(3))
        bint1(2)=(g(2,1)*vmagrot(1)+g(2,2)*vmagrot(2)+g(2,3)*vmagrot(3))
        bint1(3)=(g(3,1)*vmagrot(1)+g(3,2)*vmagrot(2)+g(3,3)*vmagrot(3))

        bxint=tsinv(1,1)*bint1(1)+tsinv(1,2)*bint1(2)+tsinv(1,3)*bint1(3)
        byint=tsinv(2,1)*bint1(1)+tsinv(2,2)*bint1(2)+tsinv(2,3)*bint1(3)
        bzint=tsinv(3,1)*bint1(1)+tsinv(3,2)*bint1(2)+tsinv(3,3)*bint1(3)

      endif !(bpebc(8,imag).eq.1)

      return
      end
