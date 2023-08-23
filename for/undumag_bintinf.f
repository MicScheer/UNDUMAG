*CMZ :  2.02/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.00/00 11/04/2018  12.59.53  by  Michael Scheer
*CMZ :  1.23/03 27/09/2017  09.16.22  by  Michael Scheer
*CMZ :  1.23/02 15/09/2017  21.47.55  by  Michael Scheer
*-- Author :    Michael Scheer   30/08/2017
      subroutine undumag_bintinf(x0in,y0in,z0in,vxin,vyin,vzin,
     &  bintxo,bintyo,bintzo,ifail)

      use bpolyederf90m
      use undumagf90m

      use commandlinef90m

      implicit none

      double precision, dimension (:,:,:), allocatable :: corn
      double precision, dimension (:,:), allocatable :: vnor,corny

      double precision bintxo,bintyo,bintzo,bintx,binty,bintz,
     &  bxi,byi,bzi,
     &  x,y,z,x0,y0,z0,vx,vy,vz,a1,b1,vn,vxin,vyin,vzin,
     &  ts(3,3),tsinv(3,3),r1(3),r2(3),bcx,bcy,bcz,
     &  x0in,y0in,z0in,v3(3),
     &  v3r(3),w3r(3),g(3),vni(3),v(3),vp(3)

      double precision ans,ans1,ans2,ans3,ans4,ans5,ans6,ans7,ans8,ans9,ans10,
     &  ans11,ans12,ans13

      double precision xl,xh,yl,yh,
     &  gxy,gyy,gzy,gxys,gyys,gzys,
     &  chargedens

      double precision :: eps=1.0d-7

      integer lfail,kfail,ifail,imag,icorn,iplan,ncorn,nplan,i,j,ical

      ifail=0
      lfail=0
      bintxo=0.0d0
      bintyo=0.0d0
      bintzo=0.0d0

      vn=sqrt(vxin*vxin+vyin*vyin+vzin*vzin)

      if (vn.eq.0.0d0) then
        ifail=-1
        return
      endif

      x0=x0in*1000.0d0
      y0=y0in*1000.0d0
      z0=z0in*1000.0d0

      v(1)=vxin/vn
      v(2)=vyin/vn
      v(3)=vzin/vn

      if (
     &  abs(v(2)).gt.1.0d-6.or.abs(v(3)).gt.1.0d-6
c     &    .or.abs(y0in).gt.1.0d-6.or.abs(z0in).gt.1.0d-6
     &    ) then
        write(lun6,*)
     &    '*** Error in undumag_bintinf: Integration must be along x-axis ***'
        ifail=99
        return
      endif

      allocate (corn(3,ncornmax,nplanmax))
      allocate (corny(3,ncornmax))
      allocate (vnor(3,nplanmax))

      do imag=1,nmag

        bcx=bpebc(4,imag)
        bcy=bpebc(5,imag)
        bcz=bpebc(6,imag)

        if (abs(bcx).lt.eps.and.abs(bcy).lt.eps.and.abs(bcz).lt.eps) cycle

        nplan=ibpeplan(imag)

        do iplan=1,nplan
          ncorn=ibpecorn(iplan,imag)
          do icorn=1,ncorn
            corn(1,icorn,iplan)=bpemag(1,icorn,iplan,imag)
            corn(2,icorn,iplan)=bpemag(2,icorn,iplan,imag)-y0
            corn(3,icorn,iplan)=bpemag(3,icorn,iplan,imag)-z0
          enddo
          vnor(1:3,iplan)=bpetm(1:3,8,iplan,imag)
          vnor(1:3,iplan)=vnor(1:3,iplan)/
     &      sqrt(vnor(1,iplan)**2+vnor(2,iplan)**2+vnor(3,iplan)**2)
        enddo

        do iplan=1,nplan

          ! We rotate the plane for the integration such, that
          ! the normal vector is (0,0,1)

          vp=vnor(1:3,iplan)
          chargedens=-(bcx*vp(1)+bcy*vp(2)+bcz*vp(3))

c          if (abs(chargedens).lt.eps) cycle

          call util_matrix_to_rot_vec_to_z(vp,ts,ifail)
          do i=1,3
            do j=1,3
              tsinv(i,j)=ts(j,i)
            enddo
          enddo

          v3=vp
          v3r(1)=ts(1,1)*v3(1)+ts(1,2)*v3(2)+ts(1,3)*v3(3)
          v3r(2)=ts(2,1)*v3(1)+ts(2,2)*v3(2)+ts(2,3)*v3(3)
          v3r(3)=ts(3,1)*v3(1)+ts(3,2)*v3(2)+ts(3,3)*v3(3)
          vni=v3r



          v3=v
          vx=ts(1,1)*v3(1)+ts(1,2)*v3(2)+ts(1,3)*v3(3)
          vy=ts(2,1)*v3(1)+ts(2,2)*v3(2)+ts(2,3)*v3(3)
          vz=ts(3,1)*v3(1)+ts(3,2)*v3(2)+ts(3,3)*v3(3)

          ncorn=ibpecorn(iplan,imag) ! closed polygon

          do icorn=1,ncorn
            v3=corn(1:3,icorn,iplan)
            v3r(1)=ts(1,1)*v3(1)+ts(1,2)*v3(2)+ts(1,3)*v3(3)
            v3r(2)=ts(2,1)*v3(1)+ts(2,2)*v3(2)+ts(2,3)*v3(3)
            v3r(3)=ts(3,1)*v3(1)+ts(3,2)*v3(2)+ts(3,3)*v3(3)
            corny(1:3,icorn)=v3r
          enddo !ncorn

          ncorn=ncorn-1 !open polygon

          kfail=0
          gxys=0.0d0
          gyys=0.0d0
          gzys=0.0d0

          do icorn=1,ncorn

            v3r=corny(1:3,icorn)
            w3r=corny(1:3,icorn+1)

            if (abs(v3r(3)-w3r(3)).gt.eps) then
              write(lun6,*)"*** Warning in undumag_bintinf: Bad transformation of plane ***"
              write(lun6,*)"imag,iplan,icorn:",imag,iplan,icorn
              write(lun6,*)"(abs(v3r(3)-w3r(3)).gt.eps)",
     &          abs(v3r(3)-w3r(3)),eps
c              ifail=-2
c              goto 9999
            endif

            xl=v3r(1)
            yl=v3r(2)
            xh=w3r(1)
            yh=w3r(2)

            z=v3r(3)

            if (abs(xl-xh).lt.eps) cycle

            a1=(yh-yl)/(xh-xl)
            b1=yl-a1*xl

            if (a1.ne.0.0d0) then
              if (abs(vy).lt.eps.and.abs(vz).lt.eps) then
c                include 'g_nur_vx.f'
                gxy=0.0d0
                gyy=(-(2.0d0*(atan((a1*xh+b1)/z)*z-a1*xh)+(a1*xh+b1)*log((a1*xh+
     &            2.0d0*b1)*a1*xh+b1**2+z**2)-(2.0d0*(atan((a1*xl+b1)/z)*z-a1*xl)+(
     &            a1*xl+b1)*log((a1*xl+2.0d0*b1)*a1*xl+b1**2+z**2))))/(2.0d0*a1)
                gzy=(-(2.0d0*(a1*xh+b1)*atan((a1*xh+b1)/z)-log((a1*xh+2.0d0*b1)*a1*
     &            xh+b1**2+z**2)*z-(2.0d0*(a1*xl+b1)*atan((a1*xl+b1)/z)-log((a1*xl
     &            +2.0d0*b1)*a1*xl+b1**2+z**2)*z)))/(2.0d0*a1)

              else if (abs(vx).lt.eps.and.abs(vy).lt.eps) then
c              else if (vx.eq.0.0d0.and.vy.eq.0.0d0) then
c                include 'g_nur_vz.f'
                gxy=(-(2.0d0*(a1**2*xh+a1*b1+xh)*atan((a1*xh+b1)/xh)+log((a1*xh+
     &            2.0d0*b1)*a1*xh+b1**2+xh**2)*b1-(2.0d0*(a1**2*xl+a1*b1+xl)*atan((
     &            a1*xl+b1)/xl)+log((a1*xl+2.0d0*b1)*a1*xl+b1**2+xl**2)*b1)))/(2.0d0
     &            *(a1**2+1.0d0))
                gyy=(-((a1**2*xh+a1*b1+xh)*log((a1*xh+2.0d0*b1)*a1*xh+b1**2+xh**2
     &            )-(a1**2*xl+a1*b1+xl)*log((a1*xl+2.0d0*b1)*a1*xl+b1**2+xl**2)+
     &            2.0d0*((a1**2+1.0d0)*xl-atan((a1**2*xl+a1*b1+xl)/b1)*b1)-2.0d0*((a1
     &            **2+1.0d0)*xh-atan((a1**2*xh+a1*b1+xh)/b1)*b1)))/(2.0d0*(a1**2+1.0d0
     &            ))
                gzy=0.0d0

              else if (abs(vx).lt.eps.and.abs(vz).lt.eps) then
c              else if (vx.eq.0.0d0.and.vz.eq.0.0d0) then
c                include 'g_nur_vy.f'
                gxy=(2.0d0*atan(xh/z)*a1*z-2.0d0*atan(xl/z)*a1*z-log(xh**2+z**2)*b1
     &            +log(xl**2+z**2)*b1-2.0d0*a1*xh+2.0d0*a1*xl)/2.0d0
                gyy=0.0d0
                gzy=(-(2.0d0*atan(xh/z)*b1+log(xh**2+z**2)*a1*z-(2.0d0*atan(xl/z)*
     &            b1+log(xl**2+z**2)*a1*z)))/2.0d0
              else
c              include 'g.f'
                ans2=-(((b1*vy**3-b1*vy+2.0d0*vx*vy**2*xh+vy**2*vz*z+vz*z)*a1*vx-
     &            (vx*xh+vz*z)*(vy**2+vz**2)*vy-((vy**2+vz**2)*b1-a1**2*vx*vy*xh
     &            )*(vy+1.0d0)*(vy-1.0d0))*log((xh**2+z**2)*vy**2+vz**2*xh**2+(vx*z-
     &            2.0d0*vz*xh)*vx*z-(2.0d0*(vx*xh+vz*z)*vy+(vy+1.0d0)*(vy-1.0d0)*b1)*b1-
     &            ((a1*xh+2.0d0*b1)*(vy+1.0d0)*(vy-1.0d0)+2.0d0*(vx*xh+vz*z)*vy)*a1*xh)+
     &            2.0d0*((b1*vz-vy*z+a1*vx*z)*atan((((a1*xh+b1)*(vy+1.0d0)*(vy-1.0d0)+
     &            (2.0d0*vx*xh+vz*z)*vy)*a1+(b1*vy+vz*z)*vx-(vy**2+vz**2)*xh)/(b1*
     &            vz-vy*z+a1*vx*z))-(a1**2*vy**2-a1**2+2.0d0*a1*vx*vy-vy**2-vz**2)
     &            *xh)*vx*vy)
                ans1=2.0d0*(((a1**2*vy**2-a1**2-vy**2-vz**2)*xh+(b1*vy+vz*z)*vx+(
     &            (2.0d0*vx*xh+vz*z)*vy+(vy+1.0d0)*(vy-1.0d0)*b1)*a1)*atan(((a1*xh+b1)
     &            *(vy+1.0d0)*(vy-1.0d0)+(vx*xh+vz*z)*vy)/(vx*z-vz*xh))-((a1**2*vy**
     &            2-a1**2-vy**2-vz**2)*xl+(b1*vy+vz*z)*vx+((2.0d0*vx*xl+vz*z)*vy+(
     &            vy+1.0d0)*(vy-1.0d0)*b1)*a1)*atan(((a1*xl+b1)*(vy+1.0d0)*(vy-1.0d0)+(
     &            vx*xl+vz*z)*vy)/(vx*z-vz*xl)))*vz+((b1*vy**3-b1*vy+2.0d0*vx*vy**
     &            2*xl+vy**2*vz*z+vz*z)*a1*vx-(vx*xl+vz*z)*(vy**2+vz**2)*vy-((vy
     &            **2+vz**2)*b1-a1**2*vx*vy*xl)*(vy+1.0d0)*(vy-1.0d0))*log((xl**2+z
     &            **2)*vy**2+vz**2*xl**2+(vx*z-2.0d0*vz*xl)*vx*z-(2.0d0*(vx*xl+vz*z)
     &            *vy+(vy+1.0d0)*(vy-1.0d0)*b1)*b1-((a1*xl+2.0d0*b1)*(vy+1.0d0)*(vy-1.0d0)
     &            +2.0d0*(vx*xl+vz*z)*vy)*a1*xl)+2.0d0*((b1*vz-vy*z+a1*vx*z)*atan(((
     &            (a1*xl+b1)*(vy+1.0d0)*(vy-1.0d0)+(2.0d0*vx*xl+vz*z)*vy)*a1+(b1*vy+vz
     &            *z)*vx-(vy**2+vz**2)*xl)/(b1*vz-vy*z+a1*vx*z))-(a1**2*vy**2-a1
     &            **2+2.0d0*a1*vx*vy-vy**2-vz**2)*xl)*vx*vy+ans2
                gxy=ans1/(2.0d0*(a1**2*vy**2-a1**2+2.0d0*a1*vx*vy-vy**2-vz**2)*(vy+
     &            1.0d0)*(vy-1.0d0))
                ans1=((a1**2*vy**2-a1**2-vy**2-vz**2)*xl+(b1*vy+vz*z)*vx+((2.0d0*
     &            vx*xl+vz*z)*vy+(vy+1.0d0)*(vy-1.0d0)*b1)*a1)*log((xl**2+z**2)*vy**
     &            2+vz**2*xl**2+(vx*z-2.0d0*vz*xl)*vx*z-(2.0d0*(vx*xl+vz*z)*vy+(vy+
     &            1.0d0)*(vy-1.0d0)*b1)*b1-((a1*xl+2.0d0*b1)*(vy+1.0d0)*(vy-1.0d0)+2.0d0*(vx
     &            *xl+vz*z)*vy)*a1*xl)-2.0d0*((b1*vz-vy*z+a1*vx*z)*(atan((((a1*xh+
     &            b1)*(vy+1.0d0)*(vy-1.0d0)+(2.0d0*vx*xh+vz*z)*vy)*a1+(b1*vy+vz*z)*vx-
     &            (vy**2+vz**2)*xh)/(b1*vz-vy*z+a1*vx*z))-atan((((a1*xl+b1)*(vy+
     &            1.0d0)*(vy-1.0d0)+(2.0d0*vx*xl+vz*z)*vy)*a1+(b1*vy+vz*z)*vx-(vy**2+
     &            vz**2)*xl)/(b1*vz-vy*z+a1*vx*z)))-(a1**2*vy**2-a1**2+2.0d0*a1*vx
     &            *vy-vy**2-vz**2)*(xh-xl))-((a1**2*vy**2-a1**2-vy**2-vz**2)*xh+
     &            (b1*vy+vz*z)*vx+((2.0d0*vx*xh+vz*z)*vy+(vy+1.0d0)*(vy-1.0d0)*b1)*a1)
     &            *log((xh**2+z**2)*vy**2+vz**2*xh**2+(vx*z-2.0d0*vz*xh)*vx*z-(2.0d0
     &            *(vx*xh+vz*z)*vy+(vy+1.0d0)*(vy-1.0d0)*b1)*b1-((a1*xh+2.0d0*b1)*(vy+
     &            1.0d0)*(vy-1.0d0)+2.0d0*(vx*xh+vz*z)*vy)*a1*xh)
                gyy=ans1/(2.0d0*(a1**2*vy**2-a1**2+2.0d0*a1*vx*vy-vy**2-vz**2))
                ans4=2.0d0*((b1*vz-vy*z+a1*vx*z)*atan((((a1*xh+b1)*(vy+1.0d0)*(vy-
     &            1.0d0)+(2.0d0*vx*xh+vz*z)*vy)*a1+(b1*vy+vz*z)*vx-(vy**2+vz**2)*xh)
     &            /(b1*vz-vy*z+a1*vx*z))-(a1**2*vy**2-a1**2+2.0d0*a1*vx*vy-vy**2-
     &            vz**2)*xh)*vy*vz
                ans3=-(((b1*vy**3*vz-b1*vy*vz+2.0d0*vx*vy**2*vz*xl+vy**2*vz**2*z+
     &            vy**2*z+vz**2*z-z)*a1+(vx*vz**2*z+vx*z-vy**2*vz*xl-vz**3*xl)*
     &            vy+(a1**2*vy*xl+b1*vx)*(vy+1.0d0)*(vy-1.0d0)*vz)*log((xl**2+z**2)*
     &            vy**2+vz**2*xl**2+(vx*z-2.0d0*vz*xl)*vx*z-(2.0d0*(vx*xl+vz*z)*vy+(
     &            vy+1.0d0)*(vy-1.0d0)*b1)*b1-((a1*xl+2.0d0*b1)*(vy+1.0d0)*(vy-1.0d0)+2.0d0*
     &            (vx*xl+vz*z)*vy)*a1*xl)+2.0d0*((b1*vz-vy*z+a1*vx*z)*atan((((a1*
     &            xl+b1)*(vy+1.0d0)*(vy-1.0d0)+(2.0d0*vx*xl+vz*z)*vy)*a1+(b1*vy+vz*z)*
     &            vx-(vy**2+vz**2)*xl)/(b1*vz-vy*z+a1*vx*z))-(a1**2*vy**2-a1**2+
     &            2.0d0*a1*vx*vy-vy**2-vz**2)*xl)*vy*vz)+((b1*vy**3*vz-b1*vy*vz+
     &            2.0d0*vx*vy**2*vz*xh+vy**2*vz**2*z+vy**2*z+vz**2*z-z)*a1+(vx*vz
     &            **2*z+vx*z-vy**2*vz*xh-vz**3*xh)*vy+(a1**2*vy*xh+b1*vx)*(vy+
     &            1.0d0)*(vy-1.0d0)*vz)*log((xh**2+z**2)*vy**2+vz**2*xh**2+(vx*z-2.0d0
     &            *vz*xh)*vx*z-(2.0d0*(vx*xh+vz*z)*vy+(vy+1.0d0)*(vy-1.0d0)*b1)*b1-((
     &            a1*xh+2.0d0*b1)*(vy+1.0d0)*(vy-1.0d0)+2.0d0*(vx*xh+vz*z)*vy)*a1*xh)+
     &            ans4
                ans2=2.0d0*(((a1**2*vy**2-a1**2-vy**2-vz**2)*xh+(b1*vy+vz*z)*vx+(
     &            (2.0d0*vx*xh+vz*z)*vy+(vy+1.0d0)*(vy-1.0d0)*b1)*a1)*atan(((a1*xh+b1)
     &            *(vy+1.0d0)*(vy-1.0d0)+(vx*xh+vz*z)*vy)/(vx*z-vz*xh))-((a1**2*vy**
     &            2-a1**2-vy**2-vz**2)*xl+(b1*vy+vz*z)*vx+((2.0d0*vx*xl+vz*z)*vy+(
     &            vy+1.0d0)*(vy-1.0d0)*b1)*a1)*atan(((a1*xl+b1)*(vy+1.0d0)*(vy-1.0d0)+(
     &            vx*xl+vz*z)*vy)/(vx*z-vz*xl)))*vx+ans3
                ans1=-ans2
                gzy=ans1/(2.0d0*(a1**2*vy**2-a1**2+2.0d0*a1*vx*vy-vy**2-vz**2)*(vy+
     &            1.0d0)*(vy-1.0d0))
              endif
            else  !a1

              if (abs(vy).lt.eps.and.abs(vz).lt.eps) then
c              if (vy.eq.0.0d0.and.vz.eq.0.0d0) then
c                include 'g0_nur_vx.f'
                gxy=0.0d0
                gyy=(-(xh-xl)*log(yh**2+z**2))/2.0d0
                gzy=-(xh-xl)*atan(yh/z)

              else if (abs(vx).lt.eps.and.abs(vy).lt.eps) then
c              else if (vx.eq.0.0d0.and.vy.eq.0.0d0) then
c                include 'g0_nur_vz.f'
                gxy=(-(2.0d0*atan(yh/xh)*xh+log(xh**2+yh**2)*yh-(2.0d0*atan(yh/xl)*
     &            xl+log(xl**2+yh**2)*yh)))/2.0d0
                gyy=(-((log(xh**2+yh**2)-2.0d0)*xh+2.0d0*atan(xh/yh)*yh-((log(xl**2
     &            +yh**2)-2.0d0)*xl+2.0d0*atan(xl/yh)*yh)))/2.0d0
                gzy=0.0d0

              else if (abs(vx).lt.eps.and.abs(vz).lt.eps) then
c              else if (vx.eq.0.0d0.and.vz.eq.0.0d0) then
c                include 'g0_nur_vy.f'
                gxy=(-(log(xh**2+z**2)-log(xl**2+z**2))*yh)/2.0d0
                gyy=0.0d0
                gzy=-(atan(xh/z)-atan(xl/z))*yh
              else
c                include 'g0.f'
                gxy=(2.0d0*(((vy**2+vz**2)*xl-(vy*yh+vz*z)*vx)*atan(((vx*yh-vy*xl
     &            )*vx-(vy*z-vz*yh)*vz)/(vx*z-vz*xl))*vz-((vy**2+vz**2)*xl-(vy*z
     &            -vz*yh)*atan(((vy**2+vz**2)*xl-(vy*yh+vz*z)*vx)/(vy*z-vz*yh)))
     &            *vx*vy+((vy**2+vz**2)*xh-(vy*z-vz*yh)*atan(((vy**2+vz**2)*xh-(
     &            vy*yh+vz*z)*vx)/(vy*z-vz*yh)))*vx*vy-((vy**2+vz**2)*xh-(vy*yh+
     &            vz*z)*vx)*atan(((vx*yh-vy*xh)*vx-(vy*z-vz*yh)*vz)/(vx*z-vz*xh)
     &            )*vz)-((vx*yh-vy*xl)*vx-(vy*z-vz*yh)*vz)*(vy**2+vz**2)*log(((
     &            xl**2+z**2)*vy-2.0d0*vz*yh*z)*vy+(xl**2+yh**2)*vz**2-(2.0d0*(vy*yh
     &            +vz*z)*xl-(yh**2+z**2)*vx)*vx)+((vx*yh-vy*xh)*vx-(vy*z-vz*yh)*
     &            vz)*(vy**2+vz**2)*log(((xh**2+z**2)*vy-2.0d0*vz*yh*z)*vy+(xh**2+
     &            yh**2)*vz**2-(2.0d0*(vy*yh+vz*z)*xh-(yh**2+z**2)*vx)*vx))/(2.0d0*(
     &            vy**2+vz**2)*(vy+1.0d0)*(vy-1.0d0))
                gyy=(((vy**2+vz**2)*xl-(vy*yh+vz*z)*vx)*log(((xl**2+z**2)*vy-
     &            2.0d0*vz*yh*z)*vy+(xl**2+yh**2)*vz**2-(2.0d0*(vy*yh+vz*z)*xl-(yh**
     &            2+z**2)*vx)*vx)-2.0d0*((vy**2+vz**2)*xl-(vy*z-vz*yh)*atan(((vy**
     &            2+vz**2)*xl-(vy*yh+vz*z)*vx)/(vy*z-vz*yh)))+2.0d0*((vy**2+vz**2)
     &            *xh-(vy*z-vz*yh)*atan(((vy**2+vz**2)*xh-(vy*yh+vz*z)*vx)/(vy*z
     &            -vz*yh)))-((vy**2+vz**2)*xh-(vy*yh+vz*z)*vx)*log(((xh**2+z**2)
     &            *vy-2.0d0*vz*yh*z)*vy+(xh**2+yh**2)*vz**2-(2.0d0*(vy*yh+vz*z)*xh-(
     &            yh**2+z**2)*vx)*vx))/(2.0d0*(vy**2+vz**2))
                gzy=((vx*vy**2*vz*yh+vx*vy*vz**2*z+vx*vy*z-vx*vz*yh-vy**3*vz*xh
     &            -vy*vz**3*xh)*log(((xh**2+z**2)*vy-2.0d0*vz*yh*z)*vy+(xh**2+yh**
     &            2)*vz**2-(2.0d0*(vy*yh+vz*z)*xh-(yh**2+z**2)*vx)*vx)-(vx*vy**2*
     &            vz*yh+vx*vy*vz**2*z+vx*vy*z-vx*vz*yh-vy**3*vz*xl-vy*vz**3*xl)*
     &            log(((xl**2+z**2)*vy-2.0d0*vz*yh*z)*vy+(xl**2+yh**2)*vz**2-(2.0d0*
     &            (vy*yh+vz*z)*xl-(yh**2+z**2)*vx)*vx)-2.0d0*((vy**2+vz**2)*xl-(vy
     &            *z-vz*yh)*atan(((vy**2+vz**2)*xl-(vy*yh+vz*z)*vx)/(vy*z-vz*yh)
     &            ))*vy*vz-2.0d0*((vy**2+vz**2)*xl-(vy*yh+vz*z)*vx)*atan(((vx*yh-
     &            vy*xl)*vx-(vy*z-vz*yh)*vz)/(vx*z-vz*xl))*vx+2.0d0*((vy**2+vz**2)
     &            *xh-(vy*z-vz*yh)*atan(((vy**2+vz**2)*xh-(vy*yh+vz*z)*vx)/(vy*z
     &            -vz*yh)))*vy*vz+2.0d0*((vy**2+vz**2)*xh-(vy*yh+vz*z)*vx)*atan(((
     &            vx*yh-vy*xh)*vx-(vy*z-vz*yh)*vz)/(vx*z-vz*xh))*vx)/(2.0d0*(vy**2
     &            +vz**2)*(vy+1.0d0)*(vy-1.0d0))
              endif
            endif

            gxys=gxys+gxy
            gyys=gyys+gyy
            gzys=gzys+gzy

          enddo !ncorn

          if (kfail.ne.0) then
            ifail=kfail
          endif


          chargedens=chargedens/twopi
          g(1)=gxys
          g(2)=gyys
          g(3)=gzys

          ! We rotate back form the xy-plane

          bintx=tsinv(1,1)*g(1)+tsinv(1,2)*g(2)+tsinv(1,3)*g(3)
          binty=tsinv(2,1)*g(1)+tsinv(2,2)*g(2)+tsinv(2,3)*g(3)
          bintz=tsinv(3,1)*g(1)+tsinv(3,2)*g(2)+tsinv(3,3)*g(3)

          if (bintx.ne.bintx) then
            lfail=lfail+1
          else
            bintxo=bintxo+bintx*chargedens
          endif

          if (binty.ne.binty) then
            lfail=lfail+1
          else
            bintyo=bintyo+binty*chargedens
          endif

          if (bintz.ne.bintz) then
            lfail=lfail+1
          else
            bintzo=bintzo+bintz*chargedens
          endif

        enddo !nplan

      enddo !imag

      bintxo=bintxo/1000.0d0
      bintyo=bintyo/1000.0d0
      bintzo=bintzo/1000.0d0

9999  continue

      deallocate (corn,vnor,corny)

      if (lfail.ne.0) then
        print*
        print*,"*** Warning in undumag_bintinf: NaN found for at least one voxel ***"
        print*
        call sleep(3)
      endif

      ifail=ifail+lfail

      return
      end
