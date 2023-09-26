*CMZ :  2.04/22 26/09/2023  12.20.19  by  Michael Scheer
*CMZ :  2.04/03 03/03/2023  11.14.37  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  17.33.21  by  Michael Scheer
*CMZ :  2.02/01 03/11/2021  12.49.04  by  Michael Scheer
*CMZ :  2.02/00 16/09/2020  16.08.25  by  Michael Scheer
*-- Author :    Michael Scheer   10/09/2020
      subroutine util_shrink_xyz(m,x,y,z,cen,coat,tiny,ns,xs,ys,zs,kfail)

c +PATCH,//UNDUMAG/UTIL
c +DECK,util_shrink_xyz.

      implicit none

      ! shrink set of points by the coating to xs,ys,zs

      double precision, dimension (:), allocatable :: xr,yr,zr

      double precision x(m),y(m),z(m),cen(3),xs(m),ys(m),zs(m),coat,gcen(3),
     &  tiny,p1(3),p2(3),p3(3),vn(3),gcenr(3),
     &  signum,xx,yy,zz,gcenp(3),xmin,xmax,ymin,ymax,zmin,zmax,
     &  q1(3),q2(3),q3(3),r1(3),r2(3),r3(3),pp1(3),pp2(3),pp3(3)

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull, kface,ik

      integer i,n,kfail,nhull,nedge,nface,kfacelast,m,ns,ipoi,iplan,
     &  kplan(3),ifound,npoi,k,kpoi,l


      kfail=1
      n=m !might be overwritten in util_weed

      xmin=minval(x)
      xmax=maxval(x)
      ymin=minval(y)
      ymax=maxval(y)
      zmin=minval(z)
      zmax=maxval(z)

      if (xmax.eq.xmin) then
        return
      endif

      if (ymax.eq.ymin) then
        return
      endif

      if (zmax.eq.zmin) then
        return
      endif

      allocate(khull(n),kedge(4,n*n),kface((n+1)*n),xr(n),yr(n),zr(n),ik(n))

      call util_convex_hull_3d_overwrite(-9,n,x,y,z,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,tiny,
     &  kfail)


      if (kfail.ne.0) goto 9999

      ns=nhull

      gcen=0.0d0
      do i=1,ns
        gcen(1)=gcen(1)+x(i)
        gcen(2)=gcen(2)+y(i)
        gcen(3)=gcen(3)+z(i)
      enddo
      gcen=gcen/dble(ns)

      if (cen(1).eq.9999.0d0.and.cen(2).eq.9999.0d0.and.cen(3).eq.9999.0d0)
     &  cen=gcen

      gcenr=gcen-cen

      do i=1,ns
        xr(i)=x(i)-cen(1)
        yr(i)=y(i)-cen(2)
        zr(i)=z(i)-cen(3)
      enddo

      do ipoi=1,nhull

        ifound=0
        k=1
        do iplan=1,nface

          npoi=kface(k)

          do kpoi=1,npoi
            if (kface(k+kpoi).eq.ipoi) then
              ifound=ifound+1
              kplan(ifound)=k
              exit
            endif
          enddo !kpoi

          if (ifound.eq.3) then

            l=kplan(1)
            pp1(1)=xr(kface(l+1))
            pp1(2)=yr(kface(l+1))
            pp1(3)=zr(kface(l+1))
            pp2(1)=xr(kface(l+2))
            pp2(2)=yr(kface(l+2))
            pp2(3)=zr(kface(l+2))
            pp3(1)=xr(kface(l+3))
            pp3(2)=yr(kface(l+3))
            pp3(3)=zr(kface(l+3))
            call util_vnorm_of_plane(pp1,pp2,pp3,vn,kfail)
            if (kfail.ne.0) goto 9999
            gcenp=(pp1+pp2+pp3)/3.0d0-gcenr
            if (vn(1)*gcenp(1)+vn(2)*gcenp(2)+vn(3)*gcenp(3).gt.0.0d0) then
              signum=1.0d0
            else
              signum=-1.0d0
            endif
            p1=pp1-vn*signum*coat
            p2=pp2-vn*signum*coat
            p3=pp3-vn*signum*coat

            l=kplan(2)
            pp1(1)=xr(kface(l+1))
            pp1(2)=yr(kface(l+1))
            pp1(3)=zr(kface(l+1))
            pp2(1)=xr(kface(l+2))
            pp2(2)=yr(kface(l+2))
            pp2(3)=zr(kface(l+2))
            pp3(1)=xr(kface(l+3))
            pp3(2)=yr(kface(l+3))
            pp3(3)=zr(kface(l+3))
            call util_vnorm_of_plane(pp1,pp2,pp3,vn,kfail)
            if (kfail.ne.0) goto 9999
            gcenp=(pp1+pp2+pp3)/3.0d0-gcenr
            if (vn(1)*gcenp(1)+vn(2)*gcenp(2)+vn(3)*gcenp(3).gt.0.0d0) then
              signum=1.0d0
            else
              signum=-1.0d0
            endif
            q1=pp1-vn*signum*coat
            q2=pp2-vn*signum*coat
            q3=pp3-vn*signum*coat

            l=kplan(3)
            pp1(1)=xr(kface(l+1))
            pp1(2)=yr(kface(l+1))
            pp1(3)=zr(kface(l+1))
            pp2(1)=xr(kface(l+2))
            pp2(2)=yr(kface(l+2))
            pp2(3)=zr(kface(l+2))
            pp3(1)=xr(kface(l+3))
            pp3(2)=yr(kface(l+3))
            pp3(3)=zr(kface(l+3))
            call util_vnorm_of_plane(pp1,pp2,pp3,vn,kfail)
            if (kfail.ne.0) goto 9999
            gcenp=(pp1+pp2+pp3)/3.0d0-gcenr
            if (vn(1)*gcenp(1)+vn(2)*gcenp(2)+vn(3)*gcenp(3).gt.0.0d0) then
              signum=1.0d0
            else
              signum=-1.0d0
            endif
            r1=pp1-vn*signum*coat
            r2=pp2-vn*signum*coat
            r3=pp3-vn*signum*coat

            call util_common_point_of_3_planes(p1,p2,p3,q1,q2,q3,r1,r2,r3,
     &        xx,yy,zz,kfail)

            if (kfail.ne.0) goto 9999

            xs(ipoi)=xx+cen(1)
            ys(ipoi)=yy+cen(2)
            zs(ipoi)=zz+cen(3)

            exit

          endif !ifound.eq.3

          k=k+1+npoi

        enddo !iplan
      enddo !ipoi

9999  deallocate(khull,kedge,kface,xr,yr,zr,ik)

      kfail=0

      return
      end
