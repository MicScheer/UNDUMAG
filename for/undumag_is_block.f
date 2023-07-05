*CMZ :  2.03/00 31/07/2022  18.33.07  by  Michael Scheer
*CMZ :  2.02/01 27/10/2021  12.44.25  by  Michael Scheer
*-- Author :    Michael Scheer   26/10/2021
      function iundumag_is_block(x,y,z,tiny)

      implicit none

      double precision tiny,x(8),y(8),z(8),v(3,6),p1(3),p2(3),p3(3),v3(3)

      integer iundumag_is_block ! function

      integer, parameter :: npoip=8
      integer khull(8),kedge(4,2*npoip-2),kface(5*npoip),ifail,iplan,k,kpoi,ipoi,
     &  npoi,nhull,nedge,nface,kfacelast,jplan

      iundumag_is_block=0

      npoi=npoip
      call util_convex_hull_3d(npoi,x,y,z,khull,kedge,kface,nhull,nedge,nface,
     &  kfacelast,tiny,ifail)

      k=0
      do iplan=1,nface
        k=k+1
        if (kface(k).ne.4) return
        k=k+1
        p1(1)=x(kface(k))
        p1(2)=y(kface(k))
        p1(3)=z(kface(k))
        k=k+1
        p2(1)=x(kface(k))
        p2(2)=y(kface(k))
        p2(3)=z(kface(k))
        k=k+1
        p3(1)=x(kface(k))
        p3(2)=y(kface(k))
        p3(3)=z(kface(k))
        call util_vnorm_of_plane(p1,p2,p3,v(1:3,iplan),ifail)
        k=k+1
      enddo !iplan

      do iplan=1,6
        do jplan=iplan+1,6
          if (
     &        dot_product(v(:,iplan),v(:,jplan)).ne.0.0d0.and.
     &        abs(dot_product(v(:,iplan),v(:,jplan))+1.0d0).gt.1.0d-15
     &        ) then
            return
          endif
        enddo
      enddo

      iundumag_is_block=1

      return
      end
