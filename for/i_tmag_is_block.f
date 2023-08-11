*CMZ :  2.04/07 09/08/2023  16.00.40  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.33.07  by  Michael Scheer
*CMZ :  2.02/01 27/10/2021  12.44.25  by  Michael Scheer
*-- Author :    Michael Scheer   26/10/2021
      function i_tmag_is_block(tmag)

      use magnets_structure

      implicit none

      double precision v(3,6),p1(3),p2(3),p3(3)

      integer i_tmag_is_block ! function

      integer ifail,iplan,k,ipoi,nface,jplan

      Type(T_Magnet) tmag

      if (tmag%isblock.ne.0) then
        i_tmag_is_block=1
        return
      else
        i_tmag_is_block=0
      endif

      nface = tmag%nface

      if (nface.ne.6) return

      k=0
      do iplan=1,nface
        k=k+1
        if (tmag%kface(k).ne.4) return
        k=k+1
        ipoi=tmag%kface(k)
        p1(1)=tmag%xhull(ipoi)
        p1(2)=tmag%yhull(ipoi)
        p1(3)=tmag%zhull(ipoi)
        k=k+1
        p2(1)=tmag%xhull(ipoi)
        p2(2)=tmag%yhull(ipoi)
        p2(3)=tmag%zhull(ipoi)
        k=k+1
        p3(1)=tmag%xhull(ipoi)
        p3(2)=tmag%yhull(ipoi)
        p3(3)=tmag%zhull(ipoi)
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

      i_tmag_is_block=1

      return
      end
