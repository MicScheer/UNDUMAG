*CMZ :  2.04/07 09/08/2023  16.09.52  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.33.07  by  Michael Scheer
*CMZ :  2.02/01 27/10/2021  12.44.25  by  Michael Scheer
*-- Author :    Michael Scheer   26/10/2021
      function i_tvoxel_is_block(tvoxel)

      use magnets_structure

      implicit none

      double precision v(3,6),p1(3),p2(3),p3(3)

      integer i_tvoxel_is_block ! function

      integer ifail,iplan,k,ipoi,nface,jplan

      Type(T_Voxel) tvoxel

      if (tvoxel%isblock.ne.0) then
        i_tvoxel_is_block=1
        return
      else
        i_tvoxel_is_block=0
      endif

      nface = tvoxel%nface

      if (nface.ne.6) return

      k=0
      do iplan=1,nface
        k=k+1
        if (tvoxel%kface(k).ne.4) return
        k=k+1
        ipoi=tvoxel%kface(k)
        p1(1)=tvoxel%xhull(ipoi)
        p1(2)=tvoxel%yhull(ipoi)
        p1(3)=tvoxel%zhull(ipoi)
        k=k+1
        p2(1)=tvoxel%xhull(ipoi)
        p2(2)=tvoxel%yhull(ipoi)
        p2(3)=tvoxel%zhull(ipoi)
        k=k+1
        p3(1)=tvoxel%xhull(ipoi)
        p3(2)=tvoxel%yhull(ipoi)
        p3(3)=tvoxel%zhull(ipoi)
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

      i_tvoxel_is_block=1

      return
      end
