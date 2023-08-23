*CMZ :  2.04/02 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/01 13/02/2023  13.19.45  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.19.49  by  Michael Scheer
*CMZ :  2.02/02 15/02/2022  16.00.05  by  Michael Scheer
*CMZ :  2.02/01 23/01/2022  14.52.10  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_check_orient(imag)

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision x,y,z,gcen(3),p1(3),p2(3),p3(3),vnormal(3),dist

      integer :: kdebug=1,
     &  l,iplan,n,ivox,imag,nvox,ip,j,iover,istat

      type(T_Magnet) :: tmag
      type(T_Voxel) :: tvox

      if (kdebug.gt.0) call util_break

      tmag=t_magnets(imag)
      gcen=t_magnets(imag)%gcen

      l=0
      ivox=0
      do iplan=1,tmag%nface
        l=l+1
        n=tmag%kface(l)
        do j=1,n
          l=l+1
          ip=tmag%kface(l)
          x=tmag%xhull(ip)
          y=tmag%yhull(ip)
          z=tmag%zhull(ip)
          vnormal=0.0d0
          if (j.eq.1) then
            p1=[x,y,z]
          else if (j.eq.2) then
            p2=[x,y,z]
          else if (j.eq.3) then
            p3=[x,y,z]
            call util_plane(p1,p2,p3,tmag%gcen,vnormal,dist,iover,istat)
          endif
          if (kdebug.ne.0) then
            write(802,*) imag,ivox,iplan,j,l,x,y,z,vnormal
          else
            exit
          endif
        enddo
      enddo

      nvox=t_magnets(imag)%nvoxels

      do ivox=1,nvox
        tvox=t_magnets(imag)%t_voxels(ivox)
        l=0
        do iplan=1,tvox%nface
          l=l+1
          n=tvox%kface(l)
          do j=1,n
            l=l+1
            ip=tvox%kface(l)
            x=tvox%xhull(ip)
            y=tvox%yhull(ip)
            z=tvox%zhull(ip)
            if (j.eq.1) then
              p1=[x,y,z]
            else if (j.eq.2) then
              p2=[x,y,z]
            else if (j.eq.3) then
              p3=[x,y,z]
            endif
            call util_plane(p1,p2,p3,tvox%gcen,vnormal,dist,iover,istat)
            if (kdebug.ne.0) then
              write(802,*) imag,ivox,iplan,j,l,x,y,z,vnormal
            else
              exit
            endif
          enddo
        enddo
      enddo

      return
      end
