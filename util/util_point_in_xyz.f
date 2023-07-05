*CMZ :  2.02/01 04/02/2022  12.57.55  by  Michael Scheer
*-- Author :    Michael Scheer   03/02/2022
      subroutine util_point_in_xyz(ndim,x,y,z,p,tiny,ipoi)

      implicit none

      integer ndim,ipoi,i
      double precision x(ndim),y(ndim),z(ndim),p(3),tiny,xyz(3),t2

      t2=tiny*tiny

      ipoi=0
      do i=1,ndim
        if ((x(i)-p(1))**2+(y(i)-p(2))**2-(z(i)-p(3))**2.le.t2) then
          ipoi=i
          exit
        endif
      enddo

      return
      end
