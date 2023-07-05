*CMZ :  2.02/01 04/02/2022  13.00.08  by  Michael Scheer
*-- Author :    Michael Scheer   03/02/2022
      subroutine util_point_in_array(ndim,xyz,p,tiny,ipoi)

      implicit none

      integer ndim,ipoi,i
      double precision xyz(3,ndim),p(3),tiny,t2

      t2=tiny*tiny

      ipoi=0
      do i=1,ndim
        if ((xyz(1,i)-p(1))**2+(xyz(2,i)-p(2))**2-(xyz(3,i)-p(3))**2.le.t2) then
          ipoi=i
          exit
        endif
      enddo

      return
      end
