*CMZ :  1.11/05 07/02/2017  14.27.25  by  Michael Scheer
*CMZ :  1.11/04 21/01/2017  16.50.32  by  Michael Scheer
*-- Author :    Michael Scheer   20/01/2017
      subroutine util_weed_points_2d(npoi,x,y,tolerance)

      implicit none

      double precision x(npoi),y(npoi),tolerance
      integer npoi,i,ifound,mpoi,k

      mpoi=0
      do i=1,npoi
        ifound=0
        do k=1,mpoi
          if (
     &        abs(x(k)-x(i)).lt.tolerance.and.
     &        abs(y(k)-y(i)).lt.tolerance) then
            ifound=k
            exit
          endif
        enddo
        if (ifound.eq.0) then
          mpoi=mpoi+1
          x(mpoi)=x(i)
          y(mpoi)=y(i)
        endif
      enddo

      npoi=mpoi

      return
      end
