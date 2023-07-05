*CMZ : 00.00/02 26/01/2004  16.28.03  by  Michael Scheer
*-- Author :    Michael Scheer   26/01/2004
      subroutine util_vnorm(ndim,vin,vout)

      implicit none

      double precision vin(3),vout(3),vn
      integer ndim,i

      vn=0.d0
      do i=1,ndim
        vn=vn+vin(i)*vin(i)
      enddo

      if (vn.ne.0.d0) then
        vn=1.d0/sqrt(vn)
        do i=1,ndim
          vout(i)=vin(i)*vn
        enddo
      else
        do i=1,ndim
          vout(i)=vin(i)
        enddo
        print *
        print *,'*** Warning in util_vnorm: Zero length of vector'
        print *
      endif

      return
      end
