*CMZ :  2.05/01 03/10/2023  10.48.16  by  Michael Scheer
*CMZ :  1.11/04 21/01/2017  16.50.32  by  Michael Scheer
*-- Author :    Michael Scheer   20/01/2017
      subroutine util_weed_integer_list(npoi,ipois)

      implicit none

      integer npoi,ipois(npoi),nweed,ipoi,ifound,i,k

      nweed=1
      do ipoi=1,npoi
        do i=nweed+1,npoi
          ifound=0
          do k=1,nweed
            if(ipois(k).eq.ipois(i)) then
              ifound=1
              exit
            endif
          enddo
          if (ifound.eq.0) then
            nweed=nweed+1
            ipois(nweed)=ipois(i)
          endif
        enddo
      enddo

      npoi=nweed

      return
      end
