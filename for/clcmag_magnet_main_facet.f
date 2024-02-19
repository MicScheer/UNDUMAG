*CMZ :          04/09/2023  16.33.39  by  Michael Scheer
*CMZ :  2.04/13 03/09/2023  20.28.53  by  Michael Scheer
*CMZ :  2.04/09 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/08 11/08/2023  12.58.25  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  12.43.30  by  Michael Scheer
*CMZ :  2.04/06 04/08/2023  11.32.02  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  19.30.13  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  20.28.35  by  Michael Scheer
*CMZ :  2.04/00 09/12/2022  11.08.58  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  10.13.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_magnet_main_facet

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      Type(T_Magnet) tmag

      integer :: imag,luno,npoi,i,l,j,iv=0,k

      open(newunit=luno,file='undumag_main_facets.fct')

      write(luno,*)nmag_t+nspecmag_t

      do imag=1,nmag_t+nspecmag_t
        k=1
        do l=1,tmag%kfacelast
          npoi=tmag%kface(k)
          write(luno,*)npoi,tmag%icol,tmag%icol,iv,imag,tmag%cnam,tmag%cmoth
          do j=1,npoi
            k=k+1
            i=tmag%kface(k)
            write(luno,*)
     &        tmag%xhull(i)+tmag%gcen(1),
     &        tmag%yhull(i)+tmag%gcen(2),
     &        tmag%zhull(i)+tmag%gcen(3)
          enddo
          k=k+1
          if (k.gt.tmag%kfacelast) exit
        enddo
      enddo

      flush(luno)
      close(luno)

      return
      end
