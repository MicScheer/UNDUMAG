*CMZ :  2.04/14 06/09/2023  07.07.15  by  Michael Scheer
*-- Author :
      subroutine clcmag_dump_hull_of_magnet(imag,lund)

      use magnets_structure

      implicit none

      integer imag,lund,k,l,ipoi,npoi

      Type(T_Magnet) tmag

      tmag=t_magnets(imag)

      if (lund.gt.200.or.lund.lt.10) lund=10+imag

      write(lund,'(a)') '* ' // tmag%cnam // ' ' // tmag%cmoth // ' ' // tmag%ctype

      k=1
      do l=1,tmag%kfacelast
        if(k.gt.tmag%kfacelast-3) exit
        npoi=tmag%kface(k)
        k=k+1
        do ipoi=1,npoi
          write(lund,*)
     &      tmag%xhull(tmag%kface(k)),
     &      tmag%yhull(tmag%kface(k)),
     &      tmag%zhull(tmag%kface(k)),
     &      tmag%gcen(1),
     &      tmag%gcen(2),
     &      tmag%gcen(3),
     &      ipoi
          k=k+1
        enddo
      enddo

      flush(lund)
      close(lund)

      return
      end
