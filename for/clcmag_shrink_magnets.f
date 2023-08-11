*CMZ :  2.04/08 10/08/2023  20.49.21  by  Michael Scheer
*CMZ :  2.04/03 03/03/2023  14.14.57  by  Michael Scheer
*CMZ :  2.04/01 20/01/2023  12.55.16  by  Michael Scheer
*CMZ :  2.04/00 14/01/2023  11.51.31  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  13.37.13  by  Michael Scheer
*CMZ :  2.02/01 24/01/2022  17.35.48  by  Michael Scheer
*-- Author :    Michael Scheer   25/10/2021
      subroutine clcmag_shrink_magnets

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure

      implicit none

      ! Apply coating to permanent magnets,
      ! i.e. shrinking of the magnet to the effective size

      ! xhull etc. refer to lab-system

      double precision vol

      integer imag,k,l,i,kfail,npoi
      type (T_Magnet) tmag

      if (coating.eq.0.0d0) return

      do k=1,nmagtot_t
        t_magnets(k)%IsPart=0
        do l=1,nmagtot_t
          if (k.ne.l.and.t_magnets(k)%cmoth.eq.t_magnets(l)%cmoth) t_magnets(k)%IsPart=1
        enddo
      enddo

      do imag=1,nmagtot_t

        tmag=t_magnets(imag)
        if (tmag%IsPole.ne.0.or.tmag%ctype.eq.'Cylinder') cycle

        if (tmag%IsPart.ne.0) then
          write(lun6,*)""
          write(lun6,*)"*** Warning in clcmag_shrink_magnets: Shrinking magnet, which is part of the compound mother magnet ***"
          write(lun6,*)"*** Mother, magnet:  ",trim(tmag%cmoth),"  ",trim(tmag%cnam)
          write(lun6,*)""
        endif

        if (tmag%ctype.eq.'Corners'.or.tmag%ctype.eq.'File') then
          call clcmag_shrink_xyz(imag)
        endif

      enddo !imag

      do imag=1,nmagtot_t
        if (t_magnets(imag)%ctype.eq.'Cylinder') cycle
        npoi=t_magnets(imag)%nhull
        do i=1,npoi
          k=t_magnets(imag)%khull(i)
          xpuffer1(i)=t_magnets(imag)%xhull(k)
          ypuffer1(i)=t_magnets(imag)%yhull(k)
          zpuffer1(i)=t_magnets(imag)%zhull(k)
        enddo
        call util_volume(npoi,xpuffer1,ypuffer1,zpuffer1,hulltiny,vol,kfail)
        if (kfail.ne.0) then
          print*,"*** Warning in clcmag_shrink_magnets: Bad return from util_volume ***"
          print*,"*** Magnet: ",t_magnets(imag)%cnam
        else
          t_magnets(imag)%volume=vol
        endif
      enddo

      return
      end
