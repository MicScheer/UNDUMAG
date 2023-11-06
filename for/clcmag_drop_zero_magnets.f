*CMZ :  2.05/02 31/10/2023  13.50.19  by  Michael Scheer
*CMZ :  2.04/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.03/00 23/07/2022  09.34.17  by  Michael Scheer
*CMZ :  2.02/01 26/01/2022  20.00.07  by  Michael Scheer
*-- Author :    Michael Scheer   25/10/2021
      subroutine clcmag_drop_zero_magnets

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure

      implicit none

      ! Drop zero magnets
      ! At entrance xhull,yhull,zhull refer to lab-system.
      ! At exit to gcen of the magnet or pole

      type (T_Magnet) tmag

      integer nmagl,nspecl,mmag,imag,ipoi


      nmagl=0
      nspecl=0
      nmagtot_t=nmag_t+nspecmag_t

      mmag=0
      do imag=1,nmagtot_t

        tmag=t_magnets(imag)

        if (tmag%IsPole.eq.0.and.tmag%Brn.eq.0.0d0.or.tmag%imat.eq.0) cycle

        if (tmag%Isspecial.eq.0) then
          nmagl=nmagl+1
        else
          nspecl=nspecl+1
        endif

        mmag=mmag+1

        do ipoi=1,tmag%nhull
          tmag%xhull(ipoi)=tmag%xhull(ipoi)-tmag%gcen(1)
          tmag%yhull(ipoi)=tmag%yhull(ipoi)-tmag%gcen(2)
          tmag%zhull(ipoi)=tmag%zhull(ipoi)-tmag%gcen(3)
        enddo

        t_magnets(mmag)=tmag

      enddo !imag

      nmag_t=nmagl
      nspecmag_t=nspecl
      nmagtot_t=mmag


      return
      end
