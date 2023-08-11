*CMZ :  2.04/07 09/08/2023  16.16.46  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  19.29.01  by  Michael Scheer
*CMZ :  2.04/02 26/02/2023  21.47.15  by  Michael Scheer
*CMZ :  2.04/00 09/12/2022  11.08.58  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  10.13.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021

      subroutine clcmag_voxel_volume(imag,ivox)

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      type(T_Voxel) tvox

      double precision vol
      integer imag,ivox,kfail
      character(32) ctype

      ctype=t_magnets(imag)%ctype

      tvox=t_magnets(imag)%t_voxels(ivox)

      if (tvox%isblock.eq.0) then
        call util_volume(tvox%nhull,tvox%xhull,tvox%yhull,tvox%zhull,hulltiny,vol,kfail)
      else
        vol=tvox%size(1)*tvox%size(2)*tvox%size(3)
      endif

      t_magnets(imag)%t_voxels(ivox)%volume=vol

      if (kfail.ne.0) then
        write(lun6,*)"*** Error in clcbuff_to_magnets: Subroutine util_volume failed for ",
     &    trim(t_magnets(nmag)%cnam)
        stop
        endif

      return
      end
