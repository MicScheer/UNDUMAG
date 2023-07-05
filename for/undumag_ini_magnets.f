*CMZ :  2.04/02 25/02/2023  14.48.14  by  Michael Scheer
*CMZ :  2.04/00 25/02/2023  14.36.37  by  Michael Scheer
*CMZ :  2.02/02 02/03/2022  13.26.45  by  Michael Scheer
*CMZ :  2.02/01 11/02/2022  09.49.54  by  Michael Scheer
*-- Author :    Michael Scheer   01/04/2016
      subroutine undumag_ini_magnets(kseg)

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      integer kseg

c      print*," "
c      print*," "
c      print*,"Baustellen:"
c      print*,"bpebc(18:20),corrtiny, dedgefb, simpson-file, chicut und andere Variablen aus undumag_ini_old.Randommechnismus, spez. mit Blick auf 1. Integral"
      print*," "

      ! Evaluate buffer of coils
      call clccoil_to_coils

      if (kseg.eq.2) return

      call undumag_read_modules

      ! Evaluate buffer of magnets and poles
      call clcbuff_to_magnets

      ! Apply translations, rotations, and copying of magnets and poles
      if (ntransrotcop.ne.0) call clctransrotcop

      ! Drop zero magnets , after the call, xhull, yhull, zhull refer to gcen
      call clcmag_drop_zero_magnets

      ! Coating of magnets
      call clcmag_shrink_magnets

      ! Mothers
      call clcmag_mothers

      ! inhomogenities of magnets
      call clcmag_inhom

      ! Evaluate modules
      call clcmag_copy_magnets

      ! Apply longitudinal symmetry and center set-up
      call clcmag_sym

      ! segmentation of magnets
      call clcmag_cut

      ! convert to internal arrays for undumag_proc and undumag_end
      call clcmag_to_bpe
      call undumag_ini_bpetm

      ! write set-up to lists
      call clcmag_magnets_list

      if (kseg.ne.0) return

      call clcmag_ini_force

      return
      end
