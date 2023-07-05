*CMZ : 00.00/16 05/09/2014  12.38.49  by  Michael Scheer
*-- Author :    Michael Scheer   05/09/2014
      subroutine util_random_get_seed(isize,iseed)

      use iso_fortran_env, only: int64

      implicit none

      integer isize
      integer iseed(isize)

      call random_seed(get=iseed)

      return
      end
