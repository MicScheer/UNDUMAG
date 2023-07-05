*CMZ :  3.03/04 06/11/2017  16.07.30  by  Michael Scheer
*CMZ :  3.03/02 20/01/2016  10.34.45  by  Michael Scheer
*CMZ :  3.02/03 27/10/2014  10.39.03  by  Michael Scheer
*-- Author :    Michael Scheer   05/09/2014
      subroutine util_random_set_seed(isize,iseed)

c The actual size of the seed array is at least 64
c If iseed(1:n) are set zero, the behaviour is unclear to me, but it seems
c to reduce the used seed array to iseed(n+1:64), but what else is changed?
c So be careful!

      use iso_fortran_env !, only: int64

      implicit none

      integer isize
      integer iseed(isize),myseed(64)

      if (isize.lt.64) then
        myseed(1:64-isize)=0
        myseed(isize+1:64)=iseed(1:isize)
        call random_seed(put=myseed)
      else
        call random_seed(put=iseed)
      endif

      return
      end
