*CMZ :  3.06/00 22/01/2019  16.22.02  by  Michael Scheer
*CMZ :  3.03/02 17/11/2015  14.46.41  by  Michael Scheer
*CMZ :  3.02/03 05/09/2014  12.37.33  by  Michael Scheer
*-- Author :    Michael Scheer   05/09/2014
      subroutine util_random_init(isize,iseed)

c isize contains size if seed-array to be provided by the user
c For a simple use of the generated array call random_seed(isize), where
c isize is an integer

C IT SEEMS NOT TO WORK AS EXPECTED, MAYBE THE FIRST HALF OF THE SEEDS MUST BE
C SAVED TO THE SECOND HALF OF THE ARRAY!?

      use iso_fortran_env !, only: int64

      implicit none

      integer isize,n
      integer iseed(isize)

      n=isize
      call random_seed(size=isize)

C      if (isize.ne.64) then
      if (n.ne.64) then
        iseed(1)=-1
        print*,
     &    '*** Error in util_random_init: Dimension of seed-array must be 64:'
c     &    ,isize
        return
      endif

      call random_seed(get=iseed)
      iseed(33:64)=iseed(1:32)

      isize=n

      return
      end
