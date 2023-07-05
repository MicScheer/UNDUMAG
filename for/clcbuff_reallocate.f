*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 05/03/2023  14.34.23  by  Michael Scheer
*-- Author :    Michael Scheer   05/03/2023
      subroutine clcbuff_reallocate

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      deallocate(
     &  xpuffer1,ypuffer1,zpuffer1,
     &  xpuffer2,ypuffer2,zpuffer2,
     &  xpuffer3,ypuffer3,zpuffer3)

      allocate(
     &  xpuffer1(ncornmax),ypuffer1(ncornmax),zpuffer1(ncornmax),
     &  xpuffer2(ncornmax),ypuffer2(ncornmax),zpuffer2(ncornmax),
     &  xpuffer3(ncornmax),ypuffer3(ncornmax),zpuffer3(ncornmax))

      return
      end
