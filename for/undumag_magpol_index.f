*CMZ :  2.02/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  1.18/02 13/06/2017  08.58.09  by  Michael Scheer
*CMZ :  1.13/03 10/03/2017  18.56.39  by  Michael Scheer
*CMZ :  1.11/04 24/01/2017  16.09.36  by  Michael Scheer
*-- Author :    Michael Scheer   24/01/2017
      subroutine undumag_magpol_index(chmagpol,ind)

      use undumagf90m
      use bpolyederf90m

      use commandlinef90m

      implicit none

      integer imp,ic,ind,kcs,kc

      character(32) chmagpol
      character c1,c1s

      equivalence (kc,c1)
      equivalence (kcs,c1s)

      ind=0

      kc=0
      kcs=0
      do imp=1,nmagpols
        ind=1
        write(lun6,*)imp,chmagpols(1:32,imp)
        do ic=1,32
          c1=chmagpol(ic:ic)
          c1s=chmagpols(ic,imp)
          write(lun6,*)chmagpol(ic:ic),kc,imp,chmagpols(ic,imp),kcs
          if (chmagpols(ic,imp).ne.chmagpol(ic:ic)) then
            ind=0
            exit
          endif
        enddo
        if (ind.eq.1) then
          ind=imp
          exit
        endif
      enddo

 9999 continue
      return
      end
