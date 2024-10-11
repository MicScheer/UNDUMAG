*CMZ :  2.02/01 23/10/2023  14.26.43  by  Michael Scheer
*-- Author :    Michael Scheer   19/10/2021
      subroutine undumag_greeter
      use undumagf90m
      use commandlinef90m
      implicit none
*KEEP,unduver.
      print *
      print *
      print *
      write(lun6,*)'     **********************************************************'
      write(lun6,*)'     *                                                        *'
      write(lun6,*)'     *                       UNDUMAG                          *'
      write(lun6,*)'     *                                                        *'
      write(lun6,*)'     *                    Version 2.05/06                     *'
      write(lun6,*)'     *                       11.10.2024                       *'
      write(lun6,*)'     *                                                        *'
      write(lun6,*)'     *                     Michael Scheer                     *'
      write(lun6,*)'     *                       HZB/BESSY                        *'
      write(lun6,*)'     *                                                        *'
      write(lun6,*)'     **********************************************************'

      print *
      print *
      print *

      call util_zeit_kommentar(lun6,"")

      open(unit=999,file="undumag.ver")
      chuvers="2.05/06"
      write(999,*)chuvers
      close(999)

*KEND.
      return
      end
