*CMZ :  2.02/01 11/09/2023  11.59.29  by  Michael Scheer
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
      write(lun6,*)'     *                    Version 2.04/16                     *'
      write(lun6,*)'     *                        11.9.2023                       *'
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
      chuvers="2.04/16"
      write(999,*)chuvers
      close(999)
*KEND.
      return
      end
