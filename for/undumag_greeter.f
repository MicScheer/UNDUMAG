*CMZ :  2.02/01 28/09/2023  13.26.09  by  Michael Scheer
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
      write(lun6,*)'     *                    Version 2.04/26                     *'
      write(lun6,*)'     *                        28.9.2023                       *'
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
      chuvers="2.04/26"
      write(999,*)chuvers
      close(999)
*KEND.
      return
      end
