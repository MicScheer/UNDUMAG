*CMZ :          01/08/2018  12.30.47  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_ztitoff(offset)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real offset
      ztitoff_ps=offset

      return
      end
