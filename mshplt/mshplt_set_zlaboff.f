*CMZ :          01/08/2018  12.27.21  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_zlaboff(offset)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real offset
      zlaboff_ps=offset

      return
      end
