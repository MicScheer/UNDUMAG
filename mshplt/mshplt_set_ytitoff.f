*CMZ :          01/08/2018  15.24.42  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_ytitoff(offset)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real offset
      ytitoff_ps=offset

      return
      end
