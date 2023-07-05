*CMZ :  1.03/02 01/04/2016  12.31.53  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  13.50.41  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_hplset(chopt,val)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) chopt
      real val

      if (chopt.eq.'YGTI'.or.chopt.eq.'ygti') then
        ygti_ps=val
        offgtity_ps=val
      else if (chopt.eq.'GSIZ'.or.chopt.eq.'gsiz') then
        gsiz_ps=val
      else if (chopt.eq.'YMGL'.or.chopt.eq.'ymgl') then
        ymgl_ps=val
      else if (chopt.eq.'YMGU'.or.chopt.eq.'ymgu') then
        ymgu_ps=val
      else if (chopt.eq.'XMGL'.or.chopt.eq.'xmgl') then
        xmgl_ps=val
      else if (chopt.eq.'XMGR'.or.chopt.eq.'xmgr') then
        xmgr_ps=val
      else if (chopt.eq.'YWIN'.or.chopt.eq.'ywin') then
        ywin_ps=val
      else if (chopt.eq.'YWIN'.or.chopt.eq.'xwin') then
        xwin_ps=val
      else
        print*,'*** WARNING in mshplt_hplset: Option ',
     &    chopt(1:len_trim(chopt)),' not available! ***'
      endif

      return
      end
