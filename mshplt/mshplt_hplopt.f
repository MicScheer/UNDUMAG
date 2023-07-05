*CMZ :          25/09/2016  13.35.45  by  Michael Scheer
*CMZ :  1.03/01 09/10/2014  15.40.58  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.54.32  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_hplopt(chopt,n)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) chopt
      integer n

      if (n.gt.1) then
        print*,'*** WARNING in mshplt_hplopt: Number of options must not exceed one!'
      endif

      if (chopt.eq.'DATE'.or.chopt.eq.'date') then
        kDate_ps=1
        isDate_ps=1
      else if (chopt.eq.'NDAT'.or.chopt.eq.'ndat') then
        kDate_ps=0
        isDate_ps=1
      else if (chopt.eq.'BOX '.or.chopt.eq.'box ') then
        kBox_ps=1
        isBox_ps=1
      else if (chopt.eq.'NBOX'.or.chopt.eq.'nbox') then
        isBox_ps=1
        kBox_ps=0
      else
        print*,'*** WARNING in mshplt_hplopt: Option ',
     &    chopt(1:len_trim(chopt)),' not available! ***'
      endif

      return
      end
