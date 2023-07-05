*CMZ :  1.03/01 09/10/2014  14.42.41  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 24/08/2014  14.21.01  by  Michael Scheer
*CMZ :  0.00/04 08/08/2014  16.50.25  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  16.13.29  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_marker_type(ktyp)

      implicit none

      integer mtyp,ktyp

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      mtyp=ktyp

      if (mtyp.eq.-9999) goto 9999

      if (ktyp.eq.20) then
        mtyp=1
      else if (ktyp.eq.24) then
        mtyp=9
      else if (ktyp.eq.31) then
        mtyp=5
      endif

      if (mtyp.lt.0.or.mtyp.gt.9) then
        write(6,*) '*** Error in mshplt_set_marker_type: Range of valid marker types is 0 -> 9'
        return
      endif

      if (mtyp.eq.9) then
        chmarker_ps=chch_ps(nint(rmtyp_ps(1,1)))
      endif

9999  mtyp_ps=mtyp
      return
      end
