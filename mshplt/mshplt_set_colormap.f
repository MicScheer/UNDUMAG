*CMZ :  1.03/03 03/02/2025  18.05.05  by  Michael Scheer
*CMZ :  1.02/01 03/10/2014  14.42.26  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.10.09  by  Michael Scheer
*CMZ :  0.00/04 08/08/2014  16.20.54  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_colormap(imap)

      use cmapmod

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.
c+seq,cmap.

      integer imap

      kcolormap=imap

      cmaps(1:3,1:256,1)=cmapviridis(1:3,1:256)
      cmaps(1:3,1:256,2)=cmapjet(1:3,1:256)

      if (imap.lt.1.or.imap.gt.2) then
        print*,"*** Error in mshplt_set_colormap: Map index out of range [1,2]"
        cmap(1:3,1:256)=cmaps(1:3,1:256,1)
        kcolormap=1
      else
        cmap(1:3,1:256)=cmaps(1:3,1:256,imap)
      endif

      return
      end
