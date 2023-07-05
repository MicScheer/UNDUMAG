*CMZ :  1.02/00 30/09/2014  11.27.05  by  Michael Scheer
*CMZ :  1.00/01 24/09/2014  09.11.35  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  13.49.18  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 05/08/2014  15.53.35  by  Michael Scheer
*CMZ :  0.00/02 08/07/2014  11.10.50  by  Michael Scheer
*CMZ :  0.00/01 07/07/2014  12.00.23  by  Michael Scheer
*CMZ : 00.00/02 04/07/2014  21.11.57  by  Michael Scheer
*-- Author :    Michael Scheer   11/06/2014
      subroutine mshplt_view_axis(vxmin,vxmax,vymin,vymax,smin,smax,title,
     &  iticside,anglab,titang,offtit,offlab)

c Draw an axis from (vxmin,vxmax) to (vymin,vymax) in view system
c with labeling from smin to smax, and title title

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real
     &  vxmin,vxmax,vymin,vymax,anglab,
     &  smin,smax,titang,offtit,offlab

      real sizlab(100),xlabrel(100),ylaboffset(100),anglabrel(100),ticlen(100),
     &  ticposrel(100),titoff,titangrel,ticangrel(100),titsiz,titposrel

      integer iticside
      integer nlab,ntic

      character(*) title
      character(12) chlab(100)

      nlab=-100
      sizlab=aLabHeight_ps*(1-inolabs_ps)
      ylaboffset=offlab
      anglabrel=anglab
      ticlen=ticsiz_ps
      if (iticside.lt.0) ticlen=-ticlen
      ticangrel=90.
      titsiz=tsiz_ps*(1-inolabs_ps)
      titoff=offtit
      titangrel=titang
      titposrel=0.5

      call mshplt_axis_taylor(
     &  vxmin,vxmax,vymin,vymax,smin,smax,
     &  nlab,chlab,sizlab,xlabrel,ylaboffset,
     &  xoffexp_ps,yoffexp_ps,
     &  anglabrel,
     &  ntic,ticlen,ticposrel,ticangrel,
     &  titsiz,titposrel,titoff,titangrel,title
     &  )

      return
      end
