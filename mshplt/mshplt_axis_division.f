*CMZ :          30/01/2018  14.43.10  by  Michael Scheer
*CMZ :  1.03/01 07/10/2014  15.45.02  by  Michael Scheer
*CMZ :  1.02/00 01/10/2014  08.53.27  by  Michael Scheer
*CMZ :  1.01/02 27/09/2014  12.27.11  by  Michael Scheer
*CMZ :  0.01/02 19/09/2014  17.39.37  by  Michael Scheer
*-- Author :    Michael Scheer   15/09/2014
      subroutine mshplt_axis_division(xmin,xmax,nlabmax,nlab,xlab)

      implicit none

      real xmin,xmax,dx,xhigh,xdiff,rmandiff,xlab(100),rmanmax,x,eps
      integer il,nlab,nexpdiff,nexpmax,nlabmax

      data eps/1.0e-5/

      character(12) chreal

      if (xmax.ne.xmax.or.xmin.ne.xmin) then
        print*,"*** Error in mshplt_axis_division: Xmin or Xmax not a number ***"
        nlab=0
        return
      endif

      if (nlabmax.eq.0) then
        nlab=0
        return
      else if (nlabmax.eq.1) then
        nlab=1
        xlab(1)=(xmax+xmin)/2.
        return
      endif

      if (xmax-xmin.eq.0.0.or.
     &    abs(xmax-xmin).le.1.0e-5*(abs(xmax)+abs(xmin))
     &    ) then
        nlab=2
        xlab(1)=xmin
        xlab(2)=xmax
        return
      endif

      xdiff=max(abs(xmax),abs(xmin))
      xdiff=abs(xmax-xmin)

      write(chreal,'(1pe12.5)')xdiff
      read(chreal(10:12),'(i3)')nexpdiff
      read(chreal(1:8),*)rmandiff

      il=int(rmandiff)

      if (il.eq.1) then
        dx=0.1
        if (int(rmandiff/dx).gt.nlabmax) dx=0.25
      else if (il.eq.2) then
        dx=0.2
        if (int(rmandiff/dx).gt.nlabmax) dx=0.5
      else if (il.eq.3) then
        dx=0.5
        if (int(rmandiff/dx).gt.nlabmax) dx=1.
      else if (il.eq.4) then
        dx=0.5
        if (int(rmandiff/dx).gt.nlabmax) dx=1.
      else if (il.eq.5) then
        dx=1.
        if (int(rmandiff/dx).gt.nlabmax) dx=2.
      else if (il.eq.6) then
        dx=1.
        if (int(rmandiff/dx).gt.nlabmax) dx=2.
      else if (il.eq.7) then
        dx=1.
        if (int(rmandiff/dx).gt.nlabmax) dx=2.5
      else if (il.eq.8) then
        dx=1.
        if (int(rmandiff/dx).gt.nlabmax) dx=2.
      else if (il.eq.9) then
        dx=1.
        if (int(rmandiff/dx).gt.nlabmax) dx=2.5
      else
        dx=0.5
        if (int(rmandiff/dx).gt.nlabmax) dx=1.
      endif

      do while (int(rmandiff/dx).gt.nlabmax)
        if (dx.eq.0.25) then
          dx=0.5
        else if (dx.eq.0.5) then
          dx=1.
        else if (dx.eq.1.) then
          dx=2.5
        else if (dx.eq.2.) then
          dx=5.
        else
          dx=dx*2.
        endif
      enddo

      dx=dx*10.**nexpdiff

      write(chreal,'(1pe12.5)')xmax
      read(chreal(10:12),'(i3)')nexpmax
      read(chreal(1:8),*)rmanmax
      xhigh=int(rmanmax)*10.**nexpmax

      if (xmin*xmax.lt.0.0) xhigh=0.0

      x=xhigh

      if (xhigh.lt.xmin) then
        do while(x.lt.xmin+dx/1000.)
          x=x+dx
        enddo
        xhigh=x
      endif

      x=xhigh

      nlab=0
      do while(x.le.xmax+dx/1000.)
        x=x+dx
        nlab=nlab+1
      enddo

      x=xhigh-dx
      do while(x.ge.xmin-dx/1000.)
        x=x-dx
        nlab=nlab+1
      enddo

      xlab(1)=x+dx
      do il=2,nlab
        xlab(il)=xlab(il-1)+dx
      enddo

      do  il=1,nlab
        if (abs(xlab(il)/dx).lt.eps) xlab(il)=0.0
      enddo

      return
      end
