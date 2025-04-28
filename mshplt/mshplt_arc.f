*CMZ :  1.03/03 23/01/2025  15.57.45  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  09.22.45  by  Michael Scheer
*CMZ :  0.01/02 15/09/2014  15.09.38  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  18.23.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_arc(x,y,rin,rout,phi1,phi2)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real, parameter :: pi=3.14159,grarad=pi/180.
      real :: x,y,rin,rout,x1,y1,phi1,phi2,xpl(2),ypl(2)

      character(32) cphi1,cphi2,carc

      write(cphi1,*) mod(nint(phi1),360)
      cphi1=trim(adjustl(cphi1))

      write(cphi2,*) mod(nint(phi2),360)
      cphi2=trim(adjustl(cphi2))

      if (log10x_ps.eq.0) then
        x1=xleft_ps+scalex_ps*(x-wxmin_ps)
      else
        x1=xleft_ps+scalex_ps*(alog10(x)-wxmin_ps)
      endif
      if (log10y_ps.eq.0) then
        y1=ybottom_ps+scaley_ps*(y-wymin_ps)
      else
        y1=ybottom_ps+scaley_ps*(alog10(y)-wymin_ps)
      endif

      carc=' ' // trim(cphi1) // ' ' // trim(cphi2) // ' arc stroke'
      write(cline_ps,*)'newpath ',x1,y1,rin*2.*chhe_ps,trim(carc)
      write(cline_ps,*)'newpath ',x1,y1,rout*2.*chhe_ps,trim(carc)

      xpl(1)=rin*cos(phi1*grarad)
      ypl(1)=rin*sin(phi1*grarad)
      xpl(2)=rout*cos(phi1*grarad)
      ypl(2)=rout*sin(phi1*grarad)

      call mshplt_pline(2,xpl,ypl)

      xpl(1)=rin*cos(phi2*grarad)
      ypl(1)=rin*sin(phi2*grarad)
      xpl(2)=rout*cos(phi2*grarad)
      ypl(2)=rout*sin(phi2*grarad)

      call mshplt_pline(2,xpl,ypl)

      call mshplt_fill_buff(cline_ps)

      return
      end
