*CMZ :          10/10/2024  11.17.58  by  Michael Scheer
*CMZ :  1.18/02 13/06/2017  12.39.31  by  Michael Scheer
*CMZ :  0.00/00 20/04/2016  12.46.26  by  Michael Scheer
*CMZ :  1.17/02 09/10/2014  14.45.46  by  Michael Scheer
*CMZ :  1.17/01 03/10/2014  09.56.30  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  14.24.21  by  Michael Scheer
*CMZ :  1.12/16 01/06/2007  11.17.50  by  Michael Scheer
*CMZ :  2.00/00 16/08/2004  15.07.36  by  Michael Scheer
*CMZ :  1.03/00 16/08/2004  14.00.31  by  Michael Scheer
*CMZ :  1.02/04 13/08/2004  15.35.19  by  Michael Scheer
*-- Author :    Michael Scheer   13/08/2004
      subroutine undumag_bpolypl2(xpl,ypl,col,ixyz)

*KEEP,BPOLYEDERF90U.

      use bpolyederf90m

*KEND.

      implicit none


      real xpl(2), ypl(2), x(2), y(2), col,rlwidtho
      integer ixyz

      if (nbforcx*nbforcy*nbforcz.eq.0) return

      call muwk(0,0)

      call mgset('PLCI',col)
      call mgset('PMCI',col)
      call mgset('MTYP',31.)
      call mgset('MSCF',5.)

      call mshplt_get_line_width(rlwidtho)
      call mshplt_set_line_width(rlwidtho*2.)

      if (ixyz.eq.12) then
        x(1)=sngl(torqcenxmm)
        y(1)=sngl(torqcenymm)
        call mpm(1,x,y)
        x(1)=sngl(outbox(1,1))
        y(1)=sngl(outbox(1,2))
        x(2)=sngl(outbox(2,1))
        y(2)=sngl(outbox(2,2))
        call mshplt_box(x(1),y(1),x(2),y(2))
        call muwk(0,0)
      else if (ixyz.eq.13) then
        x(1)=sngl(torqcenxmm)
        y(1)=sngl(torqcenzmm)
        call mpm(1,x,y)
        x(1)=sngl(outbox(1,1))
        y(1)=sngl(outbox(1,3))
        x(2)=sngl(outbox(2,1))
        y(2)=sngl(outbox(2,3))
        call mshplt_box(x(1),y(1),x(2),y(2))
      else if (ixyz.eq.23) then
        x(1)=sngl(torqcenzmm)
        y(1)=sngl(torqcenymm)
        call mpm(1,x,y)
        x(1)=sngl(outbox(1,3))
        y(1)=sngl(outbox(1,2))
        x(2)=sngl(outbox(2,3))
        y(2)=sngl(outbox(2,2))
        call mshplt_box(x(1),y(1),x(2),y(2))

      else

        call mgset('PMCI',1.)

        x(1)=xpl(1)
        x(2)=xpl(2)
        y(1)=ypl(1)
        y(2)=ypl(1)
        call mpl(2,x,y)

        x(1)=xpl(1)
        x(2)=xpl(1)
        y(1)=ypl(1)
        y(2)=ypl(2)
        call mpl(2,x,y)

        x(1)=xpl(1)
        x(2)=xpl(2)
        y(1)=ypl(2)
        y(2)=ypl(2)
        call mpl(2,x,y)

        x(1)=xpl(2)
        x(2)=xpl(2)
        y(1)=ypl(1)
        y(2)=ypl(2)
        call mpl(2,x,y)

      endif

      call mshplt_set_line_width(rlwidtho)
      call muwk(0,0)

      return
      end
