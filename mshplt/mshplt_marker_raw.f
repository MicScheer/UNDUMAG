*CMZ :  1.00/01 23/09/2014  19.03.09  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  09.38.20  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 24/08/2014  14.25.20  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.51.37  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_marker_raw(n,x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(*),y(*),x1,y1,yoffset,r
      integer n,i,mtyp
      character(2048) cline
      character(4) chmark

      itouched_ps=1

      if (mtyp_ps.eq.9) then
        do i=1,n
          r=rmsiz_ps*scaletxt_ps
          call mshplt_circle_raw(x(i),y(i),r)
        enddo
        return
      endif

      mtyp=nint(rmtyp_ps(1,mtyp_ps))
      chmark=chch_ps(mtyp)
      yoffset=rmtyp_ps(2,mtyp_ps)*rmsiz_ps*scaletxt_ps

      write(cline,'(a,f12.5,a)')'/Symbol findfont ',rmsiz_ps*scaletxt_ps,
     &  ' scalefont setfont'
      call mshplt_fill_buff(cline)

      do i=1,n

        x1=xleft_ps+scalex_ps*(x(i)-wxmin_ps)
        y1=ybottom_ps+scaley_ps*(y(i)-wymin_ps)

        write(cline,'(2f12.5,a,f12.5,a)')x1,y1,
     &    ' moveto ('//chmark(1:len_trim(chmark))
     &    //') dup stringwidth pop 2 div neg 0 rmoveto 0 ',
     &    yoffset,' rmoveto show'
        call mshplt_fill_buff(cline)

      enddo

      write(cline,'(a,f12.5,a)')'/Helvetica findfont ',chhe_ps,
     &  ' scalefont setfont'
      call mshplt_fill_buff(cline)

      return
      end
