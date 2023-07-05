*CMZ :  0.01/02 08/09/2014  17.36.39  by  Michael Scheer
*-- Author :    Michael Scheer   08/09/2014
      subroutine mshplt_number(
     &  x,     ! x coordinate of string ( user units )
     &  y,     ! y coordinate of string ( user units )
     &  chhe,     ! height of string ( user units )
     &  rnum,     ! number to be pplotted
     &  ang,      ! angle in respect to x-axis
     &  ndig)  ! number of digits right of decimal point

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real rnum,x,y,chhe,ang
      integer ndig,ifirst

      character(16) chnum
      character(8) chform

      chform(1:4)='(f10.'

      if (ndig.ge.0) then
        chform(1:4)='(f10.'
        write(chform(6:6),'(i1)')ndig
        chform(7:7)=')'
        write(chnum,chform)rnum
      else
        chform(1:5)='(I10)'
        write(chnum,chform)ifix(rnum+0.5)
      endif

      do ifirst=1,16
        if (chnum(ifirst:ifirst).ne.' ') goto 9
      enddo

9     continue

      if (ihigzmode_ps.eq.0) then
        write(lun_ps,*)'/Helvetica findfont',chhe*scaletxt_ps,
     &    ' scalefont setfont'
      else
        write(lun_ps,*)'/Helvetica findfont',chhe*scaletxt_ps*1.3,
     &    ' scalefont setfont'
      endif

      write(lun_ps,*)
     &  x,y,' moveto ',
     &  ang,' rotate ',
     &  '(',chnum(ifirst:len_trim(chnum)),') show',
     &  -ang,' rotate'

      return
      end
