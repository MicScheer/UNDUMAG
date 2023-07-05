*CMZ :  2.02/01 16/01/2022  12.12.05  by  Michael Scheer
*CMZ :  1.10/00 01/11/2016  17.13.23  by  Michael Scheer
*CMZ : 00.00/15 01/11/2016  17.09.16  by  Michael Scheer
*CMZ : 00.00/07 19/03/2010  16.41.49  by  Michael Scheer
*CMZ : 00.00/05 07/03/2007  10.40.20  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2007
	subroutine util_remove_double_blanks(chin,chout,lenout)

	implicit none

        integer lenin,lenout,k,i

        character(*) chin,chout
        character c1,c2

        lenin=len_trim(chin)
        chout=''

        k=1
        chout(1:1)=chin(1:1)

        do i=2,lenin
          c1=chin(i-1:i-1)
          c2=chin(i:i)
          if (c2.ne.' '.or.c2.eq.' '.and.c1.ne.' ') then
            k=k+1
            chout(k:k)=c2
          endif
        enddo

        lenout=k

	return
	end
