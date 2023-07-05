*CMZ :          01/08/2018  08.38.20  by  Michael Scheer
*CMZ :  1.03/02 22/09/2016  13.19.58  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  12.49.08  by  Michael Scheer
*CMZ :  1.01/02 25/09/2014  09.32.57  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.33.11  by  Michael Scheer
*CMZ :  1.00/01 24/09/2014  12.18.04  by  Michael Scheer
*CMZ :  0.01/02 18/09/2014  13.01.02  by  Michael Scheer
*CMZ :  0.01/01 27/08/2014  15.30.42  by  Michael Scheer
*CMZ :  0.01/00 25/08/2014  10.34.58  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  13.19.56  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.00.02  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  13.35.02  by  Michael Scheer
*CMZ :  0.00/01 07/07/2014  09.10.33  by  Michael Scheer
*CMZ : 00.00/02 30/06/2014  10.35.42  by  Michael Scheer
*-- Author :    Michael Scheer   27/06/2014
      subroutine mshplt_end

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEEP,mplot.

      real rescale_mshplt

      character(2048)
     &  fileeps_mshplt,
     &  viewer_mshplt,viewer_kill_mshplt

      common/mplotc/
     &  rescale_mshplt,
     &  fileeps_mshplt,
     &  viewer_mshplt,viewer_kill_mshplt
*KEND.

      integer nitemp
      parameter(nitemp=100)

      integer ifile,lun,lunps,ipos(2,nitemp),nwords,istat,leneps
      real scalex,scaley

      character(2048) fileps

      call mshplt_flush_buff
      write(lun_ps,*)'% begin of mshplt_end'
      write(lun_ps,*)'showpage'

c      lunps=lunall_ps(nfile_ps)+1
c      call util_get_free_lun(lunps)

      do ifile=1,nfile_ps
        if (rescale_mshplt.ne.1..and.rescale_mshplt.gt.0.0) then
          leneps=len_trim(fileall_ps(ifile))
          fileps=fileall_ps(ifile)(1:leneps-4)//
     &      '_scaled'//fileall_ps(ifile)(leneps-3:leneps)
          open(newunit=lunps,file=fileps(1:len_trim(fileps)),status='unknown')
          lun=lunall_ps(ifile)
          if (lun.gt.0) then
            write(lunall_ps(ifile),*)'showpage'
            rewind(lun)
1           read(lun,'(a)',end=9)cline_ps
            call util_string_split(cline_ps,nitemp,nwords,ipos,istat)
            if (cline_ps(ipos(1,1):ipos(2,1)).eq.'%%Title') then
              write(lunps,'(a)')'%% Title '//fileps(1:len_trim(fileps))
              goto 1
            endif
            if (nwords.ge.3) then
              if (cline_ps(ipos(1,3):ipos(2,3)).eq.'scale') then
                read(cline_ps,*)scalex,scaley
                write(cline_ps,*)scalex*rescale_mshplt,scaley*rescale_mshplt,
     &            ' scale'
                write(lunps,'(a)')cline_ps(1:len_trim(cline_ps))
                goto 11
              endif
            endif
            write(lunps,'(a)')cline_ps(1:len_trim(cline_ps))
            goto 1
11          read(lun,'(a)',end=9)cline_ps
            write(lunps,'(a)')cline_ps(1:len_trim(cline_ps))
            goto 11
9           close(lunps)
c            close(lunall_ps(ifile))
            lunall_ps(ifile)=-lunall_ps(ifile)
          endif
        else
          close(lunall_ps(ifile))
          lunall_ps(ifile)=-lunall_ps(ifile)
        endif
      enddo

      call sleep(1)

      if(irunviewer_ps.ne.0) then

        if(iviewinter_ps.ne.0) then
          write(6,*)'Hit any key to terminate viewer:'
          read(5,'(a)')
          call system(viewer_kill_mshplt(1:len_trim(viewer_kill_mshplt)))
        endif

      endif

      nfile_ps=0
      kzone_ps=0
      itouched_ps=0

      return
      end
