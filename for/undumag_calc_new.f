*CMZ :  2.04/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.03/00 13/07/2022  14.49.18  by  Michael Scheer
*CMZ :  2.02/01 20/01/2022  09.10.26  by  Michael Scheer
*CMZ :  2.02/00 30/03/2021  16.58.55  by  Michael Scheer
*CMZ :  2.01/08 28/07/2020  11.52.35  by  Michael Scheer
*CMZ :  2.01/05 21/07/2020  12.59.07  by  Michael Scheer
*CMZ :  1.25/00 24/01/2018  15.15.53  by  Michael Scheer
*CMZ :  1.17/08 30/05/2017  14.55.39  by  Michael Scheer
*CMZ :  1.11/01 09/01/2017  13.51.01  by  Michael Scheer
*CMZ :  1.11/00 06/12/2016  19.35.14  by  Michael Scheer
*CMZ :  1.10/02 25/11/2016  12.01.14  by  Michael Scheer
*CMZ :  1.10/00 11/11/2016  12.55.12  by  Michael Scheer
*CMZ :  1.02/00 22/08/2016  09.11.46  by  Michael Scheer
*CMZ :  0.00/10 13/07/2016  09.36.23  by  Michael Scheer
*CMZ :  0.00/09 30/06/2016  15.12.31  by  Michael Scheer
*CMZ :  0.00/05 10/06/2016  15.58.48  by  Michael Scheer
*CMZ :  0.00/04 11/05/2016  11.22.46  by  Michael Scheer
*-- Author :    Michael Scheer   10/05/2016
      subroutine undumag_calc_new(kecho)

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      integer i,kecho,k,luno,maxlen
      integer :: idebug=0

      double precision, dimension(:), allocatable :: par
      character(512) cline
      character(128), dimension(:), allocatable :: cpar
      character(128) :: c128,chvar

      allocate(par(nclcvar),cpar(nclcvar))

      if (kecho /= 0) then
        print*," "
        print*,"Entered undumag_calc_new:"
        print*," "
      endif

      do i=1,nclcvar
        cline=clcvar(i)
        if (kecho /= 0) print*,trim(cline)
        do k=1,len_trim(cline)
          if (cline(k:k) == '!') then
            cline=cline(1:k-1)
            exit
          endif
        enddo
        cline='$'//trim(cline)
        ! sort cpar by length
        call undumag_sort_cpar(nclcvar,nvar_t,cpar,par)
        call undumag_calc_cline(cline,nclcvar,nvar_t,par,cpar)
        if (idebug.ne.0) then
          print*,trim(cline)
          do k=1,nvar_t
            print*,nclcvar,k,par(k),trim(cpar(k))
          enddo
          !call util_break
        endif
      enddo

      if (idebug.ne.0) stop "Ende in UNDUMAG_CALC_NEW"

      call undumag_sort_cpar(nclcvar,nvar_t,cpar,par)

      allocate(t_variables(nvar_t))

      maxlen=0

      open(newunit=luno,file='undumag_variables.lis')
      do i=1,nvar_t
        t_variables(i)%val=par(i)
        t_variables(i)%cname=trim(cpar(i))
        maxlen=max(maxlen,len_trim(cpar(i)))
      enddo

      coating=-9999.0d0
      c128="coating"

      do i=1,nvar_t
        chvar=t_variables(i)%cname
        call util_lower_case(chvar)
        if (chvar.eq."coating".or.chvar.eq."mcoating")
     &    coating=t_variables(i)%val
        write(luno,*)
     &    i,t_variables(i)%cname(1:maxlen),t_variables(i)%val
      enddo
      flush(luno)
      close(luno)

      if (coating.eq.-9999.0d0) coating=0.0d0

      write(lun6,*)""
      write(lun6,*)"Coating of Magnets:",coating
      write(lun6,*)

      deallocate(par,cpar)

      return
      end
