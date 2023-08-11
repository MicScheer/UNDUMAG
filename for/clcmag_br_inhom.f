*CMZ :  2.04/07 09/08/2023  12.34.00  by  Michael Scheer
*CMZ :  2.04/06 29/03/2023  15.13.19  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/00 13/01/2023  11.36.00  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  10.13.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_br_inhom(imag,ivox,br)

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      integer,save :: ical=0

      type(T_Magnet) tma
      type(T_Voxel) vox

      double precision br(3),bra,xm,ym,zm,tmbr(3),tmbra,dxyz(3),dxyzn,coef,
     &  undumag_variable_getval

      integer imag,ivox,i,ix,iy,iz,k,ipos(2,10),istat,nwords

      character(8) cbr
      character(512) c512

      if (ical.eq.0) then
        call  clcmag_br_inhom_init
        ical=1
      endif

      tma=t_magnets(imag)

      xm=tma%xyzinh(1)
      ym=tma%xyzinh(2)
      zm=tma%xyzinh(3)

      tmbr=tma%Br
      tmbra=norm2(tmbr)

      vox=t_magnets(imag)%t_voxels(ivox)

      dxyz=[vox%xyz(1)-xm,vox%xyz(2)-ym,vox%xyz(3)-zm]
      dxyzn=norm2(dxyz)

      if (tma%IsPole.ne.0.or.tma%IsInhom.eq.0.or.
     &    tmbra.eq.0.0d0.or.dxyzn.eq.0.0d0) then
        br=tmbr
        return
      endif

      br=0.0d0
      do i=1,tma%IsInhom

        c512=tma%cinhom(i)
        call util_string_split(c512,10,nwords,ipos,istat)

        ix=nint(undumag_variable_getval(c512(ipos(1,2):ipos(2,2))))
        iy=nint(undumag_variable_getval(c512(ipos(1,3):ipos(2,3))))
        iz=nint(undumag_variable_getval(c512(ipos(1,4):ipos(2,4))))
        coef=undumag_variable_getval(c512(ipos(1,5):ipos(2,5)))

        !read(c512,*)cbr,ix,iy,iz
        read(c512,*)cbr
        if (cbr.eq.'x') then
          k=1
        else if (cbr.eq.'y') then
          k=2
        else if (cbr.eq.'z') then
          k=3
        endif
        br(k)=br(k)+coef*dxyz(1)**ix*dxyz(2)**iy*dxyz(3)**iz
      enddo

      if (tma%xyzinh(4).ne.0.0d0) then
        bra=norm2(br)
        if (bra.eq.0.0d0) return
        br=br/bra*tmbra
      endif

      return
      end
