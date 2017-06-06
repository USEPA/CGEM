      subroutine USER_get_EFDC_grid(TC_8)

      USE Fill_Value
      USE Model_dim
      USE Grid
     
      integer(kind=8) :: TC_8

!      real, intent (out) :: dz(im,jm,nsl)     !cell depth
!      real, intent (out) :: d(im,jm,nsl)      !depth from surface to bottom of cell
!      real, intent (out) :: depth(im,jm)      !total depth of column
!      real, intent (out) :: d_sfc(im,jm,nsl)  !depth from surface to cell center
!      real, intent (out) :: dxdy(im,jm)       !area of cell
!      real, intent (out) :: Vol(im,jm,nsl)    !Volume of cell
      real, dimension(im,jm) :: dx, dy
      integer :: i,j,k
      character(200) filename


      write(filename,'(A, A)') trim(DATADIR),'/dxdy.dat'
      open(19,file=filename,status='old')
      read(19,*) !dx
      do j=1,jm
         read(19,*) dx(:,j)
      enddo
      read(19,*) !dy
      do j=1,jm
         read(19,*) dy(:,j)
      enddo
      close(19)

      do j=1,jm
       do i=1,im
          area(i,j) = dx(i,j)*dy(i,j)
       enddo
      enddo

      !call USER_update_EFDC_Grid(TC_8)

      return

      end subroutine USER_get_EFDC_grid


      subroutine USER_update_EFDC_grid(TC_8)

      USE Fill_Value
      USE Model_dim
      USE Grid

      IMPLICIT NONE

      integer :: i,j,k, nz
      integer(kind=8) :: TC_8

      call interpVar(grid_info(eColDepth), TC_8, 4, 3, gridStartIndex(eColDepth), depth)
      call interpVar(grid_info(eCellDepth), TC_8, 5, 4, gridStartIndex(eCellDepth), dz)

      d = fill(0) 
      d_sfc = fill(0) 
      do j=1,jm
       do i=1,im
          if(depth(i,j).eq.0) depth(i,j)=fill(0)
          nz=nza(i,j)
          do k=1,nz !do loop will not execute if nza=0
           d_sfc(i,j,k) = sum(dz(i,j,1:(k-1))) + dz(i,j,k)/2. 
           d(i,j,k) = sum(dz(i,j,1:k)) !bottom of cell 
           Vol(i,j,k) = area(i,j) * dz(i,j,k)
#ifdef DEBUG
           write(6,*) i,j,k,dz(i,j,k),d_sfc(i,j,k),d(i,j,k)
#endif
          enddo
       enddo
      enddo
      end subroutine USER_update_EFDC_grid

