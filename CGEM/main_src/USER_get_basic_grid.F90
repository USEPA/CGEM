      subroutine USER_get_basic_grid(dz,depth,d,d_sfc,dxdy,Vol)

      USE Fill_Value
      USE Model_dim

      real, intent (out) :: dz(im,jm,km)     !cell depth
      real, intent (out) :: d(im,jm,km)      !depth from surface to bottom of cell
      real, intent (out) :: depth(im,jm)     !total depth of column
      real, intent (out) :: d_sfc(im,jm,km)  !depth from surface to cell center
      real, intent (out) :: dxdy(im,jm)      !area of cell
      real, intent (out) :: Vol(im,jm,km)    !Volume of cell
      real :: dx(im,jm), dy(im,jm)
      integer :: i,j,k,nz
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

      write(filename,'(A, A)') trim(DATADIR),'/d.dat'
      open(19,file=filename,status='old')
      read(19,*) !depth
      do j=1,jm
         read(19,*) depth(:,j)
      enddo
      close(19)

      dz = fill(0) 
      d = fill(0) 
      d_sfc = fill(0) 
      do j=1,jm
       do i=1,im
          dxdy(i,j) = dx(i,j)*dy(i,j)
          if(depth(i,j).eq.0) depth(i,j)=fill(0)
          nz=nza(i,j)
          do k=1,nz !do loop will not execute if nza=0
           dz(i,j,k) = depth(i,j)/nz !assume equally spaced
           d_sfc(i,j,k) = sum(dz(i,j,1:(k-1))) + dz(i,j,k)/2. 
           d(i,j,k) = sum(dz(i,j,1:k)) !bottom of cell 
           Vol(i,j,k) = dxdy(i,j) * dz(i,j,k)
          enddo
       enddo
      enddo

#ifdef DEBUG 
write(6,*) "-----Calling USER_get_basic_grid-----"
write(6,*) "  Which_gridio=",Which_gridio
write(6,*) "  Setting dx, dy, depth, dxdy, dz, d_sfc, d, Vol"
write(6,*)
#endif

      return

      end subroutine USER_get_basic_grid

