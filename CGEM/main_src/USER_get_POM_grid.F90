      subroutine USER_get_POM_grid()

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE INPUT_VARS , ONLY: icent,jcent

      IMPLICIT NONE
 
!      real, dimension(im,jm) :: dx
!      real, dimension(im,jm) :: dy
      real, dimension(jm) :: dxy
      integer :: i,j,k
      character(200) filename
      integer :: nzr, ns

#ifdef map_code
     write(6,*) "----Calling USER_get_NCOM_grid-----"
     write(6,*) "  setting dxy, area, zl, zz, dz_k, and h"
     write(6,*) "  Not setting depths, because don't have E"  
     write(6,*) 
#endif

      write(filename,'(A, A)') trim(DATADIR),'/dxdy.dat'
      open(19,file=filename,status='old')
      read(19,*) 
      read(19,*) dx
      read(19,*)
      read(19,*) dy
      close(19)

      do j=1,jm
       do i=1,im
#ifdef DEBUG_GRID
if (i.eq.250) then
  write(6,*) "for cell i,j=",i,j
  write(6,*) "dx", dx(i,j)
  write(6,*) "dy", dy(i,j)
endif
#endif
          area(i,j) = dx(i,j)*dy(i,j)
       enddo
      enddo
      ! Read in sigma levels for POM grid
      write(filename, '(A,A)') trim(DATADIR), '/sigma.dat'
      open(19,file=filename,status='old')
      read(19,*) !Header
      do k=1,nsl
        read(19,*) zl(k)   !assumes sigma is sigma dat file gives sigma depth at top of layer
      enddo
      close(19)
      !convert sigma to positive values
      zl = -1.0*zl
      do k=1,nsl-1
        dz_k(k) = zl(k+1) - zl(k)
        zz(k) = zl(k) + 0.5d0 * dz_k(k)
      enddo
      ! POM grid depths(undisturbed) do not change with time
      write(filename,'(A, A)') trim(DATADIR),'/d.dat'
      open(19,file=filename,status='old')
      read(19,*)  !Header
      do j=1,jm
        read(19,*) h(:,j)
      enddo
      close(19)

#ifdef DEBUG_GRID
      i=250
      do j=1,jm
      write(6,*) "i=",i,", j=",j,", h=",h(i,j)
      enddo
#endif

      return
      end subroutine USER_get_POM_grid


      subroutine USER_update_POM_grid()

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE Hydro, ONLY: E
      USE INPUT_VARS

      IMPLICIT NONE

      real :: dp !d previous
      integer :: i,j,k,nz
      integer :: init=1

#ifdef map_code 
     if(init.eq.1) then
      write(6,*) "----Calling USER_update_NCOM_grid-----"
      write(6,*) "  setting dxy, area, zl, zz, dz_k, and h"
      write(6,*) "  Not setting depths, because don't have E"
      write(6,*)
     init=0
     endif
#endif
#ifdef DEBUG
     write(6,*) "----Calling USER_get_NCOM_grid-----"
     write(6,*) "  setting dxy, area, zl, zz, dz_k, and h"
     write(6,*) "  For cell i,j,k=",icent,jcent,km
     write(6,*) "h,E,d,zz,area,d_sfc,Vol="
     write(6,*) h(icent,jcent),E(icent,jcent),d(icent,jcent,km)
     write(6,*) zz(km),area(icent,jcent),d_sfc(icent,jcent,km),Vol(icent,jcent,km)
#endif

       do j=1,jm
        do i=1,im
          if (E(i,j).eq.-9999 .or. h(i,j).lt.0.0) then
             depth(i,j) = -9999
          else 
            depth(i,j) = h(i,j)  + E(i,j)
          endif
        enddo
       enddo

 
       do j=1,jm
        do i=1,im
           nz = nza(i,j)
           dp = depth(i,j)
           do k=1,nz   
             dz(i,j,k) = depth(i,j)*(dz_k(k)) 
             d_sfc(i,j,k) = depth(i,j) * zz(k)
             Vol(i,j,k) = area(i,j) * dz(i,j,k)
             d(i,j,k) = sum(dz(i,j,1:k))

#ifdef DEBUG_GRID
if (k.eq.5 .AND. i.eq.250) then
  write(6,*) "for cell i,j,k=",i,j,k
  write(6,*) "h",h(i,j)
  write(6,*) "depth",depth(i,j)
  write(6,*) "E",E(i,j)
  write(6,*) "zl",zl(k)
  write(6,*) "d",d(i,j,k)
  write(6,*) "dz",dz(i,j,k)
  write(6,*) "zz",zz(k)
  write(6,*) "area",area(i,j)
  write(6,*) "d_sfc",d_sfc(i,j,k)
  write(6,*) "Vol",Vol(i,j,k)
endif
#endif

        enddo
       enddo
      enddo  

      end subroutine USER_update_POM_grid

