      subroutine USER_get_POM_grid()

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE INPUT_VARS , ONLY: icent,jcent

      IMPLICIT NONE

      real, dimension(im,jm) :: dx,dy 
      integer :: i,j,k
      character(200) filename
      integer :: nzr, ns

      write(filename,'(A, A)') trim(DATADIR),'/dxdy.dat'
      open(19,file=filename,status='old')
      read(19,*) 
      read(19,*) dx
      read(19,*)
      read(19,*) dy
      close(19)

      do j=1,jm
       do i=1,im
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

#ifdef DEBUG 
     write(6,*) "----Calling USER_get_POM_grid-----"
     write(6,*) "  setting dxy, area, zl, zz, dz_k, and h"
     write(6,*) "  Not setting depths, because don't have E"
     write(6,*)
#endif

      return
      end subroutine USER_get_POM_grid


      subroutine USER_update_POM_grid(T_8,myid,numprocs)

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE Hydro, ONLY: E
      USE INPUT_VARS

      IMPLICIT NONE

      real :: dp !d previous
      integer :: i,j,k,nz,mpierr
      integer, intent(in) :: myid, numprocs
      integer(kind=8) :: T_8, t_current
      integer :: init=1
      logical :: broadcast_grid

      t_current = T_8
      broadcast_grid = .FALSE.

      if (myid.eq.0)then
        if (t_current.gt.grid_t2) then
          broadcast_grid = .TRUE.
        endif
      endif

      if (myid.eq.0)then
        Vol_prev = Vol
      endif


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
        enddo
       enddo
      enddo  

      if(numprocs.gt.1) then
         call MPI_BCAST(dz,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
         call MPI_BCAST(d,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
         call MPI_BCAST(d_sfc,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
         call MPI_BCAST(Vol,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
         call MPI_BCAST(Vol_prev,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
      endif


#ifdef DEBUG
     write(6,*) "----Calling USER_get_POM_grid-----"
     write(6,*) "  setting dxy, area, zl, zz, dz_k, and h"
     write(6,*) "  For cell i,j,k=",icent,jcent,km
     write(6,*) "h,E,d,zz,area,d_sfc,Vol="
     write(6,*) h(icent,jcent),E(icent,jcent),d(icent,jcent,km)
     write(6,*) zz(km),area(icent,jcent),d_sfc(icent,jcent,km),Vol(icent,jcent,km)
#endif
      return
      end subroutine USER_update_POM_grid

