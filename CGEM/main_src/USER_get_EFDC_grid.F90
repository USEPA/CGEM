      subroutine USER_get_EFDC_grid()

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE INPUT_VARS, Only:icent,jcent 

      real, dimension(im,jm) :: dx, dy, sdetg
      integer :: i,j
      character(200) filename

      write(filename,'(A, A)') trim(DATADIR),'/dxdy.dat'
      open(19,file=filename,status='old')
      read(19,*) !dx
      do j=1,jm
         read(19,*) dx(:,j)
      !   write(6,*) j,dx(:,j)
      enddo
      read(19,*) !dy
      do j=1,jm
         read(19,*) dy(:,j)
      !   write(6,*) j,dy(:,j)     
      enddo
      close(19)

      write(filename,'(A, A)') trim(DATADIR),'/lxly.dat'
      open(19,file=filename,status='old')
      read(19,*) !sqrt(det(g))
      do i=1,im
         read(19,*) sdetg(i,:)
      enddo
      close(19)

      do j = 1, jm
       do i = 1, im
          if(nza(i,j) > 0) then
          area(i,j) = dx(i,j)*dy(i,j) !*sdetg(i,j)
      !    write(6,*) "area",i,j,area(i,j),area(i,j)*sdetg(i,j)
          !write(6,*) "area*g",area(i,j)*sdetg(i,j)
          endif
       enddo
      enddo

      !stop

#ifdef DEBUG
write(6,*) "---USER_get_EFDC grid----"
write(6,*) "  only setting dx, dy, dxdy, area, and depth"
write(6,*)
#endif

      return

      end subroutine USER_get_EFDC_grid


      subroutine USER_update_EFDC_grid(TC_8,T_8,myid,numprocs)

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE interp_utils

      IMPLICIT NONE

      integer :: i,j,k, nz, mpierr
      integer, intent(in) :: myid,numprocs
      integer(kind=8) :: TC_8,T_8, t_current
      integer(kind=8) :: T_1,T_2, TC_1,TC_2  !bookend time values
      real x
      logical :: broadcast_grid  ! logical for whether to broadcast T_8

!      call interpVar(grid_info(eColDepth), TC_8, gridStartIndex(eColDepth), depth)
!      call interpVar(grid_info(eCellDepth), TC_8, gridStartIndex(eCellDepth), dz)
!      call interpVar(grid_info(eColDepth), T_8, gridStartIndex(eColDepth), depth)
!      call interpVar(grid_info(eCellDepth), T_8, gridStartIndex(eCellDepth), dz)



       t_current = T_8

       broadcast_grid = .FALSE.
  
       if (myid.eq.0)then
         if (t_current.gt.grid_t2) then
           broadcast_grid = .TRUE.
         endif
       endif

!       if(t_current .gt. grid_t2) then
       if(broadcast_grid .AND. myid.eq.0) then
!         print*,"reading grid vars for time: ", t_current
         call retrieveBookendVar(grid_info(eColDepth), t_current, gridStartIndex(eColDepth), depth1, depth2, grid_t1, grid_t2)
         call retrieveBookendVar(grid_info(eCellDepth), t_current, gridStartIndex(eCellDepth), dz1, dz2, grid_t1, grid_t2)
!       else 
!         print*,"skipping read of grid vars for time: ", t_current
       end if


!       print*,"b4 depth1(175,2)=",depth1(175,2), " for myid:",myid
!       print*,"b4 depth2(175,2)=",depth2(175,2), " for myid:",myid

       if(numprocs.gt.1)then
         call MPI_BCAST(broadcast_grid,1,MPI_LOGICAL,0,MPI_COMM_WORLD,mpierr)
         if(broadcast_grid)then
           call MPI_BCAST(grid_t1,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
           call MPI_BCAST(grid_t2,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
           call MPI_BCAST(depth1,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
           call MPI_BCAST(depth2,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
           call MPI_BCAST(dz1,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
           call MPI_BCAST(dz2,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
         endif
       endif

!       print*,"depth1(175,2)=",depth1(175,2), " for myid:",myid
!       print*,"depth2(175,2)=",depth2(175,2), " for myid:",myid

       call interp(depth1, depth2, grid_t1, grid_t2, t_current, depth, .TRUE.)
       call interp(dz1, dz2, grid_t1, grid_t2, t_current, dz, .TRUE.)
                
!       print*,"depth(175,2)=",depth(175,2), " for myid:",myid

      if (myid .eq. 0)then
        Vol_prev = Vol
      endif

      d = fill(0) 
      d_sfc = fill(0) 
      do j=1,jm
       do i=1,im
          nz=nza(i,j)
          do k=1,nz !do loop will not execute if nza=0
           if(depth(i,j).le.0) then  !L3 Soon change to error checking before run, assume data is correct.
                 write(6,*) "DEPTH.LE.0=",depth(i,j)," at i=",i," j=",j
                 stop
           endif
           !L3 Soon change to error checking before run, assume data is correct.
           x = dz(i,j,k)
           if(x.ne.x) then
             write(6,*) "dz is NAN",x
             stop
           endif 
           if(dz(i,j,k).le.0) then
                 write(6,*) "dz for i,j,k=",i,j,k
                 write(6,*) "where nz(i,j)=",nza(i,j)
                 write(6,*) "dz",dz(i,j,k)
                 write(6,*) "depth",depth(i,j)
                 write(6,*) "Stopping b/c dz <= 0"
                 stop
           endif
           d_sfc(i,j,k) = sum(dz(i,j,1:(k-1))) + dz(i,j,k)/2. 
           d(i,j,k) = sum(dz(i,j,1:k)) !bottom of cell 
           Vol(i,j,k) = area(i,j) * dz(i,j,k)
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

!stop
#ifdef debug
write(6,*) "---USER_update_EFDC grid----"
write(6,*) "  setting depth, dz, d_sfc, d, Vol"
write(6,*) "  Time in seconds",TC_8
nz=nza(icent,jcent)
write(6,*) "  At i,j,k=",icent,jcent,nz)
write(6,*) " d, d_sfc, depth, dz, Vol, area="
write(6,*) d(icent,jcent,nz),d_sfc(icent,jcent,nz),depth(icent,jcent)
write(6,*) dz(icent,jcent,nz),Vol(icent,jcent,nz),area(icent,jcent)
#endif

      return

      end subroutine USER_update_EFDC_grid

