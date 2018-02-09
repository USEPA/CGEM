      MODULE Grid

       USE Model_dim
       USE netcdf_utils

       IMPLICIT NONE

      real,allocatable,save :: lat(:,:),lon(:,:) !Latitude and longitude of each grid cell
      real,allocatable,save :: d(:,:,:)          !Distance from surface to bottom of cell 
      real,allocatable,save :: d_sfc(:,:,:)      !Distance from surface to center of cell
      real,allocatable,save :: depth(:,:)        !Depth of water column    
      real,allocatable,save :: dz(:,:,:)         !Thickness of cell
      real,allocatable,save :: Vol(:,:,:), Vol_prev(:,:,:)        !Volume of each cell
      real,allocatable,save :: area(:,:)       !Area of each cell
      real,allocatable,save :: fm(:,:,:)        ! land(0)/sea(1) mask
      integer,allocatable,save :: wsm(:,:)       ! shelf(0)/open ocean(1) mask

      !NCOM grid variables
      real,allocatable,save :: zz(:)  !depth at middle of layer 
      real,allocatable,save :: zl(:)  !depth at top of layer
      real,allocatable,save :: dzz(:) !sigma difference between middle of layers
      real,allocatable,save :: h(:,:)   !undisturbed water depth - at center of cells for NCOM, at bottom of cell for POM
      real, allocatable, save :: dz_k(:) !Original (Ko) dz, sigma thickness of layer k
      real, save :: Hs  !reference depth used to calculate sigma values given depths of layer surfaces and centers

!Temporary variables to debug Advection for EFDC
      real,allocatable,save :: dx(:,:)  !dx
      real,allocatable,save :: dy(:,:)  !dy
  
 
      integer, parameter :: numGridFiles = 2
      type(netCDF_file), save :: grid_info(numGridFiles)    ! 1-column depth, 2-cell depth
      integer, dimension(numGridFiles), save :: gridStartIndex  ! holds the last time index accessed from netcdf file for each grid variable

      integer, parameter :: eColDepth = 1
      integer, parameter :: eCellDepth = 2

      contains

      Subroutine Set_Grid(myid,numprocs)

      IMPLICIT NONE

      integer j
      character(200) filename
      integer, intent(in) :: myid,numprocs
      integer mpierr

      call Grid_allocate(myid)

if(myid.eq.0) then
      if (Which_gridio .eq. 0 .OR. Which_gridio .eq. 1) then   ! used for basic and EFDC grids
         write(filename,'(A, A)') trim(DATADIR),'/nz.dat'
         open(unit=19,file=filename)
         read(19,*)
         do j=1,jm
           read(19,*) nza(:,j)
         enddo
         close(19)
      else if (Which_gridio .eq. 2 .OR. Which_gridio .eq. 3) then  ! used for NCOM and POM grids
         nza = km 
      else 
          write(6,*)'Could not read number of layers from file'
          write(6,*)'...stopping execution'
          stop
      endif
endif
if(numprocs.gt.1) then
     call MPI_BCAST(nza,im*jm,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
endif

!----------------------
! --- get grid location
!----------------------
if(myid.eq.0) then
      call USER_getLonLat(lat,lon)
endif
if(numprocs.gt.1) then
     call MPI_BCAST(lat,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
     call MPI_BCAST(lon,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
endif

if(myid.eq.0) then
      if (Which_gridio.eq.0) then
        call USER_get_basic_grid(dz,depth,d,d_sfc,area,Vol)
      else if (Which_gridio.eq.1) then
        gridStartIndex=1
        call USER_get_EFDC_grid()
      else if (Which_gridio.eq.2) then
        call USER_get_NCOM_grid()
      else if (Which_gridio.eq.3) then
        call USER_get_POM_grid()
      endif
endif
if(numprocs.gt.1) then
     call MPI_BCAST(dz,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
     call MPI_BCAST(depth,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
     call MPI_BCAST(d,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
     call MPI_BCAST(d_sfc,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
     call MPI_BCAST(area,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
     call MPI_BCAST(Vol,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
endif
!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
!      call USER_get_masks() !No, need to calculate depth first!!
#ifdef DEBUG 
write(6,*) "---Set_Grid----"
write(6,*) "  Allocated Grid in Grid_allocate"
write(6,*) "  Reading nza for 0D and EFDC, setting nza=km for NCOM"
write(6,*) "  *** nza really should be zero if land, but is set to km here in Set_Grid"
write(6,*)
#endif

      return

      End Subroutine Set_Grid

      Subroutine Grid_allocate(myid)

      USE Fill_Value

      IMPLICIT NONE

      integer, intent(in) :: myid

      ALLOCATE(lat(im,jm))
      ALLOCATE(lon(im,jm))
      ALLOCATE(depth(im,jm))
      ALLOCATE(d(im,jm,km))
      ALLOCATE(Vol(im,jm,km))
      ALLOCATE(Vol_prev(im,jm,km))
      ALLOCATE(area(im,jm))
      ALLOCATE(dz(im,jm,km))
      ALLOCATE(d_sfc(im,jm,km))
      ALLOCATE(fm(im,jm,km))
      ALLOCATE(wsm(im,jm))

!Temporary variables to debug Advection for EFDC
      ALLOCATE(dx(im,jm))  !dx
      ALLOCATE(dy(im,jm))  !dy
      dx=fill(0)  !Fill values for netCDF
      dy=fill(0)


      if (Which_gridio .eq. 2 .or. Which_gridio .eq. 3) then
         ALLOCATE(zz(35))
         ALLOCATE(zl(35))
         ALLOCATE(dzz(35))
         ALLOCATE(dz_k(35))
         ALLOCATE(h(im,jm))
         zz=fill(0)
         zl=fill(0)
         dzz=fill(0)
         h=fill(0)
      endif

      lat=fill(0)  !Fill values for netCDF
      lon=fill(0)  
      depth=fill(0) 
      d=fill(0)  
      Vol=fill(0)  
      area=fill(0)
      dz=fill(0) 
      d_sfc=fill(0)  
      fm=0. !Default land, real zero
      wsm=0 !Default shelf, integer zero

if(myid.eq.0) then
      if (Which_gridio.eq.1) then
       write(grid_info(eColDepth)%fileName, '(A,A)')trim(DATADIR),'/INPUT/WaterDepth.nc'
       write(grid_info(eCellDepth)%fileName, '(A,A)')trim(DATADIR),'/INPUT/LayerDepth.nc'
       call Open_Grid_NetCDF()
      endif
endif
    
#ifdef DEBUG 
write(6,*) "----Grid_allocate"
write(6,*) "  Allocating NaN for stuff the code should never access"
write(6,*) "  Also Open_Grid_NetCDF"
write(6,*)
#endif

      return

      End Subroutine Grid_allocate


      Subroutine Open_Grid_NetCDF()
      
      IMPLICIT NONE
      integer :: i

      do i=1,numGridFiles
        call open_netcdf(grid_info(i)%fileName, 0, grid_info(i)%ncid)
        call init_info(grid_info(i))
#ifdef DEBUG
        write(6,*) "netCDF file=",grid_info(i)%fileName
        call report_info(grid_info(i))
#endif
      enddo

      return
      End Subroutine Open_Grid_NetCDF


      Subroutine Close_Grid_NetCDF()
      
      IMPLICIT NONE
      integer :: i

      do i=1,numGridFiles
        call close_netcdf(grid_info(i)%ncid)
      enddo

      return
      End Subroutine Close_Grid_NetCDF


      End Module Grid
