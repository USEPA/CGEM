      MODULE Grid

       USE Model_dim
       USE netcdf_utils

       IMPLICIT NONE

      real,allocatable,save :: lat(:,:),lon(:,:) !Latitude and longitude of each grid cell
      real,allocatable,save :: d(:,:,:)          !Water depth at cell bottom
      real,allocatable,save :: d_sfc(:,:,:)      !Distance from surface to center of cell
      real,allocatable,save :: depth(:,:)      !Distance from surface to center of cell
      real,allocatable,save :: dz(:,:,:)         !Thickness of cell
      real,allocatable,save :: Vol(:,:,:), Vol_prev(:,:,:)        !Volume of each cell
      real,allocatable,save :: area(:,:)       !Area of each cell
      integer,allocatable,save :: fm(:,:)        ! land(0)/sea(1) mask for NCOM, other for EFDC
      integer,allocatable,save :: wsm(:,:)       ! shelf(0)/open ocean(1) mask
   
      integer, parameter :: numGridFiles = 2
      type(netCDF_file), save :: grid_info(numGridFiles)    ! 1-column depth, 2-cell depth
      integer, dimension(numGridFiles), save :: gridStartIndex  ! holds the last time index accessed from netcdf file for each grid variable

      integer, parameter :: eColDepth = 1
      integer, parameter :: eCellDepth = 2

      contains

      Subroutine Set_Grid(TC_8)

      IMPLICIT NONE

      integer j
      character(200) filename
      integer(kind=8) :: TC_8

      call Grid_allocate()

      write(filename,'(A, A)') trim(DATADIR),'/nz.dat'
      open(unit=19,file=filename)
      read(19,*)
      do j=1,jm
        read(19,*) nza(:,j)
      enddo
      close(19)

!----------------------
! --- get grid location
!----------------------
      call USER_getLonLat(lat,lon)

      if (Which_gridio.eq.0) then
        call USER_get_basic_grid(dz,depth,d,d_sfc,area,Vol)
      else if (Which_gridio.eq.1) then
        gridStartIndex=1
        call USER_get_EFDC_grid(TC_8)
      endif
!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
      call USER_get_masks(fm,wsm)

      return

      End Subroutine Set_Grid

      Subroutine Grid_allocate()

      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(lat(im,jm))
      ALLOCATE(lon(im,jm))
      ALLOCATE(depth(im,jm))
      ALLOCATE(d(im,jm,nsl))
      ALLOCATE(Vol(im,jm,nsl))
      ALLOCATE(Vol_prev(im,jm,nsl))
      ALLOCATE(area(im,jm))
      ALLOCATE(dz(im,jm,nsl))
      ALLOCATE(d_sfc(im,jm,nsl))
      ALLOCATE(fm(im,jm))
      ALLOCATE(wsm(im,jm))

      lat=fill(0)  !Fill values for netCDF
      lon=fill(0)  
      depth=fill(0) 
      d=fill(0)  
      Vol=fill(0)  
      area=fill(0)
      dz=fill(0) 
      d_sfc=fill(0)  
      fm=fill(0) 
      wsm=fill(0) 

      if (Which_gridio.eq.1) then
        write(grid_info(eColDepth)%fileName, '(A,A)')trim(DATADIR),'/INPUT/WaterDepth.nc'
        write(grid_info(eCellDepth)%fileName, '(A,A)')trim(DATADIR),'/INPUT/LayerDepth.nc'
        call Open_Grid_NetCDF()
      endif

      return

      End Subroutine Grid_allocate


      Subroutine Open_Grid_NetCDF()
      
      IMPLICIT NONE
      integer :: i

      do i=1,numGridFiles
        call open_netcdf(grid_info(i)%fileName, 0, grid_info(i)%ncid)
        call init_info(grid_info(i))
        !call report_info(grid_info(i))
      enddo

      End Subroutine Open_Grid_NetCDF


      Subroutine Close_Grid_NetCDF()
      
      IMPLICIT NONE
      integer :: i

      do i=1,numGridFiles
        call close_netcdf(grid_info(i)%ncid)
      enddo

      End Subroutine Close_Grid_NetCDF


      End Module Grid
