      MODULE Grid

       USE Model_dim
       USE netcdf_utils

       IMPLICIT NONE

      real,allocatable,save :: lat(:,:),lon(:,:) !Latitude and longitude of each grid cell
      real,allocatable,save :: d(:,:,:)          !Distance from surface to bottom of cell   !!!OLDCOMMENT -- Water depth at cell bottom
      real,allocatable,save :: d_sfc(:,:,:)      !Distance from surface to center of cell
      real,allocatable,save :: depth(:,:)        !Depth of water column       !!!OLDCOMMENT -- Distance from surface to center of cell
      real,allocatable,save :: dz(:,:,:)         !Thickness of cell
      real,allocatable,save :: Vol(:,:,:), Vol_prev(:,:,:)        !Volume of each cell
      real,allocatable,save :: area(:,:)       !Area of each cell
      integer,allocatable,save :: fm(:,:)        ! land(0)/sea(>0) mask for NCOM, other for EFDC
      integer,allocatable,save :: wsm(:,:)       ! shelf(0)/open ocean(1) mask

      !NCOM grid variables
      real,allocatable,save :: zz(:)  !depth at middle of layer 
      real,allocatable,save :: zl(:)  !depth at top of layer
      real,allocatable,save :: dzz(:) !sigma difference between middle of layers
      real,allocatable,save :: h(:,:)   !undisturbed water depth at center of cells
      real, save :: Hs  !reference depth used to calculate sigma values given depths of layer surfaces and centers

   
      integer, parameter :: numGridFiles = 2
      type(netCDF_file), save :: grid_info(numGridFiles)    ! 1-column depth, 2-cell depth
      integer, dimension(numGridFiles), save :: gridStartIndex  ! holds the last time index accessed from netcdf file for each grid variable

      integer, parameter :: eColDepth = 1
      integer, parameter :: eCellDepth = 2

      contains

      Subroutine Set_Grid(TC_8)

      IMPLICIT NONE

      integer i,j
      character(200) filename
      integer(kind=8) :: TC_8

      call Grid_allocate()

      if (Which_gridio .eq. 0 .OR. Which_gridio .eq. 1) then   ! used for basic, EFDC, and NCOM grids
         write(filename,'(A, A)') trim(DATADIR),'/nz.dat'
         open(unit=19,file=filename)
         read(19,*)
         do j=1,jm
           read(19,*) nza(:,j)
         enddo
         close(19)
      else if (Which_gridio .eq. 2) then
         nza = nz_max
      else 
          write(6,*)'Could not read number of layers from file'
          write(6,*)'...stopping execution'
          stop
      endif

!----------------------
! --- get grid location
!----------------------
      call USER_getLonLat(lat,lon)

      if (Which_gridio.eq.0) then
        call USER_get_basic_grid(dz,depth,d,d_sfc,area,Vol)
      else if (Which_gridio.eq.1) then
        gridStartIndex=1
        call USER_get_EFDC_grid(TC_8)
      else if (Which_gridio.eq.2) then
        call USER_get_NCOM_grid(TC_8)
      endif

!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
      call USER_get_masks()

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

      if (Which_gridio .eq. 2) then
         ALLOCATE(zz(35))
         ALLOCATE(zl(35))
         ALLOCATE(dzz(35))
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
