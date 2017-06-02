      MODULE Hydro 

      USE netcdf_utils

      IMPLICIT NONE

      real,allocatable,save :: S(:,:,:) !Salinity
      real,allocatable,save :: T(:,:,:) !Temperature, C
      real,allocatable,save :: Ux(:,:,:) !X- Horizonal Current Flux
      real,allocatable,save :: Vx(:,:,:) !Y- Horizonal Current Flux
      real,allocatable,save :: Wx(:,:,:) !Z- Vertical Current Flux
      real,allocatable,save :: Kh(:,:,:) !Diffusion Coefficients
      real,allocatable,save :: E(:,:) !Elevation
      real,allocatable,save :: Wind(:,:) !Wind Speed, m/s
      real,allocatable,save :: Rad(:,:) !Solar Radiation, watts/m2
      
      type(netCDF_file) :: hydro_info(7)  !indices refer to order declared below in enum, 
                                          !omitting Wind Speed and Solar Radiation
      character(len=200) :: netcdf_fileNames(7)  !holds filenames of hydro netcdf data files
      integer, dimension(7) :: startIndex ! holds the last time index from netcdf file used for each hydro variable
                                          ! used as starting point for next lookup

      integer, parameter :: eSal=1      !Salinity
      integer, parameter :: eTemp = 2   !Temperature
      integer, parameter :: eUx = 3     !X- Horizontal Current Flux
      integer, parameter :: eVx = 4     !Y- Horizontal Current Flux
      integer, parameter :: eWx = 5     !Z- Vertical Current Flux
      integer, parameter :: eKh = 6     !Diffusion Coefficients
      integer, parameter :: eE = 7      !Elevation
      !enum, bind(c) 
      !  enumerator :: eSal=1      !Salinity
      !  enumerator :: eTemp = 2   !Temperature
      !  enumerator :: eUx = 3     !X- Horizontal Current Flux
      !  enumerator :: eVx = 4     !Y- Horizontal Current Flux
      !  enumerator :: eWx = 5     !Z- Vertical Current Flux
      !  enumerator :: eKh = 6     !Diffusion Coefficients
      !  enumerator :: eE = 7      !Elevation
      !endenum  

      contains

      Subroutine Allocate_Hydro()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(S(im,jm,nsl))
      ALLOCATE(T(im,jm,nsl))
      ALLOCATE(Wind(im,jm))
      ALLOCATE(Rad(im,jm))
      ALLOCATE(Ux(im,jm,nsl))
      ALLOCATE(Vx(im,jm,nsl))
      ALLOCATE(Wx(im,jm,nsl))
      ALLOCATE(Kh(im,jm,nsl))
      ALLOCATE(E(im,jm))

      !Fill values for netCDF
      S=fill(0)  
      T=fill(0) 
      Wind=fill(0)  
      Rad=fill(0) 
      Ux=fill(0)
      Vx=fill(0)  
      Wx=fill(0) 
      Kh=fill(0)
      E=fill(0) 

      return

      End Subroutine Allocate_Hydro 



      Subroutine Init_Hydro_NetCDF()
      
      USE Model_dim

      IMPLICIT NONE

      integer :: i

      !Set filenames for netCDF 
      write(netcdf_fileNames(1), '(A, A)') trim(DATADIR), '/INPUT/Salt.nc'
      write(netcdf_fileNames(2), '(A, A)') trim(DATADIR), '/INPUT/Temp.nc'
      write(netcdf_fileNames(3), '(A, A)') trim(DATADIR), '/INPUT/UFlow.nc'
      write(netcdf_fileNames(4), '(A, A)') trim(DATADIR), '/INPUT/VFlow.nc'
      write(netcdf_fileNames(5), '(A, A)') trim(DATADIR), '/INPUT/WFlow.nc'
      write(netcdf_fileNames(6), '(A, A)') trim(DATADIR), '/INPUT/Ev.nc'
      write(netcdf_fileNames(7), '(A, A)') trim(DATADIR), '/INPUT/SurfaceElev.nc'

      do i=1,7
        call open_netcdf(netcdf_fileNames(i), 0, hydro_info(i)%ncid)
        hydro_info(i)%fileName = netcdf_fileNames(i)
        call init_info(hydro_info(i))
      enddo

      startIndex = 1
      
      End Subroutine Init_Hydro_NetCDF


      Subroutine Close_Hydro_NetCDF()

      IMPLICIT NONE
      integer :: i

      do i=1,7
        call close_netcdf(hydro_info(i)%ncid)
      enddo

      End Subroutine Close_Hydro_NetCDF


      End Module Hydro 
