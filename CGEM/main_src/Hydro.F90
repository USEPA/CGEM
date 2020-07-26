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

      real,allocatable,save :: S1(:,:,:)  !Bookend hydro variables 
      real,allocatable,save :: T1(:,:,:) 
      real,allocatable,save :: Ux1(:,:,:)
      real,allocatable,save :: Vx1(:,:,:)
      real,allocatable,save :: Wx1(:,:,:)
      real,allocatable,save :: Kh1(:,:,:)
      real,allocatable,save :: E1(:,:) 
      real,allocatable,save :: Wind1(:,:)
      real,allocatable,save :: Rad1(:,:) 
      real,allocatable,save :: S2(:,:,:) 
      real,allocatable,save :: T2(:,:,:) 
      real,allocatable,save :: Ux2(:,:,:)
      real,allocatable,save :: Vx2(:,:,:)
      real,allocatable,save :: Wx2(:,:,:)
      real,allocatable,save :: Kh2(:,:,:)
      real,allocatable,save :: E2(:,:) 
      real,allocatable,save :: Wind2(:,:)
      real,allocatable,save :: Rad2(:,:) 

      integer(kind=8),save :: hydro_t1, hydro_t2  ! bookend time values for hydro variables
                                                  ! may be same or different
                                                  ! depending on whether
                                                  ! T_8 or TC_8 is used
      integer(kind=8),save :: hydro_tc1, hydro_tc2
      
      type(netCDF_file) :: hydro_info(9)  !indices refer to order declared below in enum, 
                                          !omitting Wind Speed and Solar Radiation
      character(len=200) :: netcdf_fileNames(9)  !holds filenames of hydro netcdf data files
      integer, dimension(9) :: startIndex ! holds the last time index from netcdf file used for each hydro variable
                                          ! used as starting point for next lookup

      integer, parameter :: eSal=1      !Salinity
      integer, parameter :: eTemp = 2   !Temperature
      integer, parameter :: eUx = 3     !X- Horizontal Current Flux
      integer, parameter :: eVx = 4     !Y- Horizontal Current Flux
      integer, parameter :: eWx = 5     !Z- Vertical Current Flux
      integer, parameter :: eKh = 6     !Diffusion Coefficients
      integer, parameter :: eE = 7      !Elevation
      integer, parameter :: eWind = 8   !Wind
      integer, parameter :: eRad = 9    !Rad

      integer, save :: fHv, lHv  ! looping index of FirstHydroVar and LastHydroVar (currently allows skipping of vars if they are not needed)

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

      ALLOCATE(S1(im,jm,nsl))
      ALLOCATE(T1(im,jm,nsl))
      ALLOCATE(Wind1(im,jm))
      ALLOCATE(Rad1(im,jm))
      ALLOCATE(Ux1(im,jm,nsl))
      ALLOCATE(Vx1(im,jm,nsl))
      ALLOCATE(Wx1(im,jm,nsl))
      ALLOCATE(Kh1(im,jm,nsl))
      ALLOCATE(E1(im,jm))
      ALLOCATE(S2(im,jm,nsl))
      ALLOCATE(T2(im,jm,nsl))
      ALLOCATE(Wind2(im,jm))
      ALLOCATE(Rad2(im,jm))
      ALLOCATE(Ux2(im,jm,nsl))
      ALLOCATE(Vx2(im,jm,nsl))
      ALLOCATE(Wx2(im,jm,nsl))
      ALLOCATE(Kh2(im,jm,nsl))
      ALLOCATE(E2(im,jm))
      
      !Fill values for netCDF
      S=fill(0)  
      T=fill(0) 
      Wind=fill(0)  
      Rad=fill(0) 
      Ux=fill(0)
      Vx=fill(0)  
      Wx=fill(0)
      !Ux = 0.0
      !Vx = 0.0
      !Wx = 0.0 
      Kh=fill(0)
      E=fill(0) 

      S1=fill(0)  
      T1=fill(0) 
      Wind1=fill(0)  
      Rad1=fill(0) 
      Ux1=fill(0)
      Vx1=fill(0)  
      Wx1=fill(0)
      Kh1=fill(0)
      E1=fill(0) 
      S2=fill(0)  
      T2=fill(0) 
      Wind2=fill(0)  
      Rad2=fill(0) 
      Ux2=fill(0)
      Vx2=fill(0)  
      Wx2=fill(0)
      Kh2=fill(0)
      E2=fill(0) 

#ifdef DEBUG
        write(6,*) "Allocate_Hydro"
        write(6,*)
#endif
      
      return

      End Subroutine Allocate_Hydro 


      Subroutine Init_Hydro_NetCDF()
      
      USE Model_dim

      IMPLICIT NONE

      integer :: i

      !Set filenames for netCDF
      if (Which_gridio .eq. 1) then 
         write(netcdf_fileNames(eSal), '(A, A)') trim(DATADIR), '/INPUT/Salt.nc'
         write(netcdf_fileNames(eTemp), '(A, A)') trim(DATADIR), '/INPUT/Temp.nc'
         write(netcdf_fileNames(eUx), '(A, A)') trim(DATADIR), '/INPUT/UFlow.nc'
         write(netcdf_fileNames(eVx), '(A, A)') trim(DATADIR), '/INPUT/VFlow.nc'
         write(netcdf_fileNames(eWx), '(A, A)') trim(DATADIR), '/INPUT/WFlow.nc'
         write(netcdf_fileNames(eKh), '(A, A)') trim(DATADIR), '/INPUT/Ev.nc'
         write(netcdf_fileNames(eE), '(A, A)') trim(DATADIR), '/INPUT/SurfaceElev.nc'
      else if (Which_gridio .eq. 2) then
         write(netcdf_fileNames(eSal), '(A, A)') trim(DATADIR), '/INPUT/S.nc'
         write(netcdf_fileNames(eTemp), '(A, A)') trim(DATADIR), '/INPUT/T.nc'
         write(netcdf_fileNames(eUx), '(A, A)') trim(DATADIR), '/INPUT/U.nc'
         write(netcdf_fileNames(eVx), '(A, A)') trim(DATADIR), '/INPUT/V.nc'
         write(netcdf_fileNames(eWx), '(A, A)') trim(DATADIR), '/INPUT/W.nc'
         write(netcdf_fileNames(eKh), '(A, A)') trim(DATADIR), '/INPUT/KH.nc'
         write(netcdf_fileNames(eE), '(A, A)') trim(DATADIR), '/INPUT/E.nc'
      else if (Which_gridio .eq. 3) then
         write(netcdf_fileNames(eSal), '(A, A)') trim(DATADIR), 'NA'  !No salinity input
         write(netcdf_fileNames(eTemp), '(A, A)') trim(DATADIR), '/INPUT/T.nc'
         write(netcdf_fileNames(eUx), '(A, A)') trim(DATADIR), '/INPUT/U.nc'
         write(netcdf_fileNames(eVx), '(A, A)') trim(DATADIR), '/INPUT/V.nc'
         write(netcdf_fileNames(eWx), '(A, A)') trim(DATADIR), '/INPUT/W.nc'
         write(netcdf_fileNames(eKh), '(A, A)') trim(DATADIR), '/INPUT/Kh.nc'
         write(netcdf_fileNames(eE), '(A, A)') trim(DATADIR), '/INPUT/E.nc'
         write(netcdf_fileNames(eWind), '(A, A)') trim(DATADIR), '/INPUT/Wind.nc'
         write(netcdf_fileNames(eRad), '(A, A)') trim(DATADIR), '/INPUT/Rad.nc'
      endif


      if (Which_gridio .eq. 1 .OR. Which_gridio .eq. 2) then  !EFDC and NCOM do not use Wind or Rad from NetCDF
         fHv = 1;
         lHv = 7;
      else if (Which_gridio .eq. 3) then  !POM does not use Salinity
         fHv = 2;
         lHv = 9
      endif
      do i=fHv,lHv
        call open_netcdf(netcdf_fileNames(i), 0, hydro_info(i)%ncid)
        hydro_info(i)%fileName = netcdf_fileNames(i)
        call init_info(hydro_info(i))
#ifdef DEBUG
        call report_info(hydro_info(i))
#endif
      enddo

      startIndex = 1
      hydro_t1 = 0
      hydro_t2 = 0
      hydro_tc1 = 0
      hydro_tc2 = 0

#ifdef DEBUG 
write(6,*) "---Init_Hydro_NetCDF----"
write(6,*) "  Opening netCDF for Which_gridio=",Which_gridio
write(6,*) "  1==EFDC, 2==NCOM, 3==POM "
write(6,*)
#endif
      return
 
      End Subroutine Init_Hydro_NetCDF


      Subroutine Close_Hydro_NetCDF()

      IMPLICIT NONE
      integer :: i

      do i=fHv,lHv
        call close_netcdf(hydro_info(i)%ncid)
      enddo

#ifdef DEBUG
write(6,*) "---Close_Hydro_NetCDF----"
write(6,*)
#endif

      return

      End Subroutine Close_Hydro_NetCDF


      End Module Hydro 
