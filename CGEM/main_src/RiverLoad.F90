      MODULE RiverLoad 

      USE netcdf_utils

      IMPLICIT NONE

      real,allocatable,save :: Riv1(:) !Var1
      real,allocatable,save :: Riv2(:) !Var2
      real,allocatable,save :: Riv3(:) !Var3
      real,allocatable,save :: Riv4(:) !Var4
      real,allocatable,save :: Riv5(:) !Var5
      real,allocatable,save :: Riv6(:) !Var6
      real,allocatable,save :: Riv7(:) !Var7
      real,allocatable,save :: Riv8(:) !Var8
      real,allocatable,save :: Riv9(:) !Var9


      real,allocatable,save :: Riv1A(:) !Var1
!      real,allocatable,save :: Riv2A(:) !Var2
!      real,allocatable,save :: Riv3A(:) !Var3
!      real,allocatable,save :: Riv4A(:) !Var4
!      real,allocatable,save :: Riv5A(:) !Var5
!      real,allocatable,save :: Riv6A(:) !Var6
!      real,allocatable,save :: Riv7A(:) !Var7
!      real,allocatable,save :: Riv8A(:) !Var8
!      real,allocatable,save :: Riv9A(:) !Var9
      real,allocatable,save :: Riv1B(:) !Var1
!      real,allocatable,save :: Riv2B(:) !Var2
!      real,allocatable,save :: Riv3B(:) !Var3
!      real,allocatable,save :: Riv4B(:) !Var4
!      real,allocatable,save :: Riv5B(:) !Var5
!      real,allocatable,save :: Riv6B(:) !Var6
!      real,allocatable,save :: Riv7B(:) !Var7
!      real,allocatable,save :: Riv8B(:) !Var8
!      real,allocatable,save :: Riv9B(:) !Var9
      
      real, allocatable, save :: weights(:,:)      ! River loads fractions/weights
      integer, allocatable, save :: riversIJ(:,:)  ! Grid cell indices of river loads discharge locations

      type(netCDF_file) :: riverload_info(9)  !indices refer to order declared below in enum, 
      character(len=200) :: netcdf_riverload_fileNames(9)  !holds filenames of hydro netcdf data files
      integer, dimension(9) :: startRivIndex ! holds the last time index from netcdf file used for each riverload variable
                                          ! used as starting point for next lookup

      integer, parameter :: eRiv1 = 1    !Var1
!      integer, parameter :: eRiv2 = 2    !Var2
!      integer, parameter :: eRiv3 = 3    !Var3
!      integer, parameter :: eRiv4 = 4    !Var4
!      integer, parameter :: eRiv5 = 5    !Var5
!      integer, parameter :: eRiv6 = 6    !Var6
!      integer, parameter :: eRiv7 = 7    !Var7
!      integer, parameter :: eRiv8 = 8    !Var8
!      integer, parameter :: eRiv9 = 9    !Var9

      integer, save :: fRv, lRv  ! looping index of FirstRiverVar and LastRiverVar

      integer(kind=8), save :: river_tc1, river_tc2  !bookend time values for river variables

      contains

      Subroutine Allocate_RiverLoads()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      print*,"Allocating riverloads"

      ALLOCATE(Riv1(nRiv))
!      ALLOCATE(Riv2(nRiv))
!      ALLOCATE(Riv3(nRiv))
!      ALLOCATE(Riv4(nRiv))
!      ALLOCATE(Riv5(nRiv))
!      ALLOCATE(Riv6(nRiv))
!      ALLOCATE(Riv7(nRiv))
!      ALLOCATE(Riv8(nRiv))
!      ALLOCATE(Riv9(nRiv))
      ALLOCATE(Riv1A(nRiv))
      ALLOCATE(Riv1B(nRiv))

      ALLOCATE(weights(nRiv,NSL))
      ALLOCATE(riversIJ(nRiv,2))

      !Fill values for netCDF
      Riv1 = fill(0)  
!      Riv2 = fill(0)
!      Riv3 = fill(0)
!      Riv4 = fill(0)
!      Riv5 = fill(0)
!      Riv6 = fill(0)
!      Riv7 = fill(0)
!      Riv8 = fill(0)
!      Riv9 = fill(0)
      Riv1A = fill(0)  
      Riv1B = fill(0)  

      return

      End Subroutine Allocate_RiverLoads 


      Subroutine Init_RiverLoad_NetCDF()
      
      USE Model_dim

      IMPLICIT NONE

      integer :: i
      character(len=200) :: textfile

      !Set filenames for netCDF
      if (Which_gridio .eq. 1) then 
         write(netcdf_riverload_fileNames(eRiv1), '(A, A)') trim(DATADIR), '/INPUT/TP_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv1), '(A, A)') trim(DATADIR), '/INPUT/TN_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv2), '(A, A)') trim(DATADIR), '/INPUT/NO3_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv3), '(A, A)') trim(DATADIR), '/INPUT/NH3_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv4), '(A, A)') trim(DATADIR), '/INPUT/DON_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv5), '(A, A)') trim(DATADIR), '/INPUT/TP_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv6), '(A, A)') trim(DATADIR), '/INPUT/DIP_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv7), '(A, A)') trim(DATADIR), '/INPUT/DOP_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv8), '(A, A)') trim(DATADIR), '/INPUT/BOD1_RiverLoads.nc'
!         write(netcdf_riverload_fileNames(eRiv9), '(A, A)') trim(DATADIR), '/INPUT/DO_RiverLoads.nc'
      else if (Which_gridio .eq. 2) then
!         write(netcdf_fileNames(eSal), '(A, A)') trim(DATADIR), '/INPUT/S.nc'
!         write(netcdf_fileNames(eTemp), '(A, A)') trim(DATADIR), '/INPUT/T.nc'
!         write(netcdf_fileNames(eUx), '(A, A)') trim(DATADIR), '/INPUT/U.nc'
!         write(netcdf_fileNames(eVx), '(A, A)') trim(DATADIR), '/INPUT/V.nc'
!         write(netcdf_fileNames(eWx), '(A, A)') trim(DATADIR), '/INPUT/W.nc'
!         write(netcdf_fileNames(eKh), '(A, A)') trim(DATADIR), '/INPUT/KH.nc'
!         write(netcdf_fileNames(eE), '(A, A)') trim(DATADIR), '/INPUT/E.nc'
      else if (Which_gridio .eq. 3) then
!         write(netcdf_fileNames(eSal), '(A, A)') trim(DATADIR), 'NA'  !No salinity input
!         write(netcdf_fileNames(eTemp), '(A, A)') trim(DATADIR), '/INPUT/T.nc'
!         write(netcdf_fileNames(eUx), '(A, A)') trim(DATADIR), '/INPUT/U.nc'
!         write(netcdf_fileNames(eVx), '(A, A)') trim(DATADIR), '/INPUT/V.nc'
!         write(netcdf_fileNames(eWx), '(A, A)') trim(DATADIR), '/INPUT/W.nc'
!         write(netcdf_fileNames(eKh), '(A, A)') trim(DATADIR), '/INPUT/Kh.nc'
!         write(netcdf_fileNames(eE), '(A, A)') trim(DATADIR), '/INPUT/E.nc'
!         write(netcdf_fileNames(eWind), '(A, A)') trim(DATADIR), '/INPUT/Wind.nc'
!         write(netcdf_fileNames(eRad), '(A, A)') trim(DATADIR), '/INPUT/Rad.nc'
      endif


      if (Which_gridio .eq. 1 .OR. Which_gridio .eq. 2) then  !EFDC and NCOM do not use Wind or Rad from NetCDF
         fRv = 1
         lRv = 1
!         lRv = 9;
      else if (Which_gridio .eq. 3) then  !POM does not use Salinity
!         fHv = 2;
!         lHv = 9
      endif
      do i = fRv, lRv
        call open_netcdf(netcdf_riverload_fileNames(i), 0, riverload_info(i)%ncid)
        riverload_info(i)%fileName = netcdf_riverload_fileNames(i)
        call init_info(riverload_info(i))
#ifdef DEBUG
call report_info(riverload_info(i))
#endif
      enddo

      startRivIndex = 1

      write(textfile,'(A, A)') trim(DATADIR),'/RiverIndices.dat'
      open(19, file=textfile, status='old')
      read(19,*)    ! I and J indices of river discharge locations.
      do i = 1, nRiv
         read(19,*) riversIJ(i,1:2)
      enddo
      close(19)
      
      write(textfile,'(A, A)') trim(DATADIR),'/RiverWeights.dat'
      open(19, file=textfile, status='old')
      do i = 1, nRiv
         read(19,*) weights(i,:)
      enddo
      close(19)

      river_tc1=0
      river_tc2=0
      
      End Subroutine Init_RiverLoad_NetCDF


      Subroutine Close_RiverLoad_NetCDF()

      IMPLICIT NONE
      integer :: i

      do i = fRv, lRv
        call close_netcdf(riverload_info(i)%ncid)
      enddo

      End Subroutine Close_RiverLoad_NetCDF


      End Module RiverLoad 
