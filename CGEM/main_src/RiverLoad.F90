      MODULE RiverLoad 

      USE netcdf_utils

      IMPLICIT NONE

      real,allocatable,save :: Var1(:) !Var1
      real,allocatable,save :: Var2(:) !Var2
      real,allocatable,save :: Var3(:) !Var3
      real,allocatable,save :: Var4(:) !Var4
      real,allocatable,save :: Var5(:) !Var5
      real,allocatable,save :: Var6(:) !Var6
      real,allocatable,save :: Var7(:) !Var7
      real,allocatable,save :: Var8(:) !Var8
      real,allocatable,save :: Var9(:) !Var9
      
      real, allocatable, save :: weights(:,:)      ! River loads fractions/weights
      integer, allocatable, save :: riversIJ(:,:)  ! Grid cell indices of river loads discharge locations

      type(netCDF_file) :: riverload_info(9)  !indices refer to order declared below in enum, 
      character(len=200) :: netcdf_riverload_fileNames(9)  !holds filenames of hydro netcdf data files
      integer, dimension(9) :: startRivIndex ! holds the last time index from netcdf file used for each riverload variable
                                          ! used as starting point for next lookup

      integer, parameter :: eVar1 = 1    !Var1
      integer, parameter :: eVar2 = 2    !Var2
      integer, parameter :: eVar3 = 3    !Var3
      integer, parameter :: eVar4 = 4    !Var4
      integer, parameter :: eVar5 = 5    !Var5
      integer, parameter :: eVar6 = 6    !Var6
      integer, parameter :: eVar7 = 7    !Var7
      integer, parameter :: eVar8 = 8    !Var8
      integer, parameter :: eVar9 = 9    !Var9

      integer, save :: fRv, lRv  ! looping index of FirstRiverVar and LastRiverVar 

      contains

      Subroutine Allocate_RiverLoads()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(Var1(nRiv))
      ALLOCATE(Var2(nRiv))
      ALLOCATE(Var3(nRiv))
      ALLOCATE(Var4(nRiv))
      ALLOCATE(Var5(nRiv))
      ALLOCATE(Var6(nRiv))
      ALLOCATE(Var7(nRiv))
      ALLOCATE(Var8(nRiv))
      ALLOCATE(Var9(nRiv))

      ALLOCATE(weights(nRiv,NSL))
      ALLOCATE(riversIJ(nRiv,2))

      !Fill values for netCDF
      Var1 = fill(0)  
      Var2 = fill(0)
      Var3 = fill(0)
      Var4 = fill(0)
      Var5 = fill(0)
      Var6 = fill(0)
      Var7 = fill(0)
      Var8 = fill(0)
      Var9 = fill(0)

      return

      End Subroutine Allocate_RiverLoads 


      Subroutine Init_RiverLoad_NetCDF()
      
      USE Model_dim

      IMPLICIT NONE

      integer :: i
      character(len=200) :: textfile

      !Set filenames for netCDF
      if (Which_gridio .eq. 1) then 
         write(netcdf_riverload_fileNames(eVar1), '(A, A)') trim(DATADIR), '/INPUT/TN_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar2), '(A, A)') trim(DATADIR), '/INPUT/NO3_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar3), '(A, A)') trim(DATADIR), '/INPUT/NH3_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar4), '(A, A)') trim(DATADIR), '/INPUT/DON_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar5), '(A, A)') trim(DATADIR), '/INPUT/TP_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar6), '(A, A)') trim(DATADIR), '/INPUT/DIP_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar7), '(A, A)') trim(DATADIR), '/INPUT/DOP_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar8), '(A, A)') trim(DATADIR), '/INPUT/BOD1_RiverLoads.nc'
         write(netcdf_riverload_fileNames(eVar9), '(A, A)') trim(DATADIR), '/INPUT/DO_RiverLoads.nc'
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
         fRv = 1;
         lRv = 9;
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
      
      End Subroutine Init_RiverLoad_NetCDF


      Subroutine Close_RiverLoad_NetCDF()

      IMPLICIT NONE
      integer :: i

      do i = fRv, lRv
        call close_netcdf(riverload_info(i)%ncid)
      enddo

      End Subroutine Close_RiverLoad_NetCDF


      End Module RiverLoad 
