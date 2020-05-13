      MODULE BoundaryConcentration 

      USE netcdf_utils

      IMPLICIT NONE

      real,allocatable,save :: BCvar1(:) !BCvar1
      real,allocatable,save :: BCvar2(:) !BCvar2
      real,allocatable,save :: BCvar3(:) !BCvar3
      real,allocatable,save :: BCvar4(:) !BCvar4
      real,allocatable,save :: BCvar5(:) !BCvar5
      real,allocatable,save :: BCvar6(:) !BCvar6
      real,allocatable,save :: BCvar7(:) !BCvar7
      real,allocatable,save :: BCvar8(:) !BCvar8
      real,allocatable,save :: BCvar9(:) !BCvar9
      
      integer, allocatable, save :: bcIJ(:,:)  ! Grid cell indices of boundary concentration locations

      type(netCDF_file) :: boundaryconcentration_info(9)  !indices refer to order declared below in enum, 
      character(len=200) :: netcdf_boundaryconcentration_fileNames(9)  !holds filenames of hydro netcdf data files
      integer, dimension(9) :: startBcIndex ! holds the last time index from netcdf file used for each boundaryconcentration variable
                                            ! used as starting point for next lookup

      integer, parameter :: eBCvar1 = 1    !BCvar1
      integer, parameter :: eBCvar2 = 2    !BCvar2
      integer, parameter :: eBCvar3 = 3    !BCvar3
      integer, parameter :: eBCvar4 = 4    !BCvar4
      integer, parameter :: eBCvar5 = 5    !BCvar5
      integer, parameter :: eBCvar6 = 6    !BCvar6
      integer, parameter :: eBCvar7 = 7    !BCvar7
      integer, parameter :: eBCvar8 = 8    !BCvar8
      integer, parameter :: eBCvar9 = 9    !BCvar9

      integer, save :: fBCv, lBCv  ! looping index of FirstBoundaryConcentrationVar and LastBoundaryConcentrationVar 

      contains

      Subroutine Allocate_BoundaryConcentrations()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(BCvar1(nBC))
      ALLOCATE(BCvar2(nBC))
      ALLOCATE(BCvar3(nBC))
      ALLOCATE(BCvar4(nBC))
      ALLOCATE(BCvar5(nBC))
      ALLOCATE(BCvar6(nBC))
      ALLOCATE(BCvar7(nBC))
      ALLOCATE(BCvar8(nBC))
      ALLOCATE(BCvar9(nBC))

      ALLOCATE(bcIJ(nBC,2))

      !Fill values for netCDF
      BCvar1 = fill(0)  
      BCvar2 = fill(0)
      BCvar3 = fill(0)
      BCvar4 = fill(0)
      BCvar5 = fill(0)
      BCvar6 = fill(0)
      BCvar7 = fill(0)
      BCvar8 = fill(0)
      BCvar9 = fill(0)

      return

      End Subroutine Allocate_BoundaryConcentrations 


      Subroutine Init_BoundaryConcentration_NetCDF()
      
      USE Model_dim

      IMPLICIT NONE

      integer :: i
      character(len=200) :: textfile

      !Set filenames for netCDF
      if (Which_gridio .eq. 1) then 
         write(netcdf_boundaryconcentration_fileNames(eBCvar1), '(A, A)') trim(DATADIR), '/INPUT/TN_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar2), '(A, A)') trim(DATADIR), '/INPUT/NO3_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar3), '(A, A)') trim(DATADIR), '/INPUT/NH4_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar4), '(A, A)') trim(DATADIR), '/INPUT/DON_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar5), '(A, A)') trim(DATADIR), '/INPUT/TP_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar6), '(A, A)') trim(DATADIR), '/INPUT/DIP_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar7), '(A, A)') trim(DATADIR), '/INPUT/DOP_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar8), '(A, A)') trim(DATADIR), '/INPUT/BOD_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBCvar9), '(A, A)') trim(DATADIR), '/INPUT/DO_BoundaryConcentrations.nc'
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
         fBCv = 1;
         lBCv = 9;
      else if (Which_gridio .eq. 3) then  !POM does not use Salinity
!         fHv = 2;
!         lHv = 9
      endif
      do i = fBCv, lBCv
        call open_netcdf(netcdf_boundaryconcentration_fileNames(i), 0, boundaryconcentration_info(i)%ncid)
        boundaryconcentration_info(i)%fileName = netcdf_boundaryconcentration_fileNames(i)
        call init_info(boundaryconcentration_info(i))
#ifdef DEBUG
call report_info(boundaryconcentration_info(i))
#endif
      enddo

      startBcIndex = 1

      write(textfile,'(A, A)') trim(DATADIR),'/BCindices.dat'
      open(19, file=textfile, status='old')
      read(19,*)    ! I and J indices of boundary concentration locations.
      do i = 1, nBC
         read(19,*) bcIJ(i,1:2)
      enddo
      close(19)
      
      
      End Subroutine Init_BoundaryConcentration_NetCDF


      Subroutine Close_BoundaryConcentration_NetCDF()

      IMPLICIT NONE
      integer :: i

      do i = fBCv, lBCv
        call close_netcdf(boundaryconcentration_info(i)%ncid)
      enddo

      End Subroutine Close_BoundaryConcentration_NetCDF


      End Module BoundaryConcentration 
