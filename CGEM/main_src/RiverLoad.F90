      MODULE RiverLoad 

      USE netcdf_utils

      IMPLICIT NONE

      real,allocatable,save :: Riv_NO3(:) !Var1
      real,allocatable,save :: Riv_NH3(:) !Var2
      real,allocatable,save :: Riv_DON(:) !Var3
      real,allocatable,save :: Riv_TP(:) !Var4
      real,allocatable,save :: Riv_DIP(:) !Var5
      real,allocatable,save :: Riv_DOP(:) !Var6
      real,allocatable,save :: Riv_DO(:) !Var7
      real,allocatable,save :: Riv_BOD1(:) !Var8
      real,allocatable,save :: Riv_TN(:) !Var9


      real,allocatable,save :: Riv_NO3A(:) !Var1
      real,allocatable,save :: Riv_NH3A(:) !Var2
      real,allocatable,save :: Riv_DONA(:) !Var3
      real,allocatable,save :: Riv_TPA(:) !Var4
      real,allocatable,save :: Riv_DIPA(:) !Var5
      real,allocatable,save :: Riv_DOPA(:) !Var6
      real,allocatable,save :: Riv_DOA(:) !Var7
      real,allocatable,save :: Riv_BOD1A(:) !Var8
      real,allocatable,save :: Riv_TNA(:) !Var9
      real,allocatable,save :: Riv_NO3B(:) !Var1
      real,allocatable,save :: Riv_NH3B(:) !Var2
      real,allocatable,save :: Riv_DONB(:) !Var3
      real,allocatable,save :: Riv_TPB(:) !Var4
      real,allocatable,save :: Riv_DIPB(:) !Var5
      real,allocatable,save :: Riv_DOPB(:) !Var6
      real,allocatable,save :: Riv_DOB(:) !Var7
      real,allocatable,save :: Riv_BOD1B(:) !Var8
      real,allocatable,save :: Riv_TNB(:) !Var9
      
      real, allocatable, save :: weights(:,:)      ! River loads fractions/weights
      integer, allocatable, save :: riversIJ(:,:)  ! Grid cell indices of river loads discharge locations

      real, allocatable, save :: River_InFlow(:)     ! River inflow used in the 0D case.
      real, allocatable, save :: River_OutFlow(:)    ! River outflow used in the 0D case.
      real, allocatable, save :: River_Conc(:)      ! River nutrient concentration
      
      type(netCDF_file) :: riverload_info(9)  !indices refer to order declared below in enum, 
      character(len=200) :: netcdf_riverload_fileNames(9)  !holds filenames of hydro netcdf data files
      integer, dimension(9) :: startRivIndex ! holds the last time index from netcdf file used for each riverload variable
                                          ! used as starting point for next lookup

      integer, parameter :: eRiv_NO3 = 1    !Var1
      integer, parameter :: eRiv_NH3 = 2    !Var2
      integer, parameter :: eRiv_DON = 3    !Var3
      integer, parameter :: eRiv_TP = 4    !Var4
      integer, parameter :: eRiv_DIP = 5    !Var5
      integer, parameter :: eRiv_DOP = 6    !Var6
      integer, parameter :: eRiv_DO = 7    !Var7
      integer, parameter :: eRiv_BOD1 = 8    !Var8
      integer, parameter :: eRiv_TN = 9    !Var9

      integer, save :: fRv, lRv  ! looping index of FirstRiverVar and LastRiverVar

      integer(kind=8), save :: river_tc1, river_tc2  !bookend time values for river variables

      contains

      Subroutine Allocate_RiverLoads(Which_code)

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      character(6), intent(in) :: Which_code
      character(len=200) :: textfile

      integer :: i

      print*,"Allocating riverloads"

      if (Which_code.eq."CGEM") then
         ALLOCATE(Riv_NO3(nRiv))
         ALLOCATE(Riv_NH3(nRiv))
         ALLOCATE(Riv_DON(nRiv))
         ALLOCATE(Riv_TP(nRiv))
         ALLOCATE(Riv_DIP(nRiv))
         ALLOCATE(Riv_DOP(nRiv))
         ALLOCATE(Riv_DO(nRiv))
         ALLOCATE(Riv_BOD1(nRiv))
         ALLOCATE(Riv_TN(nRiv))
         ALLOCATE(Riv_NO3A(nRiv))
         ALLOCATE(Riv_NO3B(nRiv))
         ALLOCATE(Riv_NH3A(nRiv))
         ALLOCATE(Riv_NH3B(nRiv))
         ALLOCATE(Riv_DONA(nRiv))
         ALLOCATE(Riv_DONB(nRiv))
         ALLOCATE(Riv_TPA(nRiv))
         ALLOCATE(Riv_TPB(nRiv))
         ALLOCATE(Riv_DIPA(nRiv))
         ALLOCATE(Riv_DIPB(nRiv))
         ALLOCATE(Riv_DOPA(nRiv))
         ALLOCATE(Riv_DOPB(nRiv))
         ALLOCATE(Riv_DOA(nRiv))
         ALLOCATE(Riv_DOB(nRiv))
         ALLOCATE(Riv_BOD1A(nRiv))
         ALLOCATE(Riv_BOD1B(nRiv))
         ALLOCATE(Riv_TNA(nRiv))
         ALLOCATE(Riv_TNB(nRiv))
      else if (Which_code.eq."WQEM") then
         ALLOCATE(Riv_NO3(nRiv))
         ALLOCATE(Riv_NH3(nRiv))
         ALLOCATE(Riv_DON(nRiv))
         ALLOCATE(Riv_TP(nRiv))
         ALLOCATE(Riv_DIP(nRiv))
         ALLOCATE(Riv_DOP(nRiv))
         ALLOCATE(Riv_DO(nRiv))
         ALLOCATE(Riv_NO3A(nRiv))
         ALLOCATE(Riv_NO3B(nRiv))
         ALLOCATE(Riv_NH3A(nRiv))
         ALLOCATE(Riv_NH3B(nRiv))
         ALLOCATE(Riv_DONA(nRiv))
         ALLOCATE(Riv_DONB(nRiv))
         ALLOCATE(Riv_TPA(nRiv))
         ALLOCATE(Riv_TPB(nRiv))
         ALLOCATE(Riv_DIPA(nRiv))
         ALLOCATE(Riv_DIPB(nRiv))
         ALLOCATE(Riv_DOPA(nRiv))
         ALLOCATE(Riv_DOPB(nRiv))
         ALLOCATE(Riv_DOA(nRiv))
         ALLOCATE(Riv_DOB(nRiv)) 
      else
           write(6,*) "Model ", Which_code," not found in RiverLoad.f90"
           write(6,*) "Exiting"
           stop        
      endif

      ALLOCATE(weights(nRiv,NSL))
      ALLOCATE(riversIJ(nRiv,2))

      ALLOCATE(River_InFlow(nRiv))
      ALLOCATE(River_OutFlow(nRiv))
      ALLOCATE(River_Conc(4))

      !Fill values for netCDF
      if (Which_code.eq."CGEM") then
         Riv_NO3 = fill(0)
         Riv_NH3 = fill(0)
         Riv_DON = fill(0)
         Riv_TP = fill(0)
         Riv_DIP = fill(0)
         Riv_DOP = fill(0)
         Riv_DO = fill(0)
         Riv_BOD1 = fill(0)
         Riv_TN = fill(0)
         Riv_NO3A = fill(0)  
         Riv_NO3B = fill(0) 
         Riv_NH3A = fill(0)  
         Riv_NH3B = fill(0)  
         Riv_DONA = fill(0)  
         Riv_DONB = fill(0)  
         Riv_TPA = fill(0)  
         Riv_TPB = fill(0)  
         Riv_DIPA = fill(0)  
         Riv_DIPB = fill(0)  
         Riv_DOPA = fill(0)  
         Riv_DOPB = fill(0)  
         Riv_DOA = fill(0)  
         Riv_DOB = fill(0)  
         Riv_BOD1A = fill(0)  
         Riv_BOD1B = fill(0)  
         Riv_TNA = fill(0)  
         Riv_TNB = fill(0)
         River_InFlow = fill(0)
         River_OutFlow = fill(0)
         River_Conc = fill(0)
      else if (Which_code.eq."WQEM") then
         Riv_NO3 = fill(0)
         Riv_NH3 = fill(0)
         Riv_DON = fill(0)
         Riv_TP = fill(0)
         Riv_DIP = fill(0)
         Riv_DOP = fill(0)
         Riv_DO = fill(0)
         Riv_NO3A = fill(0)  
         Riv_NO3B = fill(0) 
         Riv_NH3A = fill(0)  
         Riv_NH3B = fill(0)  
         Riv_DONA = fill(0)  
         Riv_DONB = fill(0)  
         Riv_TPA = fill(0)  
         Riv_TPB = fill(0)  
         Riv_DIPA = fill(0)  
         Riv_DIPB = fill(0)  
         Riv_DOPA = fill(0)  
         Riv_DOPB = fill(0)  
         Riv_DOA = fill(0)  
         Riv_DOB = fill(0) 
      else
           write(6,*) "Model ", Which_code," not found in RiverLoad.f90"
           write(6,*) "Exiting"
           stop 
      endif

      write(textfile,'(A, A)') trim(DATADIR),'/RiverIndices.dat'
      open(19, file=textfile, status='old')
      read(19,*)    ! I and J indices of river discharge locations.
      do i = 1, nRiv
         read(19,*) riversIJ(i,1:2)
         PRINT*, "riversIJ(i,1:2) = ", riversIJ(i,1:2)
      enddo
      close(19)
      
      write(textfile,'(A, A)') trim(DATADIR),'/RiverWeights.dat'
      open(19, file=textfile, status='old')
      do i = 1, nRiv
         read(19,*) weights(i,:)
      enddo
      close(19)
      
      return

      End Subroutine Allocate_RiverLoads 


      Subroutine Init_RiverLoad_NetCDF(Which_code)
      
      USE Model_dim

      IMPLICIT NONE

      character(6), intent(in) :: Which_code

      integer :: i

      !Set filenames for netCDF
      if (Which_gridio .eq. 1) then
         if(Which_code.eq."CGEM") then
            write(netcdf_riverload_fileNames(eRiv_NO3), '(A, A)') trim(DATADIR), '/INPUT/NO3_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_NH3), '(A, A)') trim(DATADIR), '/INPUT/NH3_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DON), '(A, A)') trim(DATADIR), '/INPUT/DON_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_TP), '(A, A)') trim(DATADIR), '/INPUT/TP_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DIP), '(A, A)') trim(DATADIR), '/INPUT/DIP_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DOP), '(A, A)') trim(DATADIR), '/INPUT/DOP_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DO), '(A, A)') trim(DATADIR), '/INPUT/DO_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_BOD1), '(A, A)') trim(DATADIR), '/INPUT/BOD1_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_TN), '(A, A)') trim(DATADIR), '/INPUT/TN_RiverLoads.nc'
         else if(Which_code.eq."WQEM") then 
            write(netcdf_riverload_fileNames(eRiv_NO3), '(A, A)') trim(DATADIR), '/INPUT/NO3_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_NH3), '(A, A)') trim(DATADIR), '/INPUT/NH3_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DON), '(A, A)') trim(DATADIR), '/INPUT/DON_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_TP), '(A, A)') trim(DATADIR), '/INPUT/TP_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DIP), '(A, A)') trim(DATADIR), '/INPUT/DIP_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DOP), '(A, A)') trim(DATADIR), '/INPUT/DOP_RiverLoads.nc'
            write(netcdf_riverload_fileNames(eRiv_DO), '(A, A)') trim(DATADIR), '/INPUT/DO_RiverLoads.nc'
         else
           write(6,*) "Model ", Which_code," not found in RiverLoad.f90"
           write(6,*) "Exiting"
           stop
         endif
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
         if(Which_code.eq."CGEM") then
            lRv = 9;
         else if(Which_code.eq."WQEM") then
            lRv = 7
         else
            write(6,*) "Model ",Which_code," not found in RiverLoad.f90"
            write(6,*) "Exiting"
            stop
         endif
      else if (Which_gridio .eq. 3) then  !POM does not use Salinity
!         fHv = 2;
!         lHv = 9
      endif
      
      if (Which_gridio > 0) then
          do i = fRv, lRv
             call open_netcdf(netcdf_riverload_fileNames(i), 0, riverload_info(i)%ncid)
             riverload_info(i)%fileName = netcdf_riverload_fileNames(i)
             call init_info(riverload_info(i))
#ifdef DEBUG
  call report_info(riverload_info(i))
#endif
          enddo
      endif
      
      startRivIndex = 1

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
