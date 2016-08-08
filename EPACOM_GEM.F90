!#DEFINE DEBUG

!-------------------------------------------
! Fish Tank GEM
!-------------------------------------------
!Generalized EPACOM_GEM code 
!'Fish Tank' model- no advection or VMixing, no rivers or wind or fluxes
!Assume user wants Brad's light model, assume cell centered
!MPI code stripped out
!Assume no elevation changes (d is constant)
!No boundary conditions
!Only print out the f array
!There is no sinking...needs a sinking mechanism
!---------------------------------
      PROGRAM  EPACOM_GEM
!-----------------------------------

!------------------------------------------------------------------------
!     Written  by :: D.S.Ko/NRL  
!     Modified by :: Barry E. Herchenroder/EMVL  , April/May 2010 & later
!                    Todd Plessel/EMVL           , April/May 2010
!     Simplified by Lisa Lowe.  This is my fish tank.  September 21, 2014
! -----------------------------------------------------------------------

      USE Model_dim
      USE INPUT_VARS
      USE RKVectorMod
      USE EPA_GEM_Params
      USE OUTPUT_NETCDF

      IMPLICIT NONE

!----------------------
! Variable Declarations
!----------------------
      INTEGER, PARAMETER :: exit_code = 2 ! Status will be 2 when end
                                         ! of a successful run occurs.
      real(KIND=8), parameter :: SDay_8   = 86400.0_8       ! secs in a day
      integer  :: i, j, k, ii !loop variables
      integer  :: iout  !the output time-interval in timesteps
      integer  :: istep, istep_out !current time step, current output time step        
      integer  :: nstep !total number of timesteps in a run 
      real(KIND=8) :: sec_tot_8 !number of seconds in a run 
      real(KIND=8) :: TC_8 !Hold current time in decimal days
      real, dimension(im,jm,nsl) :: d_sfc !Distance from surface to center of cell
      real, dimension(im,jm,nsl) :: dz    !Thickness of cell
      real rlat(jm),rlon(im) !Latitude and longitude of each grid cell
      real d(im,jm)          !Water depth 
      real S(im,jm,NSL),T(im,jm,NSL)!S=Salinity in psu, T=temperature in Celsius
      real Wsp(im,jm)      !Windspeed
      real f(im,jm,nsl,nf) !Array that holds value of 41 state variables
      real Rad(im,jm) !Rad=Irradiance at top of sea surface, Wind= Wind velocity (m/s)
      real RadBot(im,jm) !PAR at sea bottom
      integer fm(im,jm)  ! land(0)/sea(1) mask
      real wsm(im,jm)    ! shelf(0)/open ocean(1) mask
      character*120 input_filename !Input file
#ifdef DEBUG
      character*120 output_filename !Output file for debugging
#endif
      CHARACTER(100) :: BASE_NETCDF_OUTPUT_FILE_NAME = './NETCDF/output.'
!'
      CHARACTER(256) NETCDF_OUTPUT_FILE_NAME
      INTEGER OUTPUT_TIMESTEPS

!---------------------- 

! --- Inputs ----------------------------------------------------
!----------------------
! --- get grid location
!----------------------
      call USER_getLonLat(rlat,rlon)
      call USER_get_basic_grid(dz,d,d_sfc)

!---------------------------------------------------------------
! --- Read input data, from GEM_InputFile 
!------------------------------------------------------------- 
      input_filename = "GEM_InputFile"
      call Read_InputFile(input_filename,rlat,rlon)

#ifdef DEBUG
!This will check if Read_InputFile is correctly reading
       output_filename = "Debug_InputFile"
       call Write_InputFile(output_filename)
       write(6,*) "DEBUG: Wrote Debug_InputFile, Stopping"
       stop
#endif

! ------------------
! ---  Initialization
! ------------------
      call Calc_RKVector !Initialize RKVectorMod Values

!  DayS_8 is calculated in Read_Inputfile 
      TC_8  = DayS_8   ! Starting time (decimal days), WRT the beginning of 2002

!----------------------------
! --- Set initial variables
!----------------------------
      call USER_get_Vars(TC_8,S,T) 

!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
      call USER_get_masks(fm,wsm)


!========================================Initialization of f==================  
!Initialize f array
      if(InitializeHow.eq.0) then
       call Set_Initial_Conditions(f,S,d_sfc,fm) !Regression Equations
      else
       call USER_Set_Initial_Conditions(f,fm,wsm)
      endif
!========End Initialization of f==============================================


!------------------------------------------------------------------       
! Calc. sec_tot, the decimal number of seconds in a run. 
!------------------------------------------------------------------
      sec_tot_8 = (DayE_8-DayS_8)*24.0_8*3600.0_8              

!---------------------------------------------------
! Calc nstep , the total number of timesteps in a run	
!---------------------------------------------------       
      nstep = NINT(sec_tot_8/REAL(dT,KIND=8),KIND=4)
      
!----------------------------------------------------------------------    
! Set  iout, the output time-interval in timesteps. 
!
! The total number of time-levels output to the NetCDF file is iout + 1, 
! including the output for the initial time in the output time-interval.
!----------------------------------------------------------------------- 
      iout   = dT_out/dT 
!----------------------------------------------------------------------  

! Open Calibration and debugging files:
       if(Calibration.eq.1) call Output_Data_Calibration_open()

      istep = 0
      istep_out     = 0

      if(Out_1D.eq.2) then
! Creates the output NetCDF file with static data:
      OUTPUT_TIMESTEPS = 1 + nstep / iout

      WRITE ( NETCDF_OUTPUT_FILE_NAME, '(A, I6.6, A)' )&
              trim(BASE_NETCDF_OUTPUT_FILE_NAME), istep_out, '.nc'

        CALL CREATE_FILE( trim(NETCDF_OUTPUT_FILE_NAME), &
                          OUTPUT_TIMESTEPS - 1, &
                          DT_OUT, RLON, RLAT, d, FM, d_sfc )
        CALL CLOSE_FILE()

! Open the output file for writing:
      CALL OPEN_FILE( trim(NETCDF_OUTPUT_FILE_NAME) )

! Write initial configuration
      CALL WRITE_SUBSET_DATA( 1, IM, 1, JM, 1, NZP1, 0, f )
      endif

!-----------------------
! Initialization is done
!-----------------------

!---------------------------
! Beginning of the time-loop
!---------------------------
!-------------- START TIME LOOP -----------------------------------
      do istep = 1, nstep

      if(mod(istep,iout).eq.0.or.istep.eq.1)  then
        write(6,*) "istep=",istep,"of",nstep
       if(Out_1D.eq.1) then
        call Output_Data_1D_text(f,istep,icent,jcent)
        !elseif(Out_1D.eq.3) then
        !call USER_output_data(f,istep)
       endif
      endif

! ----------------------------------------------------------------------
      TC_8 = DayS_8  +                                                 &
           REAL(dT,KIND=8)*(REAL(istep,KIND=8)-0.5_8)/SDay_8
                                                ! Model time in decimal days-- 
                                                ! This is the time at
                                                ! the center of timestep istep

!Get T, S for TC_8
      call USER_get_Vars(TC_8,S,T)

!------------------- Get Solar Radiation --------------------------------
      !Uncomment when read in routine is written
      !if(solarRadKo.eq.1) then
      ! call USER_Read_Solar(TC_8,Rad)
      !else
       call getSolar( rlon, rlat, TC_8, Rad)
      !endif

!-------------- GEM - Biogeochemical Equations  ---------------------------
      call  GEM_EPA( rlat, rlon, TC_8, f, d, d_sfc, Rad, RadBot, T, wsm, fm, istep, iout )

!------------- Bottom and Surface Fluxes --------------------------------

      !Read In Wind Speed
      call USER_Read_Wind(TC_8,Wsp)

      call Flux(f,T,S,RadBot,wsm,fm,dz,Wsp)

!------------ Max/Min Check ----------------------------------
      !do ii = 1, nf
      ! do k = 1, nzp1
      !  do j = 1, jm
      !   do i = 1, im 
      !    if(fm(i,j).eq.1) then
      !       f(i,j,k,ii) = AMIN1(AMAX1(f(i,j,k,ii) ,fmin(ii)),fmax(ii))
      !    endif
      !   enddo
      !  enddo
      ! enddo
      !enddo

! -------------- BEGIN OUTPUT DATA -------------------------------------------
! --- dump 3d output when istep is a multiple of iout
      if (  mod( istep, iout ) .eq. 0 ) then

       istep_out = istep_out + 1

       if(Out_1D.eq.2) CALL WRITE_SUBSET_DATA( 1, IM, 1, JM, 1, NZP1, istep_out, f )

      endif  !end of "if (mod(istep,iout).eq.0)" block if
!------------------------- END OUTPUT DATA -------------------------------------

!-------------------------------------------------------------------
      enddo   ! End of  "do istep = 1, nstep" block do timeloop.
!--------------------------------------------------------------------	 

      if(Out_1D.eq.1) then
       call Close_Output_Data_1D_text()
      !elseif(Out_1D.eq.3) then
      ! call USER_close_file()
      endif

! Close Calibration and debugging files:
       if(Calibration.eq.1) call Output_Data_Calibration_close()

       if(Out_1D.eq.2) CALL CLOSE_FILE() ! Each process closes the output NetCDF file.

!----------------------------------------------------------------
! If we get here, there will be a normal exit from the program and
! exit code will be set to 2
!----------------------------------------------------------------
      CALL EXIT(exit_code)

!-----------------------------------      
      END PROGRAM EPACOM_GEM
!-----------------------------------  
