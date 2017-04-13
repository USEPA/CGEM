!-------------------------------------------
!PI: John C. Lehrter, Ph.D.
!USEPA/NHEERL Gulf Ecology Division
!1 Sabine Island Drive
!Gulf Breeze, FL 32561-5299
!(850) 934-9255
!(850) 934-2401 -- fax
!lehrter.john@epa.gov      
!------------------------------------------------------------------------
!-------------------------------------------
! Fish Tank GEM
!-------------------------------------------
!Generalized CGEM code
!'Fish Tank' model- no advection or VMixing, no rivers or wind or fluxes
!Assume user wants Brad's light model, assume cell centered
!MPI code stripped out
!Assume no elevation changes (d is constant)
!No boundary conditions
!Only print out the f array
!There is no sinking...needs a sinking mechanism

!-----------------------------------
      Subroutine GoMDOM(input_filename,BASE_NETCDF_OUTPUT_FILE_NAME)
!-----------------------------------
 
!------------------------------------------------------------------------
!     Written  by :: Lisa L. Lowe/EMVL, lowe.lisa@epa.gov 
!                    D.S.Ko/NRL
!                    Wei Tang/EMVL
!     Simplified by Lisa Lowe.  This is my fish tank.  September 21, 2014
! -----------------------------------------------------------------------

     USE Model_dim
     USE INPUT_VARS_GD 
     USE STATES 
     USE OUTPUT_NETCDF_GD
     USE DATE_TIME 
     USE MASS_BALANCE_GD

     IMPLICIT NONE

!---------------------
! Declare Output parameters:    
!---------------------
     character(100) :: BASE_NETCDF_OUTPUT_FILE_NAME
     character(256) :: NETCDF_OUTPUT_FILE_NAME

     integer, parameter :: exit_code = 0 ! Status will be 0 when end
                                         ! of a successful run occurs.

!-----------------
     integer  :: iYr, iMon, iDay, iHr, iMin, iSec !Time variables

     integer  :: iout  !the output time-interval in timesteps
                             
     integer  :: istep !current time step        
     integer  :: istep_out !current output counter 

     integer  :: nstep !total number of timesteps in a run 

     integer(kind=8) :: TC_8 ! Current time in seconds since Model_dim::iYr0.
     integer(kind=8) :: START_SECONDS ! Seconds from iYr0 to start of run.
     integer(kind=8) :: END_SECONDS   ! Seconds from iYr0 to end of run.

     real, dimension(im,jm,nsl) :: d_sfc         !Distance from surface to center of cell

     real lat(im,jm),lon(im,jm) !Latitude and longitude of each grid cell
     real d(im,jm,nsl)    !Water depth at cell bottom 
     real Vol(im,jm,nsl)    !Volume of cell
     real area(im,jm,nsl)   !Area of cell

     real S(im,jm,nsl),T(im,jm,nsl)!S=Salinity, T=temperature in Celsius

     real f(im,jm,nsl,nf) !Array that holds value of nf state variables

     real dz(im,jm,nsl)  !Thickness of cell 

     real Rad(im,jm),Wind(im,jm) !Rad=Irradiance just below sea surface, Wind= Wind velocity (m/s)

     integer fm(im,jm)  ! land(0)/sea(1) mask
     integer wsm(im,jm)    ! shelf(0)/open ocean(1) mask

!---------------------
! Character variables 
!---------------------
#ifdef DEBUG       
      character(120) output_filename !Output file for debugging
#endif
      character(120) input_filename !Input file
!------------------------------------------------ 

! --- Inputs ----------------------------------------------------
!----------------------
! --- get grid location
!----------------------
      call USER_getLonLat(lat,lon)
      call USER_get_basic_grid(dz,d,d_sfc,Vol)
      area = Vol/dz
!---------------------------------------------------------------
! --- Read input data, from GEM_InputFile
!-------------------------------------------------------------
      call Read_InputFile_GD(input_filename,lat,lon)

#ifdef DEBUG
!This will check if Read_InputFile is correctly reading
       output_filename = "Debug_InputFile"
       call Write_InputFile_GD(output_filename)
       write(6,*) "DEBUG: Wrote Debug_InputFile, Stopping"
       stop
#endif
!--- END READ INPUT DATA --------------------------------------------


! ------------------
! ---  Initialization
! ------------------

! --- sinking speed: converted from m/s downward positive to m/s
! negative
      ws = -ws

    ! Compute starting time of run in seconds since Model_dim::iYr0:
      START_SECONDS = &
      TOTAL_SECONDS( iYr0, iYrS, iMonS, iDayS, iHrS, iMinS, iSecS )

      TC_8 = START_SECONDS

!----------------------------
! --- Set initial variables
!----------------------------
      if(Read_T.eq.0) then
        call Calc_Temp(START_SECONDS,TC_8,T)
      else
       call USER_Read(TC_8,T,'t')
      endif

      if(Read_Sal.eq.0) then
        call Calc_Sal(START_SECONDS,TC_8,S)
      else
        call USER_Read(TC_8,S,'s')
      endif


!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
      call USER_get_masks(fm,wsm)


!========================================Initialization of f==================
!Initialize f array
      if(InitializeHow.eq.0) then
       call Set_Initial_Conditions_GD(f,S,d_sfc,fm) !Regression Equations
       if(b_layer.ne.0) f(:,:,nz_max+1:nsl,:) = -9999
      else
       call USER_Set_Initial_Conditions_GD(f,fm,wsm)
      endif
!========End Initialization of f==============================================

!---------------------------------------------------
! Calc nstep , the total number of timesteps in a run	
!---------------------------------------------------       

      END_SECONDS = &
        TOTAL_SECONDS( iYr0, iYrE, iMonE, iDayE, iHrE, iMinE, iSecE )

      nstep = ( END_SECONDS - START_SECONDS ) / dT

!----------------------------------------------------------------------    
! Set  iout, the output time-interval in timesteps. 
! The total number of time-levels output to the NetCDF file is istep_out + 1,
! including the output for the initial time in the output time-interval.
!----------------------------------------------------------------------- 
      iout = dT_out/dT 
      istep = 0
      istep_out = 0

!-- Creates the output NetCDF file with static data: --------
      WRITE ( NETCDF_OUTPUT_FILE_NAME, '(A, I6.6, A)' )&
              trim(BASE_NETCDF_OUTPUT_FILE_NAME), istep_out, '.nc'
        ! Create a new NetCDF file in non-crash-restart cases only:
          CALL CREATE_FILE( trim(NETCDF_OUTPUT_FILE_NAME), &
                            im, jm, nsl, nstep, nf, EXTRA_VARIABLES, &
                            iYr0, &
                            IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS, &
                            IYRE, IMONE, IDAYE, IHRE, IMINE, ISECE, &
                            DT_OUT, &
                            LON, LAT, D, FM, &
                            DZ, AREA )
          CALL CLOSE_FILE()

! Opens the output file for writing:
       CALL OPEN_FILE( trim(NETCDF_OUTPUT_FILE_NAME), nf, EXTRA_VARIABLES, istep_out )

       CALL WRITE_DATA( im, jm, nsl, nf, istep_out, f)

       CALL WRITE_EXTRA_DATA( IM, JM, NSL, EXTRA_VARIABLES,  &
                               istep_out, SUM_DENITR, SUM_DENITR_C, &
                               SUM_DOCPRD, SUM_DOCMET, SUM_DOCZOO )

!------- End open netCDF file ------------------------------------------------

!-----------------------
! Initialization is done
!-----------------------

!---------------------------
! Beginning of the time-loop
!---------------------------

      TC_8 = START_SECONDS - dT / 2 ! Subtract half dT to 'center' of timestep.

!-------------- START TIME LOOP -----------------------------------
      do istep = 1, nstep 
       TC_8 = TC_8 + dT

!------------------------------------------------------------------
! Calculate iYr, iMon, iDay, iHr, iMin, iSec which 
! corresponds to the model timestamp, since iYr0 + TC_8 seconds.
!------------------------------------------------------------------
       CALL DATE_TIMESTAMP( iYr0, TC_8, &
                            iYr, iMon, iDay, iHr, iMin, iSec )

!------------------------------------------------------------------
! Read or calculate T, S
!------------------------------------------------------------------
      if(Read_T.eq.0) then
        call Calc_Temp(START_SECONDS,TC_8,T)
      else
       call USER_Read(TC_8,T,'t')
      endif

      if(Read_Sal.eq.0) then
        call Calc_Sal(START_SECONDS,TC_8,S)
      else
        call USER_Read(TC_8,S,'s')
      endif


!------------------- Get Solar Radiation --------------------------------
      if(Read_Solar.eq.0) then
       call getSolar( lon, lat, iYr, iMon, iDay, iHr, iMin, iSec, Rad)
      else
       call USER_Read(TC_8,Rad,'p')
      endif

!-------------- GEM - Biogeochemical Equations  ---------------------------
      call EUTRO(f,TC_8,T,S,Rad,lat,lon,fm,wsm,d,d_sfc,dz,Vol,dT)
!------------------------- Read Wind Data ------------------------
      if(Read_Wind.eq.0) then
       Wind=5. 
      else
       call USER_Read(TC_8,Wind,'w')
      endif

!-------- Surface and Bottom Fluxes ---------------------------------------
      call Flux_GD(f,T,S,wsm,fm,dz,Vol,Wind,TC_8)

! -------------- BEGIN OUTPUT DATA -------------------------------------------
! --- dump output when istep is a multiple of iout
      if (  mod( istep, iout ) .eq. 0 ) then
       istep_out = istep_out + 1
       CALL WRITE_DATA( im, jm, nsl, nf, istep_out, f)
       CALL WRITE_EXTRA_DATA( IM, JM, NSL, EXTRA_VARIABLES,  &
                               istep_out, SUM_DENITR, SUM_DENITR_C, &
                               SUM_DOCPRD, SUM_DOCMET, SUM_DOCZOO )
      endif 
!------------------------- END OUTPUT DATA -------------------------------------      

      enddo   
!-------------- END TIME LOOP -----------------------------------

      CALL CLOSE_FILE() ! Closes the output NetCDF file.

!----------------------------------------------------------------
! If we get here, there will be a normal exit from the program and
! exit code will be set to 0
!----------------------------------------------------------------
      call EXIT(exit_code)

!-----------------------------------      
      END SUBROUTINE GoMDOM 
!-----------------------------------  
