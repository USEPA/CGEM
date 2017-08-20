!-------------------------------------------
!PI: John C. Lehrter, Ph.D.
!USEPA/NHEERL Gulf Ecology Division
!1 Sabine Island Drive
!Gulf Breeze, FL 32561-5299
!(850) 934-9255
!(850) 934-2401 -- fax
!lehrter.john@epa.gov      
!-------------------------------------------
!Generalized CGEM/GoMDOM code

!----------------------------------
      PROGRAM main 
!----------------------------------

!------------------------------------------------------------------------
!     Written  by :: Lisa L. Lowe/EMVL, lowe.lisa@epa.gov 
!                    D.S.Ko/NRL
!                    Wei Tang/EMVL
!                    Louis Olszyk/EMVL
!     Simplified by Lisa Lowe.  This is my fish tank.  September 21, 2014
! -----------------------------------------------------------------------

      USE Model_dim
      USE Grid
      USE Hydro
      USE State_Vars
      USE INPUT_VARS

      IMPLICIT NONE

!---------------------
! Declare Variables:    
!---------------------
      integer, parameter :: exit_code = 0   ! Status 0 when run is successful 
      integer(kind=8) :: TC_8 ! Current time in seconds since Model_dim::iYr0.
      integer  :: istep, istep_out
      character(120) input_filename         !Input file
      character(120) init_filename         !Initial conditions file
      character(6) Which_code
      character(100) :: BASE_NETCDF_OUTPUT_FILE_NAME

!------------------------------------------------ 

! --- Command Line Arguments for file names ---
      call Command_Line_Args(Which_code,input_filename,init_filename,BASE_NETCDF_OUTPUT_FILE_NAME)

      call Set_Model_dim()

      call Set_Grid(TC_8)

      call Allocate_Input(Which_code)

      call Allocate_Hydro
      !L3 Take care of ws here

! Read_InputFile must define nstep, iout, dT, START_SECONDS
      call Read_InputFile(input_filename,Which_code) 

#ifdef DEBUG
write(6,*) "After Read_InputFile"
      write(6,*) "start,dT",START_SECONDS, dT
#endif

!L3--- This next line is from Ko, but I have no idea why this is necessary.
! Check to see if we can just use START-dT or move the time increment to
! the bottom of the loop.

      TC_8 = START_SECONDS - (dT / 2) ! Subtract half dT to 'center' of timestep.

      if (Which_gridio .gt. 0) then
        call Init_Hydro_NetCDF()
      endif
  
      call Get_Vars(TC_8) !Hydro for initial timestep 

      call Set_Vars(Which_code,init_filename) !initialize 'f' array

#ifdef DEBUG
write(6,*) "After Set_Vars"
      write(6,*) "S,T",S,T
#endif

! Add blip for testing advection
!#ifdef DEBUG_CWS
!      f(12,25,1,25) = 300
!#endif

!L3-move!      call Initialize_Output(Which_code,BASE_NETCDF_OUTPUT_FILE_NAME)     !Open file and write initial configuration

! Initialize time an loop variables
      istep = 0
      istep_out = 0

      if (Which_gridio.eq.1) then
        call USER_update_EFDC_grid(TC_8)
      else if (Which_gridio.eq.2) then
        call USER_update_NCOM_grid(TC_8)
      endif


      call Initialize_Output(Which_code,BASE_NETCDF_OUTPUT_FILE_NAME)     !Open file and write initial configuration


#ifdef DEBUG_CWS
nstep = 1 
write(6,*)"DEBUG - setting nstep to ",nstep
#endif

#ifdef CALIBRATE
     write(6,*) "O2 initial",f(1,1,1,10)
#endif

!-------------- START TIME LOOP -----------------------------------
      do istep = 1, nstep
       TC_8 = TC_8 + dT

#ifdef DEBUG
      write(6,*) "TC_8",TC_8
#endif 

      if (Which_gridio.eq.1) then
        Vol_prev = Vol
        call USER_update_EFDC_grid(TC_8)
      elseif (Which_gridio.eq.2) then
        Vol_prev = Vol
        call USER_update_NCOM_grid()
      endif


       call Get_Vars(TC_8) !Hydro, Solar, Wind, Temp, Salinity

#ifdef DEBUG
write(6,*) "After Get_Vars"
write(6,*) "T",T
write(6,*) "S",S
write(6,*) "Rad",Rad
write(6,*) "Wind",Wind
write(6,*) "E",E
write(6,*) "Kh",Kh
write(6,*) "Ux",Ux
write(6,*) "Vx",Vx
write(6,*) "Wx",Wx
#endif


     
     if (Which_gridio.eq.1) then
       call USER_update_masks()
     endif

       call WQ_Model(Which_code,TC_8,istep,istep_out)

#ifdef DEBUG
write(6,*) "After WQ_Model"
write(6,*) "istep=",istep
write(6,*) "CDOM",f(1,1,1:nsl,32)
#endif
    
       call Flux(Which_code,istep)

#ifdef DEBUG
write(6,*) "After Flux"
write(6,*) "CDOM",f(1,1,1:nsl,32)
#endif

       call Transport(Which_code)

#ifdef DEBUG
write(6,*) "After Transport"
write(6,*) "CDOM",f(1,1,1:nsl,32)
#endif

      ! -------------- BEGIN OUTPUT DATA
      ! --- dump output when istep is a multiple of iout
       if (  mod( istep, iout ) .eq. 0 ) then
        istep_out = istep_out + 1
        if(Which_gridio.ne.0) write(6,*) "output=",istep_out+1
        call Model_Output(Which_Code,istep_out)
       endif

#ifdef DEBUG_BASIC
write(6,*) "After Model_Output"
#endif

      enddo

      Call Model_Finalize(Which_code) ! Closes the output NetCDF file and whatever else
      if (Which_gridio.eq.1) then
        Call Close_Hydro_NetCDF()
        Call Close_Grid_NetCDF()
      endif

!     write(6,*) "A,Z1,Z2:", f(1,1,1,iA(1)),f(1,1,1,iZ(1),f(1,1,1,iZ(2)

#ifdef CALIBRATE
      write(6,*) "O2 final: ",f(1,1,1,10)
#ifdef CAL_LT
      write(6,*) "Percent Error Light (measured=229.57) = ",(229.57 - f(1,1,1,10))/229.57 * 100
#endif
#ifdef CAL_DK
      write(6,*) "Percent Error Dark  (measured=195.51) = ",(195.51 - f(1,1,1,10))/195.51 * 100
#endif
#ifdef CAL_LTNT
      write(6,*) "Percent Error LTNT  (measured=253.43) = ",(253.43 - f(1,1,1,10))/253.43 * 100
#endif
#endif

!----------------------------------------------------------------
! If we get here, there will be a normal exit from the program and
! exit code will be set to 0
!----------------------------------------------------------------
      call EXIT(exit_code)

!-----------------------------------      
      END PROGRAM main 
!-----------------------------------  
