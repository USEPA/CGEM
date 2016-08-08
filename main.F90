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

!----------------------------------
      PROGRAM main 
!----------------------------------

!------------------------------------------------------------------------
!     Written  by :: Lisa L. Lowe/EMVL, lowe.lisa@epa.gov 
!                    D.S.Ko/NRL
!                    Wei Tang/EMVL
!                    Louis Olszyk/EMVL
!                    Barry E. Herchenroder/EMVL 
!     Simplified by Lisa Lowe.  This is my fish tank.  September 21, 2014
! -----------------------------------------------------------------------

      USE Model_dim

      IMPLICIT NONE

!---------------------
! Declare Variables:    
!---------------------
      character(100) :: BASE_NETCDF_OUTPUT_FILE_NAME

      integer, parameter :: exit_code = 0 ! Status will be 0 when end
                                         ! of a successful run occurs.
      integer c_count

      character*120 input_filename !Input file
!------------------------------------------------ 

! --- Command Line Arguments for file names ---
       c_count = command_argument_count()
         input_filename = "GEM_InputFile"
         BASE_NETCDF_OUTPUT_FILE_NAME = './NETCDF/output.'
       if (c_count.eq.1) then
         call get_command_argument(1,input_filename)  !User selects input file name
       elseif (c_count.eq.2) then
         call get_command_argument(1,input_filename)
         call get_command_argument(2,BASE_NETCDF_OUTPUT_FILE_NAME) !User selects output file name
       endif
       write(6,*) "Base Outputfile Name will be: ",trim(BASE_NETCDF_OUTPUT_FILE_NAME)
       write(6,*) "Inputfile will be: ",trim(input_filename)

! --- Read in Model_dim parameters
      open(unit=19,file="./data/Model_dim.txt", form="formatted", status="old")
      read(19,*) !Header
      read(19,*) im
      read(19,*) jm
      read(19,*) nz
      read(19,*) nsl 
      read(19,*) nospA
      read(19,*) nospZ
      !read(19,*) nf
      close(19)

      !if(nospA*3+nospZ+16.ne.nf) then
      !  write(6,*) "Wrong number of variables."
      !  write(6,*) "Variables should be nospA*3 + nospZ + 16"
      !  write(6,*) "I am going to change your nf to ",nospA*3+nospZ+16
        nf = nospA*3+nospZ+16
      !endif

!Calculate EXTRA_VARIABLES for netCDF:
     !ir,irfrac,uN(nospA),uP(nospA),uE(nospA),uA(nospA),Chla,s_xy(8),uSi(nospA),pH,ChlC(nospA),R_11
      EXTRA_VARIABLES = 13 + 6*nospA
 
      call CGEM_vars_allocate()
      call INPUT_VARS_allocate()
      call TEMP_VARS_allocate()
      call STOICH_VARS_allocate()
      call MASS_BALANCE_allocate()
      call DailyRad_allocate()
      call SDM_allocate()
      call JWMod_allocate()
      call OUTPUT_NETCDF_allocate()

! --- Call CGEM
      call CGEM(input_filename,BASE_NETCDF_OUTPUT_FILE_NAME)

!----------------------------------------------------------------
! If we get here, there will be a normal exit from the program and
! exit code will be set to 0
!----------------------------------------------------------------
      call EXIT(exit_code)

!-----------------------------------      
      END PROGRAM main 
!-----------------------------------  
