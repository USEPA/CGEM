      Subroutine Command_Line_Args(Which_code,input_filename,init_filename,BASE_NETCDF_OUTPUT_FILE_NAME, DIntRates_filename)

      character(120),intent(out) :: input_filename !Input file
      character(120),intent(out) :: init_filename !Initial conditions file
      character(6), intent(out) ::  Which_code     
      character(100), intent(out) :: BASE_NETCDF_OUTPUT_FILE_NAME
      character(100), intent(out) :: DIntRates_filename

      integer c_count

! ./CGEM Which_code InputFile InitializationFile OutputFileBase 

! --- Command Line Arguments for file names ---
         c_count = command_argument_count()

     !Set Defaults: 
         Which_code = "CGEM"
         input_filename = "GEM_InputFile"
         init_filename = "InitialConditions.txt"
         BASE_NETCDF_OUTPUT_FILE_NAME = './NETCDF/cgem.'
         DIntRates_filename = './NETCDF/CGEM_DailyIntegrated_Rates.nc'

       if (c_count.gt.0) then
         call get_command_argument(1,Which_code)  !User selects which code
         !Some spellings for cgem 
         if(Which_code.eq."CGEM".or.Which_code.eq."cgem") then
           Which_code = "CGEM"
         endif
         !Some spellings for wqem
         if(Which_code.eq."WQEM" .or. Which_code.eq."wqem") then
           Which_code = "WQEM"
           input_filename = "WQEM_InputFile"
           init_filename = "InitialConditions_WQEM.txt"
           BASE_NETCDF_OUTPUT_FILE_NAME = './NETCDF/wqem.'
         endif
       endif

#ifdef CAL_LT
         if(Which_code.eq."CGEM") then
          init_filename = "InitialConditions.lt.txt"
         else
          init_filename = "InitialConditions_WQEM.lt.txt"
         endif

#endif
#ifdef CAL_DK
         if(Which_code.eq."CGEM") then
          init_filename = "InitialConditions.lt.txt"
         else
          init_filename = "InitialConditions_WQEM.lt.txt"
         endif
#endif

#ifdef CAL_LTNT
         if(Which_code.eq."CGEM") then
          init_filename = "InitialConditions.lt.txt"
         else
          init_filename = "InitialConditions_WQEM.ltnt.txt"
         endif
#endif


       if (c_count.gt.1) then
         call get_command_argument(2,input_filename)  !User selects input file name
       endif

       if (c_count.gt.2) then
         call get_command_argument(3,init_filename) !User selects initial conditions file name
       endif

       if (c_count.gt.3) then
         call get_command_argument(4,BASE_NETCDF_OUTPUT_FILE_NAME) !User selects output file name
       endif

       if (c_count.gt.4) then
         call get_command_argument(5,DIntRates_filename) !User selects daily-integrated rates file name
       endif

       write(6,*) "Biogeochem equations are: ",Which_code
       write(6,*) "Inputfile will be: ",trim(input_filename)
       write(6,*) "Initial Conditions filename will be: ",trim(init_filename)
       write(6,*) "Base Outputfile Name will be: ",trim(BASE_NETCDF_OUTPUT_FILE_NAME)
 
       if(Which_code.eq."CGEM") then
          write(6,*) "Daily-Integrated Rates output filename will be: ", trim(DIntRates_filename)
       endif

       return

       END Subroutine Command_Line_Args
