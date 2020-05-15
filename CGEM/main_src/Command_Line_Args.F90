      Subroutine Command_Line_Args(Which_code,input_filename,init_filename,BASE_NETCDF_OUTPUT_FILE_NAME)

      character(120),intent(out) :: input_filename !Input file
      character(120),intent(out) :: init_filename !Initial conditions file
      character(6), intent(out) ::  Which_code     
      character(100), intent(out) :: BASE_NETCDF_OUTPUT_FILE_NAME

      integer c_count

! ./CGEM Which_code InputFile InitializationFile OutputFileBase 

! --- Command Line Arguments for file names ---
         c_count = command_argument_count()

     !Set Defaults: 
         Which_code = "CGEM"
         input_filename = "GEM_InputFile"
         init_filename = "InitialConditions.txt"
         BASE_NETCDF_OUTPUT_FILE_NAME = './NETCDF/cgem.'


       if (c_count.gt.0) then
         call get_command_argument(1,Which_code)  !User selects which code
         !Some spellings for cgem 
         if(Which_code.eq."CGEM".or.Which_code.eq."cgem") then
           Which_code = "CGEM"
         endif
         !Some spellings for gomdom
         if(Which_code.eq."GOMDOM".or.Which_code.eq."GoMDOM".or.Which_code.eq."gomdom") then
           Which_code = "GOMDOM"
           input_filename = "GOMDOM_InputFile"
           ! init_filename = "InitialConditions_GD.txt"
           init_filename = "Initial_Conditions.nc"
           BASE_NETCDF_OUTPUT_FILE_NAME = './NETCDF/gomdom.'
         endif
       endif

#ifdef CAL_LT
         if(Which_code.eq."CGEM") then
          init_filename = "InitialConditions.lt.txt"
         else
          init_filename = "InitialConditions_GD.lt.txt"
         endif

#endif
#ifdef CAL_DK
         if(Which_code.eq."CGEM") then
          init_filename = "InitialConditions.lt.txt"
         else
          init_filename = "InitialConditions_GD.lt.txt"
         endif
#endif

#ifdef CAL_LTNT
         if(Which_code.eq."CGEM") then
          init_filename = "InitialConditions.lt.txt"
         else
          init_filename = "InitialConditions_GD.ltnt.txt"
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

       write(6,*) "Biogeochem equations are: ",Which_code
       write(6,*) "Inputfile will be: ",trim(input_filename)
       write(6,*) "Initial Conditions filename will be: ",trim(init_filename)
       write(6,*) "Base Outputfile Name will be: ",trim(BASE_NETCDF_OUTPUT_FILE_NAME)

       return

       END Subroutine Command_Line_Args
