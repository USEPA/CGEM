Subroutine Initialize_Output(Which_code,BASE_NETCDF_OUTPUT_FILE_NAME)

USE Model_dim

IMPLICIT NONE

character(6), intent(in) :: Which_code
character(200), intent(in) :: BASE_NETCDF_OUTPUT_FILE_NAME

if(Which_code.eq."CGEM") then !CGEM

   !Fix up NETCDF output according to InputFile:
      call Init_Output_CGEM(BASE_NETCDF_OUTPUT_FILE_NAME)
#ifdef DEBUG
write(6,*) "After Output_CGEM"
#endif


else if(Which_code.eq."GOMDOM") then !GOMDOM
     call Init_Output_GD(BASE_NETCDF_OUTPUT_FILE_NAME)
#ifdef DEBUG
write(6,*) "After Output_GD"
#endif

else

  write(6,*) "Model ",Which_code," not found, Exiting."
  stop

endif


return
END SUBROUTINE Initialize_Output

