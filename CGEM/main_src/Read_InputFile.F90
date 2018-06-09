Subroutine Read_InputFile(input_filename,Which_code,myid,numprocs)


use mpi_interface

IMPLICIT NONE

integer, intent(in) :: myid, numprocs
character(120), intent(in) :: input_filename     
character(6), intent(in) :: Which_code


if(Which_code.eq."CGEM") then !CGEM

 call Read_InputFile_CGEM(input_filename,myid,numprocs)

#ifdef DEBUG
!This will check if Read_InputFile is correctly reading
       if(myid.eq.0) call Write_InputFile_CGEM()
       write(6,*) "DEBUG: Wrote Debug_InputFile"
#endif

else if(Which_code.eq."GOMDOM") then !GOMDOM

 call Read_InputFile_GD(input_filename,myid,numprocs)

#ifdef DEBUG
!This will check if Read_InputFile is correctly reading
       if(myid.eq.0) call Write_InputFile_GD()
       write(6,*) "DEBUG: Wrote Debug_InputFile"
#endif


else

  write(6,*) "Model ",Which_code," not found, Exiting."
  stop

endif


return
END SUBROUTINE Read_InputFile
