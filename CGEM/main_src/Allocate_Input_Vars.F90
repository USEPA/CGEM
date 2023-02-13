	Subroutine Allocate_Input_Vars(Which_code)

USE Input_Vars

IMPLICIT NONE

character(6), intent(in) :: Which_code

if(Which_code.eq."CGEM") then !CGEM

         call Allocate_Input_CGEM()

else if(Which_code.eq."WQEM") then !WQEM

         call Allocate_Input_WQEM()

else

  write(6,*) "Model ",Which_code," not found, Exiting."
  stop

endif

call Allocate_Input()

return
END SUBROUTINE Allocate_INPUT_VARS
