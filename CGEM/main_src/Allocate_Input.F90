Subroutine Allocate_Input(Which_code)

USE INPUT_VARS

IMPLICIT NONE

character(6), intent(in) :: Which_code


if(Which_code.eq."CGEM") then !CGEM

         call Allocate_Input_CGEM()

else if(Which_code.eq."GOMDOM") then !GOMDOM

         call Allocate_Input_GD()

else

  write(6,*) "Model ",Which_code," not found, Exiting."
  stop

endif

         call INPUT_VARS_allocate()


return
END SUBROUTINE Allocate_Input

