Subroutine Initialize_State_Vars(Which_code)

IMPLICIT NONE

character(6), intent(in) :: Which_code

if(Which_code.eq."CGEM") then !CGEM

     call Set_Initial_Conditions_CGEM()

else if(Which_code.eq."GOMDOM") then !GOMDOM

     call Set_Initial_Conditions_GD() 

else

  write(6,*) "Model ",Which_code," not found, Exiting."
  stop

endif


return
END SUBROUTINE Initialize_State_Vars
