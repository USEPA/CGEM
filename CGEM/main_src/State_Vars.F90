      MODULE State_Vars 

       IMPLICIT NONE

      real,allocatable,save :: f(:,:,:,:) !state variable array

      contains

      Subroutine Set_Vars(Which_code,init_filename)

      IMPLICIT NONE

      character(6), intent(in) :: Which_code 
      character(120), intent(in) :: init_filename

      call Vars_allocate()

      call Set_Initial_Conditions(Which_code,init_filename) 

      return

      End Subroutine Set_Vars

      Subroutine Vars_allocate()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(f(im,jm,km,nf))
      f=fill(1)

      return

      End Subroutine Vars_allocate

      Subroutine Set_Initial_Conditions(Which_code,init_filename)

      IMPLICIT NONE

      character(6), intent(in) :: Which_code
      character(120), intent(in) :: init_filename

      if(Which_code.eq."CGEM") then !CGEM

       call Set_Initial_Conditions_CGEM(init_filename)

      else if(Which_code.eq."GOMDOM") then !GOMDOM

       call Set_Initial_Conditions_GD(init_filename)

      else

       write(6,*) "Model ",Which_code," not found, Exiting."
       stop
    
      endif

      return

      End Subroutine Set_Initial_Conditions 


      End Module State_Vars 
