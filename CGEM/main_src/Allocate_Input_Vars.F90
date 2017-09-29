Subroutine Allocate_Input_Vars(Which_code)

USE Input_Vars
USE Model_dim, ONLY: im,jm

IMPLICIT NONE

character(6), intent(in) :: Which_code

#ifdef map_code
write(6,*) "---Allocate_Input---"
write(6,*) "   first separate for CGEM and GD, then for shared"
write(6,*)
#endif

if(Which_code.eq."CGEM") then !CGEM

         call Allocate_Input_CGEM()

else if(Which_code.eq."GOMDOM") then !GOMDOM

         call Allocate_Input_GD()

else

  write(6,*) "Model ",Which_code," not found, Exiting."
  stop

endif

         call Allocate_Input()
         if(im*jm.eq.1) then
             icent=1
             jcent=1
         endif

#ifdef debug
         if(icent.gt.im)then
           write(6,*) "icent>im"
           stop
         endif
         if(jcent.gt.jm) 
          write(6,*) "jcent>jm"
          stop
         endif
#endif

return
END SUBROUTINE Allocate_INPUT_VARS

