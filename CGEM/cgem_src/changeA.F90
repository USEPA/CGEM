      Subroutine changeA(A,f,T,S,pH,s_x1A,s_y1A,s_z1A,s_x2A,s_y2A,s_z2A,&
     & s_x1Z,s_y1Z,s_z1Z,s_x2Z,s_y2Z,s_z2Z )

      USE Model_dim
      USE CGEM_vars
      USE INPUT_VARS
      USE INPUT_VARS_CGEM

      IMPLICIT NONE

      real*8, dimension(100), intent(INOUT) :: A
      real, intent(IN) :: f(nf),T,S,pH ! State variables, Temp, Salinity
      real :: R_11
      real :: s_x1A,s_y1A,s_z1A,s_x2A,s_y2A,s_z2A,s_x1Z,s_y1Z
      real :: s_z1Z,s_x2Z,s_y2Z,s_z2Z


! Calculate nitrification, time units are in years, R_11:
      call Nitrification(f(iO2),f(iNH4),KO2,KNH4,nitmax,T,R_11)

! Change A values according to state variables, T, and S
      A(1) = T   ! Temperature in C
      A(2) = S   ! Salinity 
      A(4) = pH  ! pH

! Weighted average of KG1 for OM1_A and OM1_Z (KG1 is the same)
      A(8) = KG1
! Weighted average of KGX for OM1_R and OM1_
      if((f(iOM1_R) + f(iOM1_BC)).gt.0.) then
      A(9) = (KG1_R * f(iOM1_R) + KG1_BC * f(iOM1_BC)) / &
     &                         (f(iOM1_R) + f(iOM1_BC))
      else
      A(9) = KG1 
      endif

!DOM: Weighted Averages of K_OM for all OM2
      if((f(iOM2_A) + f(iOM2_Z) + f(iOM2_R) + f(iOM2_BC)).gt.0.) then
      A(10) = (KG2*(f(iOM2_A)+f(iOM2_Z)) + KG2_R*f(iOM2_R) + KG2_BC*f(iOM2_BC)) / & 
     &             (f(iOM2_A) + f(iOM2_Z) + f(iOM2_R) + f(iOM2_BC))
      else
      A(10) = KG2 
      endif

      A(16) = R_11 

! OM1=OM1_A + OM1_Z
      if((f(iOM1_A)+f(iOM1_Z)).gt.0.) then
      A(23) = (s_x1A/s_z1A*f(iOM1_A) + s_x1Z/s_z1Z*f(iOM1_Z))/(f(iOM1_A)+f(iOM1_Z)) 
      A(24) = (s_y1A/s_z1A*f(iOM1_A) + s_y1Z/s_z1Z*f(iOM1_Z))/(f(iOM1_A)+f(iOM1_Z)) 
      else
      A(23) = 105.
      A(24) = 25.
      endif

      A(25) = 1. 

! OM2=OM1_R + OM1_BC
      if((f(iOM1_R)+f(iOM1_BC)).gt.0.) then
      A(26) = (stoich_x1R/stoich_z1R*f(iOM1_R) + stoich_x1BC/stoich_z1BC*f(iOM1_BC))/(f(iOM1_R)+f(iOM1_BC))
      A(27) = (stoich_y1R/stoich_z1R*f(iOM1_R) + stoich_y1BC/stoich_z1BC*f(iOM1_BC))/(f(iOM1_R)+f(iOM1_BC))
      else
      A(26) = 105.
      A(27) = 25.
      endif

      A(28) = 1. 

! DOM = Sum of OM
      if((f(iOM1_A)+f(iOM1_Z)+f(iOM1_R)+f(iOM1_BC)).gt.0.) then
      A(29) = (s_x2A/s_z2A*f(iOM2_A) + s_x2Z/s_z2Z*f(iOM2_Z) +     &
     &       stoich_x2R/stoich_z2R*f(iOM2_R) + stoich_x2BC/stoich_z2BC*f(iOM2_BC)) / &
     &        (f(iOM2_A) + f(iOM2_Z) + f(iOM2_R) + f(iOM2_BC))
      A(30) = (s_y2A/s_z2A*f(iOM2_A) + s_y2Z/s_z2Z*f(iOM2_Z) +     &
     &       stoich_y2R/stoich_z2R*f(iOM2_R) + stoich_y2BC/stoich_z2BC*f(iOM2_BC)) / &
     &        (f(iOM2_A) + f(iOM2_Z) + f(iOM2_R) + f(iOM2_BC))
      else
      A(29) = 105.
      A(30) = 10.
      endif

      A(31) = 1. 

      A(32) = kO2
      A(33) = kNO3
      A(47) = f(iO2)
      A(48) = f(iNO3)
      A(49) = f(iNH4) 
      A(57) = f(iOM2_A) + f(iOM2_Z) + f(iOM2_R) + f(iOM2_BC)
      A(58) = f(iDIC)
      A(59) = A(57)
      A(60) = A(49) 
      A(62) = (f(iOM1_A)*ws(iOM1_A)+f(iOM1_Z)*ws(iOM1_Z))*12.
      A(63) = (f(iOM1_R)*ws(iOM1_R)+f(iOM1_BC)*ws(iOM1_BC))*12.


      RETURN

      END Subroutine changeA 
