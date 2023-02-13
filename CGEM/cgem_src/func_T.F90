!---------------------------------------------------------------------------
  SUBROUTINE func_T( T, Tadj )   
!---------------------------------------------------------------------------

  USE Model_dim
  USE TEMP_VARS     
      
  !--------------------------------------------------------------------------
  ! INPUT:  
  !   T = temperature [degree C]
  !
  ! OUTPUT:
  !   Temperature Adjustment 
  ! 
  ! REFERENCES:
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN) :: T    ! Temperature (deg C)
    REAL, INTENT(OUT), DIMENSION(nospA+nospZ) :: Tadj 
    REAL, PARAMETER  :: f0    = 0.1
    REAL, PARAMETER  :: r     = 0.3
    REAL, PARAMETER  :: f1    = 1.0/f0 - 1.0  
    REAL, PARAMETER  :: r1    = r*(46.5/18.0) 
    REAL, PARAMETER  :: k_b    = 8.6173303e-5 !Boltzmann constant in eV/K
    REAL             :: denom(nospA+nospZ)
    REAL             :: T_in_K,Tref_in_K(nospA+nospZ)
    INTEGER          :: i


    if (Which_temperature.eq.1) then !Sigmoidal 
      denom(:) = 1.0 + f1*exp(-r1*( T - Tref(:))) 
      Tadj(:)  = 0.3 *(1.0/denom(:)) + 0.7            
    else if (Which_temperature.eq.2) then !Optimum temperature threshold T (Cerco and Noel, 2004)
         do i=1,nospA+nospZ
           if(T.le.Tref(i)) then
             Tadj(i) = exp( -KTg1(i) * (T - Tref(i))**2 )           
           else
             Tadj(i) = exp( -KTg2(i) * (Tref(i) - T)**2 )
           endif
         enddo
    else if (Which_temperature.eq.3) then !Decrease in growth rate at threshold T (Arrhenius form, Geider 1997)
      T_in_K  = T + 273.15 !Temp. in Kelvin
      Tref_in_K(:) = Tref(:) + 273.15 !Temp. in Kelvin 
      Tadj(:) = exp ( -(Ea(:)/k_b) * ( 1./T_in_K - 1./Tref_in_K(:) ) ) 
    else if (Which_temperature.eq.4) then !WQEM temperature functions
      call T_WQEM(T, Tadj)
    else  
      write(6,*) "Error in func_T"
      stop
    endif
 
    RETURN
  END SUBROUTINE func_T  
