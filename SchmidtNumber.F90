!---------------------------------------------------------------------------
      FUNCTION SchmidtNumber( S, T, which ) RESULT( RES )  
      
  !-------------------------------------------------------------------------
  ! INPUT:  (all must have same dimensions)
  !   S = salinity    [psu      (PSS-78)]
  !   T = temperature [degree C (IPTS-68)]
  !
  ! OUTPUT:
  !   Schmidt Number for Oxygen (which = 0)
  !or Schmidt Number for CO2    (which = 1) 
  ! 
  ! REFERENCES:
  !     Unesco 1983. Algorithms for computation of fundamental properties of 
  !     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN) :: S    ! Salinity (psu)
    REAL, INTENT(IN) :: T    ! Temperature (deg C)
    REAL :: RES
    REAL :: sw_dens0
    REAL :: muSW,KT
    REAL :: D_02, D_C02
    INTEGER, INTENT(IN) :: which !0 for Oxygen
                                 !1 for CO2

    REAL, PARAMETER:: K0 = 1.7910 
    REAL, PARAMETER:: K1 = -6.144e-2 
    REAL, PARAMETER:: K2 = 1.4510e-3 
    REAL, PARAMETER:: K3 = -1.6826e-5 
    REAL, PARAMETER:: K4 = -1.5290e-4
    REAL, PARAMETER:: K5 = 8.3885e-8
    REAL, PARAMETER:: K6 = 2.4727e-3
    REAL, PARAMETER:: K7 = 6.0574e-6
    REAL, PARAMETER:: K8 = -2.6760e-9
    REAL, PARAMETER:: K9 = 4.8429e-5
    REAL, PARAMETER:: K10 = -4.7172e-6
    REAL, PARAMETER:: K11 = 7.5986e-8

    REAL, PARAMETER:: D1 = 0.2604e-5 
    REAL, PARAMETER:: D2 = 0.006383e-5 
    REAL, PARAMETER:: D3 = 0.1954e-5
    REAL, PARAMETER:: D4 = 0.005089e-5 

    REAL, PARAMETER:: P = 1.01325

    KT = 273.15 + T

    muSW = K0 + K1*T + K2*T*T + K3*T*T*T + K4*P + K5*P*P + K6*S + T*(K7*P + K8*P*P) + S*(K9*T + K10*T*T + K11*T*T*T) 

    if(which.eq.0) then
     D_02 = D1 + D2*KT/muSW
     RES = muSW/(sw_dens0(S,T)*D_02)
    endif

    if(which.eq.1) then
     D_C02 = D3 + D4*KT/muSW
     RES = muSW/(sw_dens0(S,T)*D_C02)
    endif

               
    RETURN
  END FUNCTION SchmidtNumber  
