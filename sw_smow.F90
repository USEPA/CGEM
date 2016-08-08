!---------------------------------------------------------------------
      FUNCTION sw_smow( T ) RESULT( RES )  

  !------------------------------------------------------------------
  ! Denisty of Standard Mean Ocean Water (Pure Water) using EOS 1980. 
  ! Based on MATLAB code and the following references:
  ! SW_SMOW  $Revision: 1.3 $  $Date: 1994/10/10 05:51:46 $
  ! Copyright (C) CSIRO, Phil Morgan 1992.
  !
  ! INPUT: 
  !   T = temperature [degree C (IPTS-68)]
  !
  ! OUTPUT:
  !   dens = density  [kg/m^3] 
  ! 
  ! AUTHOR:  Phil Morgan 92-11-05  (morgan@ml.csiro.au)
  !
  ! DISCLAIMER:
  !   This software is provided "as is" without warranty of any kind.  
  !   See the file sw_copy.m for conditions of use and licence.
  !
  ! REFERENCES:
  !     Unesco 1983. Algorithms for computation of fundamental properties of 
  !     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
  !     UNESCO 1983 p17  Eqn(14)
  !
  !     Millero, F.J & Poisson, A.
  !     INternational one-atmosphere equation of state for seawater.
  !     Deep-Sea Research Vol28A No.6. 1981 625-629.    Eqn (6)
  !-------------------------------------------------------------------------
    IMPLICIT NONE

    REAL,INTENT(IN) :: T    ! Temperature (deg C)
    REAL            :: RES
    
    ! Locals:
    REAL, PARAMETER :: A0 = 999.842594
    REAL, PARAMETER :: A1 =   6.793952E-2
    REAL, PARAMETER :: A2 =  -9.095290E-3
    REAL, PARAMETER :: A3 =   1.001685E-4
    REAL, PARAMETER :: A4 =  -1.120083E-6
    REAL, PARAMETER :: A5 =   6.536332E-9

    RES = A0 + ( A1 + ( A2 + ( A3 + ( A4 + A5 * T ) * T ) * T ) * T ) * T

    RETURN
  END FUNCTION sw_smow  
