SUBROUTINE InRemin_allocate 
! =========================================================
! Fluxes for Instant Remineralization 
! =========================================================
       USE Model_dim
       USE InRemin

       IMPLICIT NONE

       ALLOCATE(SED_CARBON_RATE(im,jm)) 
       ALLOCATE(TSOD(im,jm)) 
       ALLOCATE(SED_NO3_RATE(im,jm)) 
       ALLOCATE(SED_NH3_RATE(im,jm)) 

       SED_CARBON_RATE=0.
       TSOD=0.
       SED_NO3_RATE=0.
       SED_NH3_RATE=0.

END SUBROUTINE InRemin_allocate 
