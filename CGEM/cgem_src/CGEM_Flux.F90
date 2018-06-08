Module CGEM_Flux
! =========================================================
! Terms for Flux Calculations
! =========================================================
       IMPLICIT NONE

       REAL,ALLOCATABLE,SAVE :: Esed(:,:) 
       REAL,ALLOCATABLE,SAVE :: CBODW(:,:) 
       REAL,ALLOCATABLE,SAVE :: pH(:,:,:)

Contains

 Subroutine Allocate_CGEM_Flux()

  USE Model_dim
  
  Allocate(Esed(myim,jm))
  Allocate(CBODW(myim,jm))
  Allocate(pH(myim,jm,km))

  return

  End Subroutine Allocate_CGEM_Flux

END Module CGEM_Flux 
