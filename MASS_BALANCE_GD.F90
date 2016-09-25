Module MASS_BALANCE_GD 

IMPLICIT NONE

REAL,ALLOCATABLE :: SUM_DENITR(:,:,:)    ! N loss to denitrification 
REAL,ALLOCATABLE :: SUM_DENITR_C(:,:,:)  ! C loss to denitrification 
REAL,ALLOCATABLE :: SUM_DOCPRD(:,:,:)    ! C loss to predation 
REAL,ALLOCATABLE :: SUM_DOCMET(:,:,:)    ! C loss to metabolism 
REAL,ALLOCATABLE :: SUM_DOCZOO(:,:,:)    ! C loss to zooplankton mortality 

END Module MASS_BALANCE_GD
