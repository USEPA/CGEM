Module Model_dim

! Reference year all timestamps are relative to.

      INTEGER, PARAMETER :: iYr0 = 2002

! =========================================================
! Dimensions for Lisa's Fish Tank - USER modified part
! =========================================================
      INTEGER :: IM, JM 
      INTEGER :: NZ_MAX
      INTEGER :: NSL                   !Bottom cell
      INTEGER,ALLOCATABLE :: NZA(:,:) 
      INTEGER :: nospA  !Read in by main.F90 in data/Model_dim.txt
      INTEGER :: nospZ 
      INTEGER :: nf 
      INTEGER :: EXTRA_VARIABLES
      INTEGER :: Which_gridio
      INTEGER :: b_layer !add a boundary layer?

END Module Model_dim
