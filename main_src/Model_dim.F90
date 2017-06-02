Module Model_dim

IMPLICIT NONE


! Reference year all timestamps are relative to.

      INTEGER, PARAMETER :: iYr0 = 1970

! =========================================================
! Dimensions for Lisa's Fish Tank - USER modified part
! =========================================================
      INTEGER, SAVE :: IM, JM 
      INTEGER, SAVE :: NZ_MAX
      INTEGER, SAVE :: NSL                   !Bottom cell
      INTEGER,ALLOCATABLE, SAVE :: NZA(:,:) 
      INTEGER, SAVE :: nospA  !Read in by main.F90 in data/Model_dim.txt
      INTEGER, SAVE :: nospZ 
      INTEGER, SAVE :: nf 
      INTEGER, SAVE :: EXTRA_VARIABLES
      INTEGER, SAVE :: Which_gridio
      INTEGER, SAVE :: b_layer !add a boundary layer?
      CHARACTER(200), SAVE :: DATADIR

CONTAINS

Subroutine Set_Model_dim()

IMPLICIT NONE

      character(200) filename

! --- Read in location of data:
      open(unit=19,file="./data/MyFiles.inp", form="formatted", status="old")
      read(19,'(a)') DATADIR
      close(19)
!"
      write(filename,'(A, A)') trim(DATADIR),'/Model_dim.txt'

! --- Read in Model_dim parameters
      open(unit=19,file=filename, form="formatted", status="old")
      read(19,*) !Header
      read(19,*) im
      read(19,*) jm
      read(19,*) nz_max
      read(19,*) b_layer
      read(19,*) nospA
      read(19,*) nospZ
      read(19,*) Which_gridio
      close(19)
      nsl = nz_max + b_layer

      call Model_dim_allocate()

Return

End Subroutine


Subroutine Model_dim_allocate

USE Fill_Value

IMPLICIT NONE

ALLOCATE(nza(im,jm))
!Initialize for netCDF
nza=fill(0)

Return

END Subroutine Model_dim_allocate



END Module Model_dim
