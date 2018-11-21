Module Model_dim

IMPLICIT NONE


! =========================================================
! Dimensions for Lisa's Fish Tank - USER modified part
! =========================================================
      INTEGER, SAVE :: IM, JM, KM
      INTEGER, SAVE :: NSL                   !Extra layer for cell faces
      INTEGER,ALLOCATABLE, SAVE :: NZA(:,:) 
      INTEGER, SAVE :: nospA  !Read in by main.F90 in data/Model_dim.txt
      INTEGER, SAVE :: nospZ 
      INTEGER, SAVE :: nf 
      INTEGER, SAVE :: EXTRA_VARIABLES
      INTEGER, SAVE :: Which_gridio
      CHARACTER(200), SAVE :: DATADIR
      INTEGER, SAVE :: iYr0 ! Reference year all timestamps are relative to.
      INTEGER, SAVE :: nRiv
      INTEGER, SAVE :: nBC

CONTAINS

Subroutine Set_Model_dim()

IMPLICIT NONE

      character(200) filename

#ifdef map_code
write(6,*) "---Set_Model_dim----"
write(6,*)
#endif

! --- Read in location of data:
      open(unit=19,file="./data/MyFiles.inp", form="formatted", status="old")
      read(19,'(a)') DATADIR
      close(19)
!"
      write(filename,'(A, A)') trim(DATADIR),'/Model_dim.txt'

      PRINT*, "DATADIR = ", DATADIR

#ifdef DEBUG
write(6,*) "Model_dim.txt=",filename
#endif

! --- Read in Model_dim parameters
      open(unit=19,file=filename, form="formatted", status="old")
      read(19,*) !Header
      read(19,*) im
      read(19,*) jm
      read(19,*) km 
      read(19,*) nospA
      read(19,*) nospZ
      read(19,*) Which_gridio
      read(19,*) iYr0
      read(19,*) nRiv
      read(19,*) nBC
      close(19)
      
      PRINT*, "Number of rivers = ", nRiv
      PRINT*, "Number of surficial boundary cells = ", nBC
      PRINT*, "Reference year = ", iYr0

      if(Which_gridio.eq.2 .or. Which_gridio.eq.3) then
         nsl = km + 1 
      else
         nsl = km
      endif

      call Model_dim_allocate()

#ifdef DEBUG
write(6,*) "im,jm,km,nsl",im,jm,km,nsl
write(6,*) 
#endif

Return

End Subroutine


Subroutine Model_dim_allocate

USE Fill_Value

IMPLICIT NONE

ALLOCATE(nza(im,jm))

#ifdef map_code
write(6,*) "---Model_dim_allocate---"
write(6,*) "    allocating nza"
write(6,*)
#endif 

!Initialize for netCDF
nza = fill(0)

Return

END Subroutine Model_dim_allocate


END Module Model_dim
