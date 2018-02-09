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
!For parallel runs
      INTEGER, SAVE :: my_im !Number of cells in i direction on the processor
      INTEGER, SAVE :: my_imstart !location of first array index WRT full grid
      INTEGER, SAVE :: my_imend !location of last array index WRT full grid

#ifdef _MPI
include 'mpif.h'
#endif

CONTAINS

Subroutine Set_Model_dim(myid,numprocs)


IMPLICIT NONE

      integer, intent(in) :: myid,numprocs
      character(200) filename
      integer mpierr

if(myid.eq.0) then

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
      read(19,*) km 
      read(19,*) nospA
      read(19,*) nospZ
      read(19,*) Which_gridio
      read(19,*) iYr0
      close(19)
      
      if(Which_gridio.eq.2 .or. Which_gridio.eq.3) then
         nsl = km + 1 
      else
         nsl = km
      endif

endif

if(numprocs.gt.1) then
      call MPI_BCAST(im,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr) 
      call MPI_BCAST(jm,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
      call MPI_BCAST(km,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
      call MPI_BCAST(nospA,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
      call MPI_BCAST(nospZ,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
      call MPI_BCAST(Which_gridio,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
      call MPI_BCAST(iYr0,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
      call MPI_BCAST(DATADIR,200,MPI_CHARACTER,0,MPI_COMM_WORLD,mpierr)
      call MPI_BCAST(nsl,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
endif

!Divide grid among processors
      call Decomp1D(im, numprocs, myid, my_imstart, my_imend)

!Define Loop Bounds
      my_im = my_imend - my_imstart + 1

      write(6,*) "im,my_im",im,my_im
      write(6,*) "my_imstart,end",my_imstart,my_imend

      call Model_dim_allocate(im)

#ifdef map_code
write(6,*) "---Set_Model_dim----"
write(6,*)
#endif

#ifdef DEBUG
write(6,*) "Model_dim.txt=",filename
write(6,*) "im,jm,km,nsl",im,jm,km,nsl
write(6,*)
#endif

Return

End Subroutine


Subroutine Model_dim_allocate(dim_i)

USE Fill_Value

IMPLICIT NONE

integer, intent(in) :: dim_i 

ALLOCATE(nza(dim_i,jm))

!Initialize for netCDF
nza=fill(0)

#ifdef map_code
write(6,*) "---Model_dim_allocate---"
write(6,*) "allocating nza, fill with",fill(0)
write(6,*)
#endif

Return

END Subroutine Model_dim_allocate


END Module Model_dim
