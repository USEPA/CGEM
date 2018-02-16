Module SDM 

IMPLICIT NONE

SAVE

INTEGER NPOINTS   !This is read in from A 
INTEGER NEQ       !Calculated from A
real, allocatable :: sedflux(:,:,:) 
double precision, dimension(100) :: A
double precision, allocatable :: YY_init(:,:,:)
double precision, allocatable :: pph_init(:,:,:)
integer, parameter :: sO2 = 1
integer, parameter :: sNO3 = 2
integer, parameter :: sNH4 = 3
integer, parameter :: sDIC = 4
integer, parameter :: sOM1 = 5
integer, parameter :: sOM2 = 6
integer, parameter :: sALK = 7
integer, parameter :: sDOC = 8
integer,parameter :: nsed = 8
integer, parameter :: mrow=64


contains

Subroutine SDM_allocate

!YY is giant, so only allocated SDM arrays if SDM is used

USE Model_dim

IMPLICIT NONE


double precision, allocatable :: dummy1(:), dummy2(:)
integer i
 
allocate(dummy1(NPOINTS))
allocate(dummy2(NEQ))

!Initial values for pH and Y
OPEN(11,STATUS='UNKNOWN', file='SDM/ph2bprofile.dat')
READ(11,*) (dummy1(i),i=1,NPOINTS)
CLOSE(11)

OPEN(11,STATUS='UNKNOWN', file='SDM/normoxia.dat')
READ(11,*) (dummy2(i),i=1,NEQ)
CLOSE(11)

allocate (sedflux(myim,jm,nsed))
allocate (ppH_init(myim,jm,NPOINTS))
allocate (YY_init(myim,jm,NEQ))

!Initialize whole grid to the initial values
do i=1,NPOINTS
   ppH_init(:,:,i) = dummy1(i)
enddo

do i=1,NEQ
   YY_init(:,:,i) = dummy2(i)
enddo

 
END Subroutine SDM_allocate

END MODULE SDM
