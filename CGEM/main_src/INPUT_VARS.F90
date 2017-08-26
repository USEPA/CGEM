Module INPUT_VARS

IMPLICIT NONE

SAVE

!Calculated:
integer(kind=8) :: START_SECONDS ! Seconds from iYr0 to start of run.
integer(kind=8) :: END_SECONDS   ! Seconds from iYr0 to end of run.
integer :: nstep  !number of timesteps in a run
integer :: iout   !output time-interval in timesteps

!--Code Run Identifier-----------
character(150) code_ID
!--Run Specifics---------------
integer iYrS,iMonS,iDayS,iHrS,iMinS,iSecS
integer iYrE,iMonE,iDayE,iHrE,iMinE,iSecE
integer dT, dT_out, dT_restart
integer icent,jcent

!--Switches ---------
integer Read_Solar,Read_Wind,Read_T,Read_Sal 
integer InitializeHow
integer Which_hydro
integer Which_Output

!----Sinking Terms----------------------------------
real,allocatable :: ws(:)

contains

Subroutine INPUT_VARS_allocate

USE Model_dim, ONLY : nf
USE Fill_Value

IMPLICIT NONE

integer ierr

!----Sinking Terms----------------------------------
ALLOCATE( ws(nf),stat=ierr ) 
if(ierr.ne.0) write(6,*) "error allocate"
return

ws=fill(0)  !Fill values for netCDF

END Subroutine INPUT_VARS_allocate

END MODULE INPUT_VARS

