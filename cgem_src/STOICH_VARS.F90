Module STOICH_VARS

IMPLICIT NONE

SAVE

real, dimension(:,:,:), allocatable :: s_x1A,s_x2A,s_y1A,s_y2A
real, dimension(:,:,:), allocatable :: s_z1A,s_z2A
real, dimension(:,:,:), allocatable :: s_x1Z,s_x2Z,s_y1Z,s_y2Z
real, dimension(:,:,:), allocatable :: s_z1Z,s_z2Z

contains

Subroutine STOICH_VARS_allocate

USE Fill_Value
USE Model_dim

IMPLICIT NONE

ALLOCATE(s_x1A(im,jm,nsl))
ALLOCATE(s_x2A(im,jm,nsl))
ALLOCATE(s_y1A(im,jm,nsl))
ALLOCATE(s_y2A(im,jm,nsl))
ALLOCATE(s_z1A(im,jm,nsl))
ALLOCATE(s_z2A(im,jm,nsl))
ALLOCATE(s_x1Z(im,jm,nsl))
ALLOCATE(s_x2Z(im,jm,nsl))
ALLOCATE(s_y1Z(im,jm,nsl))
ALLOCATE(s_y2Z(im,jm,nsl))
ALLOCATE(s_z1Z(im,jm,nsl))
ALLOCATE(s_z2Z(im,jm,nsl))

!initialize for netCDF
s_x1A=fill(0)
s_x2A=fill(0)
s_y1A=fill(0)
s_y2A=fill(0)
s_z1A=fill(0)
s_z2A=fill(0)
s_x1A=fill(0)
s_x2A=fill(0)
s_y1A=fill(0)
s_y2A=fill(0)
s_z1A=fill(0)
s_z2A=fill(0)

return

END Subroutine STOICH_VARS_allocate

END MODULE STOICH_VARS
