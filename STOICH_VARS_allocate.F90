Subroutine STOICH_VARS_allocate

USE Model_dim
USE STOICH_VARS

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

END Subroutine STOICH_VARS_allocate
