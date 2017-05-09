Module STOICH_VARS

IMPLICIT NONE

SAVE

real, dimension(:,:,:), allocatable :: s_x1A,s_x2A,s_y1A,s_y2A
real, dimension(:,:,:), allocatable :: s_z1A,s_z2A
real, dimension(:,:,:), allocatable :: s_x1Z,s_x2Z,s_y1Z,s_y2Z
real, dimension(:,:,:), allocatable :: s_z1Z,s_z2Z
real Stoich_x1A_init,Stoich_y1A_init,Stoich_z1A_init
real Stoich_x2A_init,Stoich_y2A_init,Stoich_z2A_init
real Stoich_x1Z_init,Stoich_y1Z_init,Stoich_z1Z_init
real Stoich_x2Z_init,Stoich_y2Z_init,Stoich_z2Z_init

contains

Subroutine STOICH_VARS_allocate

USE Fill_Value
USE Model_dim

IMPLICIT NONE

real fill_val
integer i,j,k

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

!initialize for netCDF, 1==-9999
fill_val = fill(1)
s_x1A=fill_val
s_x2A=fill_val
s_y1A=fill_val
s_y2A=fill_val
s_z1A=fill_val
s_z2A=fill_val
s_x1A=fill_val
s_x2A=fill_val
s_y1A=fill_val
s_y2A=fill_val
s_z1A=fill_val
s_z2A=fill_val

!Initial Stoichiometry of POC and DOC: 
  do j = 1,jm
    do i = 1,im
      do k=1,nza(i,j)
       s_x1A(i,j,k)= Stoich_x1A_init
       s_x2A(i,j,k)= Stoich_x2A_init
       s_y1A(i,j,k)= Stoich_y1A_init
       s_y2A(i,j,k)= Stoich_y2A_init
       s_z1A(i,j,k)= Stoich_z1A_init
       s_z2A(i,j,k)= Stoich_z2A_init
       s_x1Z(i,j,k)= Stoich_x1Z_init
       s_x2Z(i,j,k)= Stoich_x2Z_init
       s_y1Z(i,j,k)= Stoich_y1Z_init
       s_y2Z(i,j,k)= Stoich_y2Z_init
       s_z1Z(i,j,k)= Stoich_z1Z_init
       s_z2Z(i,j,k)= Stoich_z2Z_init
    enddo
   enddo
  enddo

return

END Subroutine STOICH_VARS_allocate

END MODULE STOICH_VARS
