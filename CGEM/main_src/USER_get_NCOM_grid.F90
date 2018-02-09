      subroutine USER_get_NCOM_grid()

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE INPUT_VARS , ONLY: icent,jcent

      IMPLICIT NONE
 
      real, dimension(jm) :: dxy
      integer :: i,j,k
      character(200) filename

      integer :: nzr, ns

      write(filename,'(A, A)') trim(DATADIR),'/dxdy.dat'
      open(19,file=filename,status='old')
      read(19,*) 
      read(19,*) dxy
      close(19)

      do j=1,jm
       do i=1,im
          area(i,j) = dxy(j)*dxy(j)
       enddo
      enddo

      ! NCOM grid depths do not change with time
      write(filename,'(A, A)') trim(DATADIR),'/Depth.dat'
      open(19,file=filename,status='old')
      read(19,*) nzr,ns,Hs   !Hs is a reference depth used to calculate sigma values given depths of layer surfaces and centers
      read(19,*) zl
      read(19,*) zz
      read(19,*) dz_k
      close(19)

      !---------------------------------------------------------------------
      !     ns : no. of sigma levels at center of sigma layers, i.e. the
      !     number
      !     of sigma layers- convert dz,zl,zz to sigma by dividing thru by the
      !     reference water thickness, Hs
      !---------------------------------------------------------------------
      do k = 1, nsl
         dz_k(k) = dz_k(k)/Hs   ! dz(k) on RHS is thickness (m) of layer k
                                    ! On LHS, dz(k) is sigma thickness of layer k
      enddo
      !------------------------------------------------------------- 
      ! In the next loop, we need zl(k=nsl+1) to calculate elevation
      ! (i.e. depth) at the bottom of sigma layer k = nsl, i.e. the
      ! top of layer k = nsl+1.
      !
      ! In the next loop We also need zz(k=nsl+1) to calculate dzz(nsl)
      ! in the following loop that calculates dzz array elements
      !-------------------------------------------------------------
      do k = 1, nsl+1
         zl(k) = zl(k)/Hs   ! zl(k) on RHS is depth (m) at at top
                            !  of layer k with zl(ns+1)=depth at sea
                            !  bottom. On LHS, zl(k) is the sigma
                            !  value associated with the top interface
                            !  of layer k
      
         zz(k) = zz(k)/Hs   ! zz(k) on RHS is depth (m) at middle of
                            !  layer k. on LHS, zz(k) is the sigma
                            !  value associated with the middle of
                            !  layer k.
      enddo
    
      do k = 1, nsl               ! zz(k)   = sigma at middle of layer k
         dzz(k) = zz(k+1)-zz(k)   ! zz(k+1) = sigma at middle of layer k+1
                                  ! dzz(k)  = sigma difference between
                                  !   middle of layer k+1 and middle of
                                  !   layer k
      enddo


      write(filename,'(A, A)') trim(DATADIR),'/TopoS.dat'
      open(19,file=filename,status='old')
      read(19,*) 
      read(19,*) h   ! This is the undisturbed water depth at the center of cells
      close(19)

      do j=1,jm
       do i=1,im
          h(i,j) = amin1(h(i,j), Hs)   !Depth for sigma layers, maximum depth 100
       enddo
      enddo

#ifdef map_code
     write(6,*) "----Calling USER_get_NCOM_grid-----"
     write(6,*) "  setting dxy, area, zl, zz, dz_k, and h"
     write(6,*) "  Not setting depths, because don't have E"
     write(6,*)
#endif

      return
      end subroutine USER_get_NCOM_grid


      subroutine USER_update_NCOM_grid()

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE Hydro, ONLY: E
      USE INPUT_VARS

      IMPLICIT NONE

      integer :: i,j,k,nz
      integer :: init=1

       do j=1,jm
        do i=1,im
          depth(i,j) = h(i,j)  + E(i,j)
        enddo
       enddo

 
       do j=1,jm
        do i=1,im
           nz = nza(i,j)
           do k=1,nz
            dz(i,j,k) = depth(i,j)*dz_k(k)
            d_sfc(i,j,k) = depth(i,j)*zz(k)
            Vol(i,j,k) = area(i,j) *dz(i,j,k)
            d(i,j,k) = sum(dz(i,j,1:k)) 
        enddo
       enddo
      enddo  

#ifdef DEBUG
     write(6,*) "----Calling USER_get_NCOM_grid-----"
     write(6,*) "  setting dxy, area, zl, zz, dz_k, and h"
     write(6,*) "  For cell i,j,k=",icent,jcent,km
     write(6,*) "h,E,d,zz,area,d_sfc,Vol="
     write(6,*) h(icent,jcent),E(icent,jcent),d(icent,jcent,km)
     write(6,*) zz(km),area(icent,jcent),d_sfc(icent,jcent,km),Vol(icent,jcent,km)
#endif

      end subroutine USER_update_NCOM_grid

