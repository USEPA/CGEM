      subroutine USER_get_NCOM_grid(TC_8)

      USE Fill_Value
      USE Model_dim
      USE Grid
    

      IMPLICIT NONE
 
      integer(kind=8) :: TC_8

!      real, intent (out) :: dz(im,jm,nsl)     !cell depth
!      real, intent (out) :: d(im,jm,nsl)      !depth from surface to bottom of cell
!      real, intent (out) :: depth(im,jm)      !total depth of column
!      real, intent (out) :: d_sfc(im,jm,nsl)  !depth from surface to cell center
!      real, intent (out) :: dxdy(im,jm)       !area of cell
!      real, intent (out) :: Vol(im,jm,nsl)    !Volume of cell
      real, dimension(jm) :: dxy
      integer :: i,j,k
      character(200) filename

      real, dimension(35) :: dz_temp
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
      read(19,*) dz_temp
      close(19)

      do i=1,im
      do j=1,jm
      do k=1,nzr
         !L3: was this for debugging?  dz(i,j,k) = 1
         dz(i,j,k) = dz_temp(k)
#ifdef DEBUG
      write(6,*) "dz, Hs, k=",k,Hs,dz_temp(k)
#endif
      enddo
      enddo
      enddo
      !---------------------------------------------------------------------
      !     ns : no. of sigma levels at center of sigma layers, i.e. the
      !     number
      !     of sigma layers- convert dz,zl,zz to sigma by dividing thru by the
      !     reference water thickness, Hs
      !---------------------------------------------------------------------
      do k = 1, nsl
         dz(:,:,k) = dz(:,:,k)/Hs   ! dz(k) on RHS is thickness (m) of layer k
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
#ifdef DEBUG
 write(6,*) "h,Hs",i,j,h(i,j),Hs
#endif
       enddo
      enddo

       do k=1,nsl
        do j=1,jm
         do i=1,im
          d_sfc(i,j,k) = h(i,j)*zz(k)
#ifdef DEBUG
 write(6,*) "d_sfc",i,j,k,d_sfc(i,j,k)
#endif
        enddo
       enddo
      enddo

      return

      end subroutine USER_get_NCOM_grid


      subroutine USER_update_NCOM_grid()

      USE Fill_Value
      USE Model_dim
      USE Grid
      USE Hydro, ONLY: E

      IMPLICIT NONE

      integer :: i,j,k, nz

      d = fill(0) 
      d_sfc = fill(0) 

      do k=1,nsl
       do j=1,jm
        do i=1,im
          d(i,j,k) = h(i,j)+E(i,j)
        enddo
       enddo
      enddo

      do k=1,nsl
       do j=1,jm
        do i=1,im
          d_sfc(i,j,k) = d(i,j,k)*zz(k)
          Vol(i,j,k) = area(i,j) *dz(i,j,k)*d(i,j,k)

#ifdef DEBUG
  write(6,*) i,j,k
  write(6,*) "h",h(i,j)
  write(6,*) "E",E(i,j)
  write(6,*) "d",d(i,j,k)
  write(6,*) "zz",zz(k)
  write(6,*) "area",area
  write(6,*) "d_sfc",d_sfc(i,j,k)
  write(6,*) "Vol",Vol(i,j,k)
#endif

        enddo
       enddo
      enddo  

      end subroutine USER_update_NCOM_grid

