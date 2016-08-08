      subroutine USER_get_basic_grid(dz,d,d_sfc)

      USE Model_dim

      real, intent (out) :: dz(im,jm,nsl)
      real, intent (out) :: d(im,jm,nsl)
      real, intent (out) :: d_sfc(im,jm,nsl)

!      dz = 25.             !Thickness of cell
!      d  = 25.             !Depth from surface to bottom of cell
!      d_sfc(:,:,1) = 12.5  !Depth from surface to cell center
!      d_sfc(:,:,2) = -9999 


      open (19,file='./data/dz.dat',status='old')
      read (19,*)    !Assumes header file
      read (19,*) dz
      close(19)

      open (19,file='./data/d.dat',status='old')
      read (19,*)    !Assumes header file
      read (19,*) d 
      close(19)

      open (19,file='./data/d_sfc.dat',status='old')
      read (19,*)    !Assumes header file
      read (19,*) d_sfc 
      close(19)


      return

      end subroutine USER_get_basic_grid

