      subroutine USER_get_masks(fm,wsm)

      USE Model_dim

      integer, intent (out) :: fm(im,jm)  ! land(0)/sea(1) mask for NCOM
                                          ! other for EFDC
      integer, intent (out) :: wsm(im,jm) ! shelf(0)/open ocean(1) mask
      integer j
      character(100) :: filename 

      if(Which_gridio.eq.0) then !FishTank
       fm  = 1  !Everything is water
       wsm = 0  !Everything is on the shelf
      elseif (Which_gridio.eq.1) then !EFDC
       wsm=0 !Everything is shelf
       write(filename,'(A, A)') trim(DATADIR),'/nz.dat'
       open(unit=19,file=filename)
       read(19,*)
       do j=1,jm
         read(19,*) fm(:,j)
       enddo
       close(19)
      endif

      return
      end subroutine USER_get_masks
