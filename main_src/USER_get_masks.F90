      subroutine USER_get_masks(fm,wsm)

      USE Model_dim

      integer, intent (out) :: fm(im,jm)  ! land(0)/sea(1) mask
      integer, intent (out)    :: wsm(im,jm) ! shelf(0)/open ocean(1) mask
      integer i,j
 
      if(Which_gridio.eq.0) then
       fm  = 1  !Everything is water
       wsm = 0  !Everything is on the shelf
      elseif (Which_gridio.eq.1) then !EFDC
       fm=0  !Default Land
       wsm=0 !Everything is shelf
       do j=1,jm
        do i=1,im  !If has layers, then water
          if(nza(i,j).gt.0) fm(i,j)=1
        enddo
       enddo
      endif

      return
      end subroutine USER_get_masks
