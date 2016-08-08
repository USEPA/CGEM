      subroutine USER_get_masks(fm,wsm)

      USE Model_dim

      integer, intent (out) :: fm(im,jm)  ! land(0)/sea(1) mask
      real, intent (out)    :: wsm(im,jm) ! shelf(0)/open ocean(1) mask

      fm  = 1  !Everything is water
      wsm = 0.  !Everything is on the shelf

      return
      end subroutine USER_get_masks
