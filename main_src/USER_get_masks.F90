      subroutine USER_get_masks()

      USE Model_dim
      USE Grid, ONLY: fm,wsm,h

      IMPLICIT NONE

      !integer, intent (out) :: fm(im,jm)  ! land(0)/sea(1) mask for NCOM
      !                                    ! other for EFDC
      !integer, intent (out) :: wsm(im,jm) ! shelf(0)/open ocean(1) mask
      integer i,j
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

      else if (Which_gridio.eq.2) then !NCOM
         do j=1,jm
         do i=1,im
            !Land-sea mask
            if (h(i,j).gt.1.) then  !Mask to determine if cell is land or ocean  (contains number of layers, with 0 layers = land)
               fm(i,j) = nz_max
            else
                fm(i,j) = 0
            endif
            !Open ocean mask
            if (h(i,j).le.100) then !Mask to determine if cell is on the shelf or open ocean
               wsm(i,j) = 0 !shelf
            else
               wsm(i,j) = 1 !open ocean
            endif
         enddo
         enddo
      endif

      return
      end subroutine USER_get_masks


      subroutine USER_update_masks()
     
      USE Model_dim
      USE Grid
      USE Hydro

      IMPLICIT NONE

      integer i,j

      if (Which_gridio.eq.2) then !NCOM
        !Redo mask in terms of Ko's variable S, some are undefined at h<=100
        do j=1,jm
          do i=1,im
             if (S(i,j,nsl).le.0) then
               wsm(i,j) = 0 ! shelf
             endif
          enddo
        enddo
      endif

      end subroutine USER_update_masks
