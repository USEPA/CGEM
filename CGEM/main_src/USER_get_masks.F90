      subroutine USER_get_masks()

      USE Model_dim
      USE Grid, ONLY: fm,wsm,depth
#ifdef DEBUG
      USE INPUT_VARS, ONLY: icent,jcent
#endif
      IMPLICIT NONE

      !integer :: fm(im,jm,km)    ! land(0)/sea(1) mask 
      !integer :: wsm(im,jm)     ! shelf(0)/open ocean(1) mask
      integer i,j,k,nz
      integer :: fmtemp(im,jm)  !temp array needed for reading in mask for POM grid
      character(200) filename
      character(80) fmt

#ifdef map_code
write(6,*) "---USER_get_masks---"
write(6,*) " EFDC does not have mask for ocean, everything now is shelf"
write(6,*)
#endif
#ifdef DEBUG
write(6,*) "---USER_get_masks---"
write(6,*) "  nza, fm, wsm, and Depth at i,j,k=",icent,jcent,km
write(6,*) "  =", nza(icent,jcent),fm(icent,jcent,km),wsm(icent,jcent),depth(icent,jcent)
write(6,*) 
#endif

      if(Which_gridio.eq.0) then !FishTank
         fm  = 1.  !Everything is water
         wsm = 0  !Everything is on the shelf
      else if(Which_gridio.eq.1) then !EFDC
         do j=1,jm
          do i=1,im
            nz = nza(i,j)
            do k=1,nz
               fm(i,j,k) = 1.  
            enddo
            !Need mask for shelf if there is open ocean.
          enddo
         enddo
      else if(Which_gridio.eq.2) then !NCOM 
         do j=1,jm
         do i=1,im
              if (depth(i,j).gt.0.) then  !Mask to determine if cell is land or ocean  (contains number of layers, with 0 layers = land)
               fm(i,j,:) = 1.
              endif
              !Open ocean mask
              if (depth(i,j).le.100) then !Mask to determine if cell is on the shelf or open ocean
                 wsm(i,j) = 0 !shelf
              else
                 wsm(i,j) = 1 !open ocean
              endif
         enddo
         enddo
      else if (Which_gridio.eq.3) then !POM
         ! Read in mask data
         write(filename, '(A,A)') trim(DATADIR), '/mask.dat'
         open(19, file=filename, status='old')
         read(19,*) !Header
         do j=1,jm
           read(19,*) fmtemp(:,j)
           do k=1,km
             fm(:,:,k) = fmtemp
           enddo
         enddo
         !update nza using mask data
         do j=1,jm
          do i=1,im 
            nza(i,j) = INT(fm(i,j,1)*nza(i,j))
          enddo
         enddo
         close(19)
      else
        write(6,*) "Which_gridio=",Which_gridio," does not exist."
        write(6,*) "Change in Which_gridio in Model_dim.txt to 0==0D, 1==EFDC, or 2==NCOM"
        stop
      endif

      return
      end subroutine USER_get_masks


      subroutine USER_update_masks()
     
      !USE Model_dim
      !USE Grid
      !USE Hydro

      IMPLICIT NONE

      !STUB- update if water cells change during simulation

      end subroutine USER_update_masks
