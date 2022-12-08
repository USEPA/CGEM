!*******************************************************************************
! 12/02/2022 Wilson Melendez: Removed icent and jcent variables, and INPUT_VARS
!                             module.
!*******************************************************************************

      subroutine USER_get_masks()

     !Sets fm and sdm, updates nza

      USE Model_dim
      USE Grid, ONLY: fm,wsm,depth

      IMPLICIT NONE

      integer i,j,k,nz
      integer :: fmtemp(im,jm)  !temp array needed for reading in mask for POM grid
      character(200) filename

      fm = 0.    !default land

      if(Which_gridio.eq.0) then !FishTank

         fm  = 1.  !Everything is water
         wsm = 0   !Everything is on the shelf

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
              if (depth(i,j).gt.0.) then  !Mask to determine if cell is land or ocean  
               fm(i,j,:) = 1.
              else
               nza(i,j) = 0     !Loop depends on nza, not fm,  If land, no active layers
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
            nza(i,j) = INT(fm(i,j,1))*nza(i,j)
          enddo
         enddo
         close(19)

      else
        write(6,*) "Which_gridio=",Which_gridio," does not exist."
        write(6,*) "Change in Which_gridio in Model_dim.txt to 0==0D, 1==EFDC, or 2==NCOM"
        stop

      endif

#ifdef DEBUG
write(6,*) "---USER_get_masks---"
write(6,*)
#endif

      return
      end subroutine USER_get_masks


      subroutine USER_update_masks()
     
      !USE Model_dim
      !USE Grid
      !USE Hydro

      IMPLICIT NONE

      !STUB- update if water cells change during simulation

      end subroutine USER_update_masks
