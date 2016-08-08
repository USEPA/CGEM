! ----------------------------------------------------------
      SUBROUTINE  ickfile (fn, RES)       
!            written  by      D.S.Ko
!            modified by B.E. Herchenroder/EMVL, April 2010
!                                                May   2011
! ----------------------------------------------------------
!     Check file status (unix)
!
!     input:
!       fn - file name including dir
!     output:
!       RES = 0   fn not exist
!       RES = 1	  fn exist and uncompressed
!       RES =-1   fn exist but compressed
!--------------------------------------------------
      IMPLICIT NONE
      
!----------------------------      
! Declare interface variable      
!----------------------------       
      CHARACTER*(*), INTENT(IN)  ::  fn
      INTEGER      , INTENT(OUT) :: RES

!-------------------------      
! Local variables      
!------------------------- 
      INTEGER  ierr
      INTEGER  l                    
      INTEGER  stat
      INTEGER  statb(13)
      CHARACTER*128  fnZ
!----------------------------        

      RES     = 1
      ierr = stat ( fn, statb )
      if (ierr.eq.0) RETURN	 ! no error; file exit

      RES     = -1
      l = index(fn,' ')-1
      fnZ = fn(:l)//'.Z'
      ierr = stat ( fnZ, statb )
      if (ierr.eq.0) RETURN	 ! no error; compressed file
                                 ! exit

      RES = 0                    ! error

      RETURN
      END SUBROUTINE ickfile
