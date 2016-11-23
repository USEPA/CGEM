Subroutine INPUT_VARS_allocate

USE INPUT_VARS 
USE Model_dim, ONLY : nospA, nospZ, nf

IMPLICIT NONE

!---Phytoplankton 
ALLOCATE( ediblevector(nospZ,nospA) )
ALLOCATE( umax(nospA)  )
ALLOCATE( alpha(nospA) )
ALLOCATE( beta(nospA) )
ALLOCATE( respg(nospA) )
ALLOCATE( respb(nospA) )
ALLOCATE( QminN(nospA) )
ALLOCATE( QminP(nospA) )
ALLOCATE( QmaxN(nospA) )
ALLOCATE( QmaxP(nospA) )
ALLOCATE( Kn(nospA) )
ALLOCATE( Kp(nospA) )
ALLOCATE( Ksi(nospA) )
ALLOCATE( KQn(nospA) )
ALLOCATE( KQp(nospA) )
ALLOCATE( nfQs(nospA) )
ALLOCATE( vmaxN(nospA) )
ALLOCATE( vmaxP(nospA) )
ALLOCATE( vmaxSi(nospA) )
ALLOCATE( aN(nospA) )
ALLOCATE( volcell(nospA) )
ALLOCATE( Qc(nospA) )
ALLOCATE( Athresh(nospA) )
ALLOCATE( mA(nospA) )
ALLOCATE( A_wt(nospA) )
!---Zooplankton
ALLOCATE( Zeffic(nospZ) )
ALLOCATE( Zslop(nospZ) )
ALLOCATE( Zvolcell(nospZ) )
ALLOCATE( ZQc(nospZ) )
ALLOCATE( ZQn(nospZ) )
ALLOCATE( ZQp(nospZ) )
ALLOCATE( ZKa(nospZ) )
ALLOCATE( Zrespg(nospZ) )
ALLOCATE( Zrespb(nospZ) )
ALLOCATE( Zumax(nospZ) )
ALLOCATE( Zm(nospZ) )
!----Sinking Terms----------------------------------
ALLOCATE( ws(nf) ) 

!Light curve parameters
ALLOCATE( alphad(nospA) ) ! Initial slope of photosynthesis-irradiance curve / Vmax
ALLOCATE( betad(nospA) )  ! Photoinhibition constant / Vmax

!Diatom array
ALLOCATE( is_diatom(nospA) )

END Subroutine INPUT_VARS_allocate
