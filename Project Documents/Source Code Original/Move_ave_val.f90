SUBROUTINE move_ave_val(nlay_old,bszlyd,valuearr,nlay_new,laydepth_new)
!
IMPLICIT NONE
!
! Subroutine arguments
!
INTEGER :: nlay_new,nlay_old
REAL,DIMENSION(*) :: bszlyd,laydepth_new,valuearr
!
! Local variables
!
REAL :: depth
INTEGER :: lay
REAL,DIMENSION(nlay_old) :: temparr
REAL :: valbydepth
!
!     + + + purpose + + +
!     averages new layer values across old layers and moves new values
!     into the same array
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     nlay_old   - number of layers in old layering
!     bszlyd     - depth to bottom of old soil layers
!     valuearr   - soil property variable array
!     nlay_new   - number of layers in new layering
!     laydepth_new - depth to bottom of new soil layers
 
!     + + + local variables + + +
 
!     + + + local definitions + + +
!     lay     - layer index
!     temparr - temporary array to hold values from old layering
!     depth   - depth in soil of top of layer
 
!     + + + functions called + + +
 
!     + + + end specifications + + +
 
      ! save old array values to be used in averaging
DO lay = 1,nlay_old
  temparr(lay) = valuearr(lay)
END DO
 
      ! start from soil surface
depth = 0.0
DO lay = 1,nlay_new
  valuearr(lay) = valbydepth(nlay_old,bszlyd,temparr,0,depth,laydepth_new(lay))
  depth = laydepth_new(lay)
END DO
! 
END SUBROUTINE move_ave_val
