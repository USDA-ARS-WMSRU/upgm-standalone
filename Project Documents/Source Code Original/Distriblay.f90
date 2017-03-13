SUBROUTINE distriblay(nlay,bszlyd,bszlyt,layval,insertval,begind,endd)
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: begind,endd,insertval
INTEGER :: nlay
REAL,DIMENSION(nlay) :: bszlyd,bszlyt,layval
!
! Local variables
!
REAL :: depth,interval_thick,prevdepth,thick
REAL :: intersect
INTEGER :: lay
!
!     the following subroutine arguments are not used: bszlyt  jcaii  8/08/08
!
!     + + + purpose + + +
!     distributes a quantity of material over an underground interval
!     adding material in each layer in proportion to the fraction of
!     the interval that is each soil layer.
 
!     note: insertval and layval need the same units
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     nlay   - number of layers in soil input array
!     bszlyd - depth to bottom of soil layers (mm)
!     bszlyt - thickness of soil layers (mm)
!     layval - the layer bins that will be supplemented by added material
!     insertval - the quantity of material to be distributed into layval
!     begind - uppper depth of soil interval (mm)
!     endd   - lower depth of soil interval (mm)
 
!     + + + local variables + + +
 
!     + + + local definitions + + +
!     lay    - layer index
!     depth  - depth to bottom of present soil layer (mm)
!     prevdepth - previous cumulative depth in soil (mm)
!     thick  - thickness of soil slice (intersection of soil interval
!              and soil layer) (mm)
!     interval_thick - thickness of soil interval over which quantity
!                      of material is being inserted. (mm)
 
!     + + + functions called + + +
 
!     + + + end specifications + + +
 
      ! interval thickness is used repeatedly, calculate here
interval_thick = endd - begind
      ! start at soil surface
depth = 0.0
DO lay = 1,nlay
          ! set depth of layer upper boundary
  prevdepth = depth
          ! set depth of layer lower boundary
  depth = bszlyd(lay)
  IF (interval_thick.GT.0.0) THEN
          ! find thickness of intersection between layer and interval
     thick = intersect(prevdepth,depth,begind,endd)
                  ! put proportional amount in this layer
     IF (thick.GT.0.0) layval(lay) = layval(lay)                                &
                                   & + insertval*thick/interval_thick
              ! zero interval thickness
  ELSE IF ((endd.LE.depth).AND.(endd.GT.prevdepth)) THEN
                  ! interval in this layer, put all in this layer
     layval(lay) = layval(lay) + insertval
  END IF
END DO
! 
END SUBROUTINE distriblay
