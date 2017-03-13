SUBROUTINE invproc(nlay,thick,xcomp)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
INTEGER :: nlay
REAL,DIMENSION(mnsz) :: thick,xcomp
!
! Local variables
!
REAL :: dthick
INTEGER :: idx,odx
REAL,DIMENSION(mnsz) :: ithick,ixcomp,othick
!
!     + + + purpose + + +
!
!	    invert the component passed to xcomp
!
!     + + + keywords + + +
!     inversion, tillage
!
!     + + + argument declarations + + +
 
!
!     + + + argument definitions + + +
 
!     nlay		- number of soil layers used
!     thick		- thickness of each layer in a subregion
!	    xcomp		- component that needs inverting
!
 
!     + + + local variables + + +
 
 
!     + + + local variable definitions + + +
!     idx      - input index for layers
!     odx      - output index for layers
!     ithick   - inverted thickness of layers
!     ixcomp   - inverted property of layers
!     othick   - temp thickness
!     dthick   - delta thickness
!
! create inverted layer thickness and property arrays
! and zero out output array
!
DO idx = 1,nlay
  ithick(idx) = thick(nlay-idx+1)
  ixcomp(idx) = xcomp(nlay-idx+1)
  xcomp(nlay-idx+1) = 0.0
  othick(idx) = thick(idx)
END DO
 
idx = 1
odx = 1
DO
 
  dthick = min(ithick(idx),othick(odx))
  xcomp(odx) = xcomp(odx) + ixcomp(idx)*dthick
  ithick(idx) = ithick(idx) - dthick
  othick(odx) = othick(odx) - dthick
  IF (ithick(idx).EQ.0.0) idx = idx + 1
  IF (othick(odx).EQ.0.0) odx = odx + 1
  IF (idx.GT.nlay.OR.odx.GT.nlay) THEN
 
     DO odx = 1,nlay
        xcomp(odx) = xcomp(odx)/thick(odx)
     END DO
     GO TO 99999
  END IF
END DO
! 
99999 END SUBROUTINE invproc
