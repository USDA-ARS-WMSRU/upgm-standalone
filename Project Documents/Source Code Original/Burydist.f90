FUNCTION burydist(lay,burydistflg,lthick,ldepth,nlay)
!
! this routine returns the fraction of material buried in layer number
! lay given the burial distribution function type burydistflg and the
! layer thicknesses lthick and the total number of layers in which
! material will be buried nlay and the tillage depth, soil layer
! thicknesses, and the number of soil layers.  it returns the number
! of layers that will be considered to be within the tillage zone for
! this operation.
! 
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! PARAMETER definitions
!
REAL,PARAMETER :: c1exp = 0.5,c2exp = 0.3,c3e1 = 2.925,c3e2 = 1.575,            &
                & c3brk = 0.65,c3split = 0.5
!
! Function arguments
!
INTEGER :: burydistflg,lay,nlay
REAL :: burydist
REAL,DIMENSION(mnsz) :: ldepth,lthick
!
! Local variables
!
REAL :: lower,upper
!
!     the following subroutine arguments are not used: lthick jcaii  8/08/08
!
 
!     argument declarations
 
!     argument definitions
!     lay         - soil layer for which fraction is returned
!     tlay        - number of soil layers affected by tillage
!     burydistflg - distribution function to be used
!              0    o uniform distribution
!              1    o mixing+inversion burial distribution
!              2    o mixing burial distribution
!              3    o inversion burial distribution
!              4    o lifting, fracturing burial distribution
!              5    o compression
!     lthick      - thickness of soil layer
!     ldepth      - distance from surface to bottom of layer
!     nlay        - number of soil layers affected
 
!     local variable declarations
 
 
!     assign depth from surface to upper and lower layer bounds
IF (lay.EQ.1) THEN
  upper = 0.0
ELSE
  upper = ldepth(lay-1)/ldepth(nlay)
END IF
lower = ldepth(lay)/ldepth(nlay)
 
!     find fraction of material buried in layer lay
SELECT CASE (burydistflg)
CASE (1)
  burydist = lower**c1exp - upper**c1exp
CASE (2,5)      ! same for compression and mixing from nat. agron. manual, 508crevisionwstir 071106dtl
  burydist = lower**c2exp - upper**c2exp
CASE (3)
  IF (lower.LE.c3brk) THEN
     burydist = c3split*(lower/c3brk)**c3e1
  ELSE
     burydist = 1.0 - c3split*((1.0-lower)/(1.0-c3brk))**c3e2
  END IF
  IF (upper.LE.c3brk) THEN
     burydist = burydist - c3split*(upper/c3brk)**c3e1
  ELSE
     burydist = burydist - (1.0-c3split*((1.0-upper)/(1.0-c3brk))**c3e2)
  END IF
CASE (4)
  burydist = lower**c1exp - upper**c1exp
CASE DEFAULT         !uniform burial distribution
  burydist = lower - upper
END SELECT
!
END FUNCTION burydist
