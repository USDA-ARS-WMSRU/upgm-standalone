FUNCTION intersect(begind_a,endd_a,begind_b,endd_b)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: begind_a,begind_b,endd_a,endd_b
REAL :: intersect
!
!     + + + purpose + + +
!     returns the intersection interval "distance" of two intervals
!     each defined by a greater and lesser value
!     obviously the units must be consistent.
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     begind_a - lesser value of interval a
!     endd_a   - greater value of interval a
!     begind_b - lesser value of interval b
!     endd_b   - greater value of interval b
 
!     + + + end specifications + + +
 
IF ((endd_a.LE.begind_b).OR.(begind_a.GE.endd_b)) THEN
  intersect = 0.0
          ! some part of interval b intersects interval a
ELSE IF ((begind_a.LE.begind_b).AND.(endd_a.GE.endd_b)) THEN
              ! interval a completely surrounds interval b
  intersect = endd_b - begind_b
ELSE IF (begind_a.LE.begind_b) THEN
              ! top part of interval b intersects farther end of interval a
  intersect = endd_a - begind_b
ELSE IF (endd_a.GE.endd_b) THEN
              ! farther end of interval b intersects nearer end of interval a
  intersect = endd_b - begind_a
ELSE
              ! interval b completely surrounds interval a
  intersect = endd_a - begind_a
END IF
! 
END FUNCTION intersect
