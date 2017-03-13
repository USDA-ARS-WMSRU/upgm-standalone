FUNCTION begtrm(val)
!
IMPLICIT NONE
!
! Function arguments
!
CHARACTER(*) :: val
INTEGER :: begtrm
!
! Local variables
!
INTEGER :: idx
INTEGER :: len
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + end specifications + + +
!
DO idx = 1,len(val)
  IF (val(idx:idx).NE.' ') THEN
     begtrm = idx
     RETURN
  END IF
END DO
! 
begtrm = 1
!
END FUNCTION begtrm
