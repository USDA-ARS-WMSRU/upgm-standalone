FUNCTION rootlay(rtdepth,lthick,nlay)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: nlay
REAL :: rtdepth
REAL,DIMENSION(nlay) :: lthick
INTEGER :: rootlay
!
! Local variables
!
REAL :: d
INTEGER :: i
!
IF ((rtdepth*1000.0).LT.lthick(1)) THEN
  rootlay = 1
  GO TO 99999
END IF
d = lthick(1)
DO i = 2,nlay
  d = d + lthick(i)
  IF ((rtdepth*1000.0).LT.d) THEN
     IF ((d-(rtdepth*1000.)).LT.((rtdepth*1000.)-(d-lthick(i)))) THEN
        rootlay = i
     ELSE
        rootlay = i - 1
     END IF
     GO TO 99999
  END IF
  rootlay = nlay
END DO
!
99999 END FUNCTION rootlay
