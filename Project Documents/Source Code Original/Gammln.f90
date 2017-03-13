FUNCTION gammln(xx)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: xx
REAL :: gammln
!
! Local variables
!
REAL*8,DIMENSION(6) :: cof
REAL*8 :: dble
REAL*8 :: fpf,half,one,ser,stp,tmp,x
INTEGER :: j
!
!     + + + purpose + + +
!     computes the ln of the gamma function for xx > 0
!     full accuracy is obtained for xx > 1
!     for 0 < xx < 1, the reflection formula (6.1.4) can be used first.
!
!     based on:
!     'numerical recipes - the art of scientific computing',
!     w.h. press, b.p. flannery, s.a. teukolsky, w.t. vetterling
!     cambridge university press, 1986
!     pg 157
!
!     + + + keywords + + +
!     gamma function
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!     xx     - real value for values > 0
!
!     + + + local variables + + +
!
DATA cof,stp/76.18009173D0, - 86.50532033D0,24.01409822D0, - 1.231739516D0,     &
   & 0.120858003D-2, - 0.536382D-5,2.50662827465D0/
DATA half,one,fpf/0.5D0,1.0D0,5.5D0/
!
!     + + + end specifications + + +
!
x = dble(xx) - one
tmp = x + fpf
tmp = (x+half)*log(tmp) - tmp
ser = one
DO j = 1,6
  x = x + one
  ser = ser + cof(j)/x
END DO
!
gammln = tmp + log(stp*ser)
! 
END FUNCTION gammln
