FUNCTION factln(n)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: n
REAL :: factln
!
! Local variables
!
REAL,DIMENSION(100) :: a
REAL :: gammln
!
!     + + + purpose + + +
!     computes the ln(n!) for n > 0
!
!     based on:
!     'numerical recipes - the art of scientific computing',
!     w.h. press, b.p. flannery, s.a. teukolsky, w.t. vetterling
!     cambridge university press, 1986
!     pg 159
!
!     + + + keywords + + +
!     factorial
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!     n      - real value for values >= 0
!
!     + + + local variables + + +
!
!
DATA a/100* - 1/
!
!     + + + end specifications + + +
!
IF (n.LT.0) WRITE (*,*) 'in function factln(n): negative factorial!!'
IF (n.LE.99) THEN
  IF (a(n+1).LT.0.0) a(n+1) = gammln(n+1.0)
  factln = a(n+1)
ELSE
  factln = gammln(n+1.0)
END IF
! 
END FUNCTION factln
