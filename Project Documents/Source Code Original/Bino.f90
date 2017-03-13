FUNCTION bino(n,k,p)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: k,n
REAL :: p
REAL :: bino
!
! Local variables
!
REAL :: bico
REAL*8 :: dble
REAL :: factln
!
!     + + + purpose + + +
!     addition to the bico function which returns binomial coefficient
!     as a floating point number.
!
!     modification based on:
!     'numerical recipes - the art of scientific computing',
!     w.h. press, b.p. flannery, s.a. teukolsky, w.t. vetterling
!     cambridge university press, 1986
!     pg 158
!
!     + + + keywords + + +
!     binomial function
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!     n,k    - inputs for computing binomial coefficient
!     p      - probability value
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     bico   - computed binomial coefficient
!
!     + + + end specifications + + +
!
bico = anint(exp(factln(n)-factln(k)-factln(n-k)))
bino = bico*(p**dble(k))*((1.0-p)**dble(n-k))
! 
END FUNCTION bino
