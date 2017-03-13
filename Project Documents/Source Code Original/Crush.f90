SUBROUTINE crush(alpha,beta,nlay,mf)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'asd.inc'
!
! Subroutine arguments
!
REAL :: alpha,beta
INTEGER :: nlay
REAL,DIMENSION(msieve+1,mnsz) :: mf
!
! Local variables
!
REAL :: bino
REAL :: chk,dratio,prob
INTEGER :: i,j,k,m
REAL,DIMENSION(msieve+1,msieve+1) :: pmat
REAL,DIMENSION(msieve+1) :: predmf
!
!     + + + purpose + + +
!     this subroutine  performs the crushing or breaking down of
!     soil aggregates into smaller sizes based on the initial aggregate
!     size distribution and two crushing parameters (alpha and beta).
!     the crushing parameters are assumed to be a function of the
!     soil intrinsic properties, soil water content, and tillage implement.
!
!     + + + keywords + + +
!     aggregate size distribution, asd, sieves, mass fractions
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!
!     alpha  - aggregate size distribution factor
!     beta   - crushing intensity factor
!     nlay   - number of soil layers used
!     mf     - mass fractions of aggregates within sieve cuts
!              (sum of all mass fractions are expected to = 1.0)
!
!     + + + accessed common block variable definitions + + +
!
!     mdia   - array containing geometric mean diameters of sieve cuts
!     nsieve - number of sieves used
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     pmat   - probability matrix
!     dratio - ratio of sieve cut d to maximum sieve cut d
!     prob   - probability value
!     chk    - variable to chk prob matrix integrity
!     i      - loop variable for sieve cut sizes
!     j      - loop variable for soil layers
!     k      - loop variable for sieve cut probabilities
!     predmf - local array to hold predicted mass fractions
!              before updating mf
!
!     + + + functions called + + +
 
 
!     + + + end specifications + + +
!
!     for each soil layer
DO j = 1,nlay
!         compute transition matrix
  DO i = 1,nsieve + 1
     dratio = mdia(i)/mdia(nsieve+1)
     prob = 1.0 - exp(-alpha+dratio*beta)
     chk = 0.0
     DO k = 1,i
        pmat(i,k) = bino(i-1,k-1,prob)
        chk = chk + pmat(i,k)
     END DO
     IF (abs(chk-1.0).GT.0.001) THEN
        WRITE (0,*) 'problem transition matrix (crush) chk:',(chk-1.0)
!                 debug code to print out transition matrix
        DO k = nsieve + 1,1, - 1
           PRINT *,(pmat(k,m),m=k,1,-1)
        END DO
        CALL exit(1)
     END IF
  END DO
  DO i = 1,nsieve + 1
     predmf(i) = 0.0
     DO k = i,nsieve + 1
        predmf(i) = predmf(i) + mf(k,j)*pmat(k,i)
     END DO
  END DO
!         put predicted mass fractions into mf
  DO i = 1,nsieve + 1
     mf(i,j) = predmf(i)
  END DO
END DO
!
END SUBROUTINE crush
