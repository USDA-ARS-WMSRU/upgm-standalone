FUNCTION func(y)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: y
REAL :: func
!
!  this function is just the equation for a straight line
!  and is used with trapzd.for which performs the integral
!  of this line.  the function is used for the triangular root distribution.
!  other equations for a line can also be used if the distribution
!  is not triangular.
! 
func = 0.5*y
! 
END FUNCTION func
