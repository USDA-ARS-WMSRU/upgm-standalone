FUNCTION erf(x)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: x
REAL :: erf
!
! Local variables
!
REAL :: a,b,c,d,e,f,g,h,i,j,k,x2,x3,x4,x5,z
!
!**********************************************************************
 
!     + + + purpose + + +
!     for the equation y = erf(x), the function returns y, given x
!     written by l. hagen and coded by i. elmanyawi.
 
!     + + + argument declarations + + +
 
!     + + + local variables + + +
!
!     + + + end specifications + + +
!
a = -0.000018936
b = 0.030284321
c = 1.12891921
d = 0.37693092
e = 0.029375235
f = 0.088848989
g = 0.068200064
h = 0.022155958
i = 0.050754183
j = 0.038090749
k = 0.034275052
 
z = x
x = abs(x)
x2 = x*x
x3 = x2*x
x4 = x3*x
x5 = x4*x
 
erf = (a+c*x+e*x2+g*x3+i*x4+k*x5)/(1.000000+b*x+d*x2+f*x3+h*x4+j*x5)
erf = erf*z/x
!
END FUNCTION erf
