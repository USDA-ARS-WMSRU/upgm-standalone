subroutine nconc(po,p5,p1,a)
!
implicit none
!
! Dummy arguments
!
real :: a,p1,p5,po
!
! Local variables
!
real :: a5,dfda,ea,ea1,eg,eg1,fu,peg,pg5,po1,pog,rt
integer :: i
!
!     + + + purpose + + +
!     this subroutine computes parameters of an equation describing the
!     n and p relations to biomass accumulation.
!
!     + + + argument declarations + + +
 
!     + + + local variables + + +
!
!     + + + local variable definitions + + +
!     a5 - intermediate variables for solving the n or p uptake ratios
!     ea - intermediate variables for solving the n or p uptake ratios
!     ea1 - intermediate variables for solving the n or p uptake ratios
!     eg - intermediate variables for solving the n or p uptake ratios
!     peg - intermediate variables for solving the n or p uptake ratios
!     po1 - intermediate variables for solving the n or p uptake ratios
!     rt - intermediate variables for solving the n or p uptake ratios
!     pg5 - intermediate variables for solving the n or p uptake ratios
!     fu - intermediate variables for solving the n or p uptake ratios
!     dfda - intermediate variables for solving the n or p uptake ratios
!
a = 5.
do i = 1,10
  a5 = a*.5
  ea = exp(a)
  ea1 = ea - 1.
  eg = exp(-a5)
  pog = po*eg
  eg1 = exp(a5)
  peg = p1*(ea-eg1)
  po1 = po*(1.-eg)
  rt = peg - po1
  pg5 = .5*pog
  fu = rt/ea1 + pog - p5
  if (abs(fu)<1.E-7) go to 10
  dfda = (ea1*(p1*(ea-.5*eg1)-pg5)-ea*rt)/(ea1*ea1) - pg5
  a = a - fu/dfda
end do
!     write (*,4) a,fu
 10   p5 = (p1*ea-po)/ea1
po = po - p5
return
 
 1000 format (//t10,'nconc did not converge',2E16.6)
!     stop
!
end subroutine nconc
