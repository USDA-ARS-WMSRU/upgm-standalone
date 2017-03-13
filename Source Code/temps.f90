function temps(bwtdmx,bwtdmn,bctopt,bctmin)
!
implicit none
!
! Dummy arguments
!
real :: bctmin,bctopt,bwtdmn,bwtdmx
real :: temps
!
! Local variables
!
real :: dst0,rto,tgx,x1
!
!     + + + purpose + + +
!     to calculate the temperature stress factor
!     this algorithms was taken from the epic subroutine cgrow.
 
!     + + + kewords + + +
!     temperature stress
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bwtdmx - daily maximum air temperature
!     bwtdmn - daily minimum air temperature
!     bctopt - optimum crop growth temperature
!     bctmin - minimum crop growth temperature
!     temps - value returned by this function for the temperature
!             stress factor
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     dst0 - average of max and min air temperatures
!     rto - interim variable
!     tgx - difference between the soil surface temperature and the minimum
!           temperature for plant growth
!     x1 - difference between the optimum and minimum temperatures for plant
!          growth
 
!     + + + end of specifications + + +
 
!     calculate temperature stress factor
!     following one statement to be removed when soil temperature is available
dst0 = (bwtdmx+bwtdmn)/2.0
tgx = dst0 - bctmin
if (tgx<=0.) tgx = 0.
x1 = bctopt - bctmin
rto = tgx/x1
temps = sin(1.5707*rto)
if (rto>2.) temps = 0.
 
      ! this reduces temperature stress around the optimum
temps = temps**0.25
!    print *, 'in temps, temps = ', temps
!
end function temps
