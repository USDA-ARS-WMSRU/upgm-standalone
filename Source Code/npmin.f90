subroutine npmin(j,ndat)
!
    use constants, only : mnsz
    use nitrogen
implicit none

    type(nitrogen_data) :: ndat
!
! Dummy arguments
!
integer :: j
!
! Local variables
!
real :: rmn,roc,rto
!
!     + + + purpose + + +
!     this subroutine computes mineral p flux between the labile(ap), active
!     mineral(pmn) and stable mineral(op) p pools.
 
!     + + + common blocks + + +
 
 
! local includes
!     include 'chumus.inc'
!     include 'cenvr.inc'
 
!     + + + local variables + + +
!
!     + + + local variable definitions + + +
!     rto - interim variable (2.171)
!     rmn - mineral p flow rate between labile and active p pools (kg/ha/d)
!     roc - mineral p flow rate between active and stable p pools (kg/ha/d)
!
!     + + + output format + + +
!
!     + + + end of specifications + + +
!
!     s5=.1*sut*exp(.115*t(j)-2.88)
!     calculate amount of p flowing from labile to active(rmn>0) or from active
!     to labile(rmn<0) mineral p pools. modified eqn. 2.171. pmn(j)=amount of
!     active mineral p pool.
rto = ndat%psp(j)/(1.-ndat%psp(j))
rmn = (ndat%ap(j)-ndat%pmn(j)*rto)
if (rmn<0.) rmn = rmn*.1
!
!     calculate amount of p flowing from stable to active(roc>0) or from active
!     to stable(roc<0) mineral p pools.  eqn. 2.176. op(j)=amount of stable
!     mineral p pool.
 
roc = ndat%bk(j)*(4.*ndat%pmn(j)-ndat%op(j))
if (roc<0.) roc = roc*.1
ndat%op(j) = ndat%op(j) + roc
ndat%pmn(j) = ndat%pmn(j) - roc + rmn
ndat%ap(j) = ndat%ap(j) - rmn
!     write (38,3017)jd,psp(j),pmn(j),op(j),ap(j),rmn
!3017 format (1x,i3,1x,5(f9.4,1x))
!
end subroutine npmin
