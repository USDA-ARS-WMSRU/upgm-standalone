!$Author: fredfox $
!$Date: 2005-02-01 22:58:50 $
!$Revision: 1.3 $
!$Source: /weru/cvs/weps/weps.src/inc/h1scs.inc,v $
!     ------------------------------------------------------------------
!     file 'h1scs.inc'
!
!     This common block contains the variables representing the SCS
!     runoff curve number for each subregion.
!
!     Parameter include files:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
!
      real             ah0cng(mnsub)
      real             ah0cnp(mnsub)
!
      common / h1scs /                                                  &
     &                 ah0cng, ah0cnp
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     ah0cng  - SCS runoff curve no. (good)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains
!
!
!     ah0cnp  - SCS runoff curve no. (poor)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the
!
!
!
!     ------------------------------------------------------------------
