!$Author: wagner $
!$Date: 2003-11-14 23:33:50 $
!$Revision: 1.4 $
!$Source: /weru/cvs/weps/weps.src/inc/c1report.inc,v $

!     This common block contains the character variables containing
!     information about the crop

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer         cprevrotation(mnsub)
      integer         cprevcalibrotation(mnsub)

      common / c1report / cprevrotation, cprevcalibrotation

!     + + + VARIABLE DEFINITIONS + + +

!     cprevrotation - rotation count number previously printed
!                     in crop harvest report

!     cprevcalibrotation - rotation count number previously printed
!                          in calibration crop harvest report
