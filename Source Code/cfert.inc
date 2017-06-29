!$Author: fredfox $
!$Date: 2005-02-01 22:58:51 $
!$Revision: 1.5 $
!$Source: /weru/cvs/weps/weps.src/inc/crop/cfert.inc,v $

!     file: cfert.inc

!     This file contains the declaration of the common block of variables
!     related to plant N & P uptake.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real      ap(mnsz), bk(mnsz)
      real      op(mnsz), pmn(mnsz), sn, sp,                    &
     &          sunn, sup, un(mnsz), un1, un2, uno3,                    &
     &          up1, up2, upp, up(mnsz), wno3(mnsz)

      common /fert/ ap,                                                 &
     &              bk,                                                 &
     &              op,                                                 &
     &              pmn,                                                &
     &              sn,                                                 &
     &              sp,                                                 &
     &              sunn,                                               &
     &              sup,                                                &
     &              un, un1, un2, uno3,                                 &
     &              up1, up2, upp, up,                                  &
     &              wno3

!     + + + VARIABLE DEFINITIONS + + +
!     ap - total available P in a layer from all sources (kg/ha)
!     bk - flow coefficient between active and stable P pools (1/d)
!     bn - N uptake parametres at different stages of growth(ratio)
!     bp - P uptake parametres at different stages of growth(ratio)
!     fn - amount of fertilizer N applied at planting (kg/ha)
!     fp - amount of fertilizer P applied at planting (kg/ha)
!     pmn - amount of active mineral P pool (kg/ha)
!     op - amount of stable mineral P pool (kg/ha)
!     sn - nitrogen stress factor
!     sp - phosphorous stress factor
!     sunn - sum of soil supplied N from all rooted layers (kg/ha/d)
!     sup - sum of soil supplied P from all rooted layers (kg/ha/d)
!     un - amount of N supplied by a rooted soil layer (kg/ha/d)
!     un1 - actual crop N concentration (kg/ha/d)
!     un2 - optimal crop N concentration (kg/ha/d)
!     uno3 - N demand rate by the crop (kg/ha/d)
!     up - amount of P supplied by a rooted soil layer (kg/ha/d)
!     up1 - actual crop P concentration (kg/ha/d)
!     up2 - optimal crop P concentration (kg/ha/d)
!     upp - N demand rate by the crop (kg/ha/d)
!     wno3 - total available N in a layer from all sources (kg/ha)
