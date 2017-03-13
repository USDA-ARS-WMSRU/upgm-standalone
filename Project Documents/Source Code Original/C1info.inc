!$Author: wagner $
!$Date: 2002-02-22 21:47:22 $
!$Revision: 1.3 $
!$Source: /weru/cvs/weps/weps.src/inc/c1info.inc,v $

!     This common block contains the character variables containing
!     information about the crop

!     Parameter include files:  p1strlen.inc

!     + + + VARIABLE DECLARATIONS + + +

      character*(80)  ac0nam(mnsub)
      integer         ac0idc(mnsub)

      common / c1info /                                                 &
     &                  ac0nam, ac0idc

!     + + + VARIABLE DEFINITIONS + + +

!     ac0nam - Crop name
!     ac0idc - The crop type number (1 = annual, perennial, . . .)

!     ------------------------------------------------------------------
