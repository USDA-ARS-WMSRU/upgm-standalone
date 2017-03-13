!$Author: fredfox $
!$Date: 2006-05-26 17:40:17 $
!$Revision: 1.6 $
!$Source: /weru/cvs/weps/weps.src/inc/d1gen.inc,v $

!     common block of variables containing general information
!     used by the Decomp model.  Variables specified in
!     WERM Global Blocks.

      character*(80)  ad0nam(mnbpls, mnsub)
      real       ad0sla(mnbpls, mnsub)
      real       ad0ck(mnbpls, mnsub)
      integer    adrbc(mnbpls,mnsub)
      real       adxstm(mnbpls,mnsub)

      common / d1gen /                                                  &
     &       ad0nam, ad0sla, ad0ck, adrbc, adxstm


!   + + + VARIABLE DEFINEITIONS + + +

!     ad0name - residue name
!     ad0sla - residue specific leaf area
!     ad0ck  - residue light extinction coeffficient (fraction)
!     adrbc  - residue burial class
!         1   o Fragile-very small (soybeans) residue
!         2   o Moderately tough-short (wheat) residue
!         3   o Non fragile-med (corn) residue
!         4   o Woody-large residue
!         5   o Gravel-rock

!     adxstm - Residue stem diameter (m)
