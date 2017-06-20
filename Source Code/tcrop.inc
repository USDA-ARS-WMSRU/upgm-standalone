!$Author: fredfox $
!$Date: 2005-02-01 22:58:53 $
!$Revision: 1.10 $
!$Source: /weru/cvs/weps/weps.src/inc/manage/tcrop.inc,v $
!     ------------------------------------------------------------------
!     file 'manage/tcrop.inc'
!
!     This common block consists of variables that contain the crop
!     property values that are "eventually" going to be moved into the
!     "decomp" pools.  Thus, these are used for temporary storage
!     before that transfer occurs, ie. like a "temporary" pool.
!     
!     Management "effects" that will deposit crop material (values)
!     into the "temporary" pool variables are:
!     
!        cut - standing live crop mass to temporary crop flat mass
!       flat - standing live crop mass to temporary crop flat mass
!              (does the same thing as cut at a height of zero)
!       kill - all crop biomass pools are moved to the temporary
!              mass pools.
!     
!     The "transfer" management effect then moves the values from the
!     temporary pool variables to the "decomp" pool variables.
!
!     Parameter include files required:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
      real             atmstandleaf(mnsub) !added state !in biomat
      real             atmstandstore(mnsub) !added state !in biomat

      real             atmflatstem(mnsub) !added state !in biomat
      real             atmflatleaf(mnsub) !added state !in biomat
      real             atmflatstore(mnsub) !added state !in biomat

      real             atmflatrootstore(mnsub) !added state !in biomat
      real             atmflatrootfiber(mnsub) !added state !in biomat

      real             atmbgstemz(mnsz,mnsub) !added state !in biomat
      real             atmbgleafz(mnsz,mnsub) !added state !in biomat
      real             atmbgstorez(mnsz,mnsub) !added state !in biomat

      real             atmbgrootstorez(mnsz,mnsub) !added state !in biomat
      real             atmbgrootfiberz(mnsz,mnsub) !added state !in biomat

      real             atzht(mnsub)  !changed from tczht state !in biomat
      real             atdstm(mnsub) !changed from tcdstm state !in biomat
      real             atxstmrep(mnsub) !changed from tcxstmrep state !in biomat
      real             atzrtd(mnsub) !changed from tczrtd state !in biomat
      real             atgrainf(mnsub) !added state !in biomat

      common / tcrop /                                                  &
     &           atmstandleaf, atmstandstore,             &
     &           atmflatstem, atmflatleaf, atmflatstore,                &
     &           atmflatrootstore, atmflatrootfiber,                    &
     &           atmbgstemz, atmbgleafz, atmbgstorez,                   &
     &           atmbgrootstorez, atmbgrootfiberz,                      &
     &           atzht, atdstm, atxstmrep, atzrtd, atgrainf

!     + + + VARIABLE DEFINITIONS + + +

!     atmstandstem - crop standing stem mass (kg/m^2)
!     atmstandleaf - crop standing leaf mass (kg/m^2)
!     atmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     atmflatstem  - crop flat stem mass (kg/m^2)
!     atmflatleaf  - crop flat leaf mass (kg/m^2)
!     atmflatstore - crop flat storage mass (kg/m^2)

!     atmflatrootstore - crop flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     atmflatrootfiber - crop flat root fibrous mass (kg/m^2)

!     atmbgstemz  - crop buried stem mass by layer (kg/m^2)
!     atmbgleafz  - crop buried leaf mass by layer (kg/m^2)
!     atmbgstorez - crop buried storage mass by layer (kg/m^2)

!     atmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     atmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)

!     atzht  - Crop height (m)
!     atdstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     atxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     atzrtd  - Crop root depth (m)
!     atgrainf - internally computed grain fraction of reproductive mass
