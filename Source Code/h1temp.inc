!     ------------------------------------------------------------------
!     file 'h1temp.inc'

!     This common block contains the state variables relating to
!     the soil temperature values by layer generated within HYDROLOGY.

!     Parameter include files:

!     + + + VARIABLE DECLARATIONS + + +

      real             ahtsmx(mnsz,mnsub)  !used
      real             ahtsmn(mnsz,mnsub)  !used

      common / h1temp /                                                 &
     &                  ahtsmx, ahtsmn

!     + + + VARIABLE DEFINITIONS + + +

!     ahtsmx - Maximum daily soil temperature (deg C)
!     ahtsmn - Minimum daily soil temperature (deg C)



