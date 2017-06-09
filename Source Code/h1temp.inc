!     ------------------------------------------------------------------
!     file 'h1temp.inc'

!     This common block contains the state variables relating to
!     the soil temperature values by layer generated within HYDROLOGY.

!     Parameter include files:

!     + + + VARIABLE DECLARATIONS + + +

      real             ahtsav(mnsz,mnsub)
      real             ahtsmx(mnsz,mnsub)  !used
      real             ahtsmn(mnsz,mnsub)  !used
      real             ahfice(mnsz,mnsub)

      common / h1temp /                                                 &
     &                  ahtsav, ahtsmx, ahtsmn, ahfice

!     + + + VARIABLE DEFINITIONS + + +

!     ahtsav - Mean daily soil temperature (deg C)
!     ahtsmx - Maximum daily soil temperature (deg C)
!     ahtsmn - Minimum daily soil temperature (deg C)
!     ahfice - fraction of soil water in layer which is frozen


