    module datetime
    implicit none

    interface
    module function julday(dd,mm,yyyy) result(jday)
    !     + + + purpose + + +
    !     in this routine julday returns the julian day number which begins at
    !     noon of the calendar date specified by day "dd", month "mm", & year "yyyy"
    !     all are integer variables. positive year signifies a.d.; negative, b.c.
    !     remember that the year after 1 b.c. was 1 a.d.
    !     the following invalid calendar dates are checked for and reported:
    !     1. zero year
    !     2. dates between 4/10/1582 and 15/10/1582 are specified.
    !
    integer, intent(in) :: dd   ! integer value of day in the range 1-31
    integer, intent(in) :: mm   ! integer value of month in the range 1-12
    integer, intent(in) :: yyyy ! integer value of year (negative a.d., positive b.c.)
    integer :: jday           ! value returned by the julday function. debe added 09/09/09
    end function julday


    module function difdat(d1,m1,yyy1,d2,m2,yyy2) result (diff)
    !     + + + purpose + + +
    !     two dates are passed to this function and the number of days between
    !     them is returned. the important thing to remember here is that the
    !     first date is subtracted _from_ the second.
    !     example:
    !        d1 m1 yyy1    d2 m2 yyy2   returns   meaning
    !        01 01 1992    02 01 1992   1         1 day from 01/01/1992 it will
    !                                             be 02/01/1992
    !        02 01 1992    01 01 1992   -1        -1 day from 02/01/1992 (or 1
    !                                             day ago) it was 01/01/1992
    !
    integer, intent(in) :: d1   ! first date day
    integer, intent(in) :: m1   ! first date month
    integer, intent(in) :: yyy1 ! first date year
    integer, intent(in) :: d2   ! second date day
    integer, intent(in) :: m2   ! second date month
    integer, intent(in) :: yyy2 ! second date year
    integer :: diff           ! difference, in days, between the first date and the second.
    end function difdat
    
    module function dayear(dd,mm,yyyy) result (numDays)
    !     + + + purpose + + +
    !     given a date in dd/mm/yyyy format,
    !     dayear will return the number of days
    !     from the first of that year.
    !
    integer, intent(in) :: dd   ! integer value of day in the range 1-31
    integer, intent(in) :: mm   ! integer value of month in the range 1-12
    integer, intent(in) :: yyyy ! integer value of year (negative a.d., positive b.c.)
    integer :: numdays          ! the number of days from the first of that year
    end function dayear
    
    end interface

! Implementations
    contains
!!!!!JULDAY
    module procedure julday
    !
    implicit none
    !
    integer, parameter :: igreg = 15 + 31*(10+12*1582)
    integer :: ja, jm, jy, y_nw
    
    if (yyyy==0) write (*,*) 'there is no year zero'
    if ((yyyy==1582).and.(mm==10).and.(dd<15).and.(dd>4)) write (*,*)               &
        & 'this is an invalid date'
    if (yyyy<0) then
        y_nw = yyyy + 1
    else
        y_nw = yyyy
    end if
    if (mm>2) then
        jy = y_nw
        jm = mm + 1
    else
        jy = y_nw - 1
        jm = mm + 13
    end if
    jday = int(365.25*jy) + int(30.6001*jm) + dd + 1720995
    if (dd+31*(mm+12*y_nw)>=igreg) then
        ja = jy/100
        !         ja=int(dble(0.01)*dble(jy))
        jday = jday + 2 - ja + int(dble(0.25)*dble(ja))
    end if
    !
    end procedure julday
!!!!!/JULDAY

!!!!!DIFDAT
    module procedure difdat
    !
    implicit none
    !
    diff = julday(d2,m2,yyy2) - julday(d1,m1,yyy1)
    !
    end procedure difdat
!!!!!/DIFDAT

!!!!!DAYEAR
    module procedure dayear
    !
    implicit none
    !
    numdays = difdat(1,1,yyyy,dd,mm,yyyy) + 1
    !
    end procedure dayear
!!!!!/DAYEAR

    end module datetime