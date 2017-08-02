    module datetime
    use constants, only : int32
    implicit none
    
    type date
        integer(int32) :: day
        integer(int32) :: mon
        integer(int32) :: year
    end type
    

    interface
    !     + + + julday(...) + + +
    !     in this routine julday returns the julian day number which begins at
    !     noon of the calendar date specified by day "dd", month "mm", & year "yyyy"
    !     all are integer variables. positive year signifies a.d.; negative, b.c.
    !     remember that the year after 1 b.c. was 1 a.d.
    !     the following invalid calendar dates are checked for and reported:
    !     1. zero year
    !     2. dates between 4/10/1582 and 15/10/1582 are specified.
    module function julday(dd,mm,yyyy) result(jday)
    integer, intent(in) :: dd   ! integer value of day in the range 1-31
    integer, intent(in) :: mm   ! integer value of month in the range 1-12
    integer, intent(in) :: yyyy ! integer value of year (negative a.d., positive b.c.)
    integer :: jday           ! value returned by the julday function. debe added 09/09/09
    end function julday

    !     + + + difdat(...) + + +
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
    module function difdat(d1,m1,yyy1,d2,m2,yyy2) result (diff)
    integer, intent(in) :: d1   ! first date day
    integer, intent(in) :: m1   ! first date month
    integer, intent(in) :: yyy1 ! first date year
    integer, intent(in) :: d2   ! second date day
    integer, intent(in) :: m2   ! second date month
    integer, intent(in) :: yyy2 ! second date year
    integer :: diff           ! difference, in days, between the first date and the second.
    end function difdat
    
    !     + + + dayear(...) + + +
    !     given a date in dd/mm/yyyy format,
    !     dayear will return the number of days
    !     from the first of that year.
    !
    module function dayear(dd,mm,yyyy) result (numDays)
    integer, intent(in) :: dd   ! integer value of day in the range 1-31
    integer, intent(in) :: mm   ! integer value of month in the range 1-12
    integer, intent(in) :: yyyy ! integer value of year (negative a.d., positive b.c.)
    integer :: numdays          ! the number of days from the first of that year
    end function dayear
    
    !     + + + caldat(...) + + +
    !     inverse of the function julday. here 'julian' is input as a julian day
    !     number, and the routine outputs the dd, mm, and yyyy on which the
    !     specified julian day started at noon.
    module subroutine caldat(ijulian,dd,mm,yyyy)
    integer,intent(out) :: dd       !dd in the range 1-31
    integer,intent(out) :: mm       !integer value of mm in the range 1-12
    integer,intent(out) :: yyyy     !yyyy (negative a.d., positive b.c.)
    integer,intent(in)  :: ijulian  !julian day value 
    end subroutine
    
    end interface

! Implementations
    contains
!!!!!JULDAY
    module procedure julday
    use constants, only : gregtojulian
    !
    implicit none
    !
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
    if (dd+31*(mm+12*y_nw)>=gregtojulian) then
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

!!!!!caldat
    module subroutine caldat(ijulian,dd,mm,yyyy)
    use constants, only : juliantogreg
    implicit none
    integer,intent(out) :: dd       !dd in the range 1-31
    integer,intent(out) :: mm       !integer value of mm in the range 1-12
    integer,intent(out) :: yyyy     !yyyy (negative a.d., positive b.c.)
    integer,intent(in)  :: ijulian  !julian day value 
    real :: alpha
    real :: c
    real :: e
    integer :: ja
    integer :: jalpha
    integer :: jb
    integer :: jc
    integer :: jd
    integer :: je
    integer :: julian
    ! start
    julian = ijulian
    if (julian>=juliantogreg) then
        alpha = (dble(julian-1867216)-dble(0.25))/dble(36524.25)
        jalpha = int(alpha)
        ja = julian + 1 + jalpha - int(dble(0.25)*jalpha)
    else
        ja = julian
    end if
    jb = ja + 1524
    c = dble(6680.0) + ((jb-2439870)-dble(122.1))/dble(365.25)
    !c = dble(6680.0) + ((jb-2439870)-dble(122.1))/dble(365.00)!de changed
    jc = int(c)
    jd = 365*jc + int(dble(0.25)*jc)
    e = (jb-jd)/dble(30.6001)
    je = int(e)
    dd = jb - jd - int(dble(30.6001)*je)
    mm = je - 1
    if (mm>12) mm = mm - 12
    yyyy = jc - 4715
    if (mm>2) yyyy = yyyy - 1
    if (yyyy<=0) yyyy = yyyy - 1
    end subroutine
!!!!!/caldat
    end module datetime