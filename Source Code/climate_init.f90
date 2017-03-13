subroutine climate_init(icli,cliname)
!
implicit none
!
include 'w1clig.inc'
include 'file.fi'
!
! Dummy arguments
!
character(80) :: cliname
integer :: icli
!
! Local variables
!
character(128) :: header
integer :: idx
!
!+ + + definitions + + +
!     + + + argument definitions + + +
!     icli - a flag to determine which type of weather file to read.
!            a value of 1 indicates that climate data should be read
!            from the cligen weather file.  a value of 0 indicates that
!            a historical climate file will be used.
 
!     + + + local variable definitions + + +
!     header -
!     idx - loop control variable
 
!     + + + common block variables definitions + + +
!     awtyav - average yearly air temperature (deg c) obtained from
!              cligen.
!     awtmav - average monthly air temperature (deg c) obtained from the
!              cligen run file.
!     awtmnav - average monthly minimum air temperature (deg c).
!     awtmxav - average monthly maximum air temperature (deg c).
!     awzmpt - average monthly total precipitation depth (mm)
 
!     + + +  newly added arguments definitions + + +
!     cliname - the name of the location for the climate data.
!
if (icli==1) then    ! read from cligen data file
!
  rewind luicli
!debe add reading in the location climate name
  read (luicli,'(a128)') cliname
!
  do idx = 1,6          ! skip first six lines
     read (luicli,'(a128)') header
!   write(6,*) 'header: ', header,':'
  end do
!
! read monthy average of daily maximum temperature
!
  read (luicli,*) (awtmxav(idx),idx=1,12)
! write(6,*)  (awtmxav(idx), idx = 1,12)
!
  read (luicli,'(a128)') header
! write(6,*) 'header: ', header
!
! read monthy average of daily minimum temperature
!
  read (luicli,*) (awtmnav(idx),idx=1,12)
! write(6,*)  (awtmnav(idx), idx = 1,12)
!
! find yearly average temperature
!
  awtyav = 0.0
!
  do idx = 1,12
!   average temperature is mean of maximum and minimum
     awtmav(idx) = (awtmnav(idx)+awtmxav(idx))/2.0
     awtyav = awtyav + awtmav(idx)
  end do
!
  awtyav = awtyav/12.0
!
! read three lines to get to precipitation values
!
  do idx = 1,3
     read (luicli,'(a128)') header
!   write(6,*) 'header: ', header,':'
  end do
!
! read average monthy total precipitation
!
  read (luicli,*) (awzmpt(idx),idx=1,12)
! write(6,*)  (awzmpt(idx), idx = 1,12)
!
  rewind luicli
!
else   ! read in historical data
!debe add reading in climate file name
  read (upgmcli,*) cliname
!
! read monthy average of daily maximum temperature
!
  read (upgmcli,*) (awtmxav(idx),idx=1,12)
!
! read monthy average of daily minimum temperature
!
  read (upgmcli,*) (awtmnav(idx),idx=1,12)
!
! find yearly average temperature
!
  awtyav = 0.0
!
  do idx = 1,12
!   average temperature is mean of maximum and minimum
     awtmav(idx) = (awtmnav(idx)+awtmxav(idx))/2.0
     awtyav = awtyav + awtmav(idx)
  end do
!
  awtyav = awtyav/12.0
!
! read monthy average of daily minimum temperature
!debe i think the above comment should read: "read average monthly
!       total precipitation"
!
  read (upgmcli,*) (awzmpt(idx),idx=1,12)
!
end if
!
end subroutine climate_init
