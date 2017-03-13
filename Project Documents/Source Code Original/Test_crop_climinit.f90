SUBROUTINE test_crop_climinit(icli,cliname)
!
IMPLICIT NONE
!
INCLUDE 'w1clig.inc'
INCLUDE 'file.fi'
!
! Subroutine arguments
!
CHARACTER(128) :: cliname
INTEGER :: icli
!
! Local variables
!
CHARACTER(128) :: header
INTEGER :: idx
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
IF (icli.EQ.1) THEN  ! read from cligen data file
!
  REWIND luicli
!debe add reading in the location climate name
  READ (luicli,'(a128)') cliname
!
  DO idx = 1,6          ! skip first six lines
     READ (luicli,'(a128)') header
!   write(6,*) 'header: ', header,':'
  END DO
!
! read monthy average of daily maximum temperature
!
  READ (luicli,*) (awtmxav(idx),idx=1,12)
! write(6,*)  (awtmxav(idx), idx = 1,12)
!
  READ (luicli,'(a128)') header
! write(6,*) 'header: ', header
!
! read monthy average of daily minimum temperature
!
  READ (luicli,*) (awtmnav(idx),idx=1,12)
! write(6,*)  (awtmnav(idx), idx = 1,12)
!
! find yearly average temperature
!
  awtyav = 0.0
!
  DO idx = 1,12
!   average temperature is mean of maximum and minimum
     awtmav(idx) = (awtmnav(idx)+awtmxav(idx))/2.0
     awtyav = awtyav + awtmav(idx)
  END DO
!
  awtyav = awtyav/12.0
!
! read three lines to get to precipitation values
!
  DO idx = 1,3
     READ (luicli,'(a128)') header
!   write(6,*) 'header: ', header,':'
  END DO
!
! read average monthy total precipitation
!
  READ (luicli,*) (awzmpt(idx),idx=1,12)
! write(6,*)  (awzmpt(idx), idx = 1,12)
!
  REWIND luicli
!
ELSE   ! read in historical data
!debe add reading in climate file name
  READ (7,*) cliname
!
! read monthy average of daily maximum temperature
!
  READ (7,*) (awtmxav(idx),idx=1,12)
!
! read monthy average of daily minimum temperature
!
  READ (7,*) (awtmnav(idx),idx=1,12)
!
! find yearly average temperature
!
  awtyav = 0.0
!
  DO idx = 1,12
!   average temperature is mean of maximum and minimum
     awtmav(idx) = (awtmnav(idx)+awtmxav(idx))/2.0
     awtyav = awtyav + awtmav(idx)
  END DO
!
  awtyav = awtyav/12.0
!
! read monthy average of daily minimum temperature
!debe i think the above comment should read: "read average monthly
!       total precipitation"
!
  READ (7,*) (awzmpt(idx),idx=1,12)
!
END IF
!
END SUBROUTINE test_crop_climinit
