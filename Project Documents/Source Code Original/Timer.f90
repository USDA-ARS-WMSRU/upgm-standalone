SUBROUTINE timer(timnum,timact)
!
IMPLICIT NONE
!
INCLUDE 'timer.fi'
!
! Subroutine arguments
!
INTEGER :: timact,timnum
!
! Local variables
!
INTEGER :: idx
REAL :: tim
REAL,DIMENSION(0:11) :: timarr
!
! ****************************************************************** wjr
!
! provides benchmark data
!
!       edit history
!       05-feb-99       wjr     original coding
!
!
                                                !# of timer being used
!
!     integer     lsttim
 
 
DATA timarr/12*0.0/                             !action: 1==start, 2==end, 3==print, 4==reset
!     data lsttim / 0 /
 
 
CALL cpu_time(tim)
 
SELECT CASE (timact)
CASE (1)                                                                        ! start a timer
  IF (timnum.NE.timweps) timarr(0) = timarr(0) + tim
  timarr(timnum) = timarr(timnum) - tim
CASE (2)                                                                        ! stop a timer
  IF (timnum.NE.timweps) timarr(0) = timarr(0) - tim
  timarr(timnum) = timarr(timnum) + tim
CASE (3)
  timarr(0) = timarr(0) + tim
  DO idx = 0,11
     WRITE (*,FMT='('' '',a8,2x,f8.2)') timnam(idx),timarr(idx)
  END DO
  timarr(0) = timarr(0) - tim
CASE (4)
  timarr(timnum) = 0.0
END SELECT
!
END SUBROUTINE timer
