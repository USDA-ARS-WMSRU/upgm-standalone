SUBROUTINE date1(gsarray)
!
!  debe added from phenologymms as a starting point for adding emergence
!  to upgm.
 
!  the date1 subroutine receives an array with daynum and year values.
!  date1 first determines if the year is a leap year. if it is it changes
!  the value of the number of days for february and the total number of days
!  of the year to those appropriate for a leap year. date1 then fills the
!  other array positions with the month and day for the daynum in
!  position 1 of the array.
 
!  check inputs and outputs!!!!!!!!
 
!  inputs:  list variables here with c and r for each variable
 
!  outputs: list variables here with c and r for each variable 
!
IMPLICIT NONE
!
! Subroutine arguments
!
INTEGER,DIMENSION(4) :: gsarray
!
! Local variables
!
INTEGER :: month,ndayyr,year
INTEGER,DIMENSION(12) :: ndays
!
!     + + + argument definitions + + +
!     gsarray - this holds the date values passed into date1.  it is an
!               array and contains daynum, and the year, month and day for
!               daynum. date1 determines the month and day based on
!               daynum and year.
 
!     + + + local variable definitions + + +
!     month - the month that daynum occurs in.
!     ndays - an array that holds the number of days in each month.  the
!             value for february changes if date1 determines it is a leap
!             year.
!     ndayyr - this holds the number of days for year passed to date1.  if
!              date1 determines it is a leap year the ndayyr value is
!              changed to 366. this variable is not really 'used' it is
!              only given a value based on whether it is a leap year or
!              not. left it in, in case it is needed in the future.
!     year - this is the year value.  it can be 1, 2, etc. or the actual
!            calendar year.
 
!  initialize variables
 !year = 1 this caused problems with winter crops when year needed to be 2
 !     print *, 'gsarray(2)in date1=', gsarray(2)
year = gsarray(2)       !this is the local variable 'year'
!      print *, 'year after initializing in date1=', year
 
! initialize ndays array to hold the days of the months
ndays(1) = 31
ndays(2) = 28
ndays(3) = 31
ndays(4) = 30
ndays(5) = 31
ndays(6) = 30
ndays(7) = 31
ndays(8) = 31
ndays(9) = 30
ndays(10) = 31
ndays(11) = 30
ndays(12) = 31
!  check if year is a leap year
IF (mod(year,4).EQ.0) THEN         !this is a leap year
  ndays(2) = 29
  ndayyr = 366
ELSE
  ndays(2) = 28         ! not a leap year
  ndayyr = 365
END IF
! 
!     subtract the number of days in each month until the day is within
!     the month.  this will give the day and month.
gsarray(4) = gsarray(1)
DO month = 1,12
  IF (gsarray(4).LE.ndays(month)) GO TO 10
  gsarray(4) = gsarray(4) - ndays(month)
 
END DO
! 
 10   gsarray(3) = month
! 
END SUBROUTINE date1
