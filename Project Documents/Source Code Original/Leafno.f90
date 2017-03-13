SUBROUTINE leafno(daynum,gdde,lnarray,lncntr,lnpout,pchron,rowcntr,todayln,     &
                & yestln)
!                
! the leafno subroutine is taken from phenologymms.  it calculates the
! number of leaves per day. 
!
! debe rowcntr, todayln, yestln are used to print leaf number when the
! integer value has incremented.
! 
IMPLICIT NONE
!
! Subroutine arguments
!
INTEGER :: daynum,lncntr,rowcntr
REAL :: gdde,pchron,todayln,yestln
REAL,DIMENSION(400,2) :: lnarray
REAL,DIMENSION(100,2) :: lnpout
!
! Local variables
!
INTEGER :: col,nextcol
INTEGER :: int
REAL :: ln
!
! debe changed dimensions of lnpout to (100,2) from (60,2)
! local variables
 
!     + + + argument definitions + + +
!     daynum - the integer version of day of year
!     gdde - the accumulated growing degree-days since emergence
!     lnarray - an array to hold the leaf number calculated for each day
!     lncntr - counter for the leafno subroutine
!     lnpout - an array to hold the leaf number when it changes to the
!              next whole number, i.e. the interger version of the real leaf
!              number
!     pchron - the number of growing degree-days required to produce another
!              leaf
!     rowcntr - a counter for the rows in an array
!     todayln - the value of the current day's leaf number
!     yestln - the value of yesterday's leaf number
 
!     + + + local variable definitions + + +
!     col - column number 1 in the arrays
!     ln - leaf number
!     nextcol - column number 2 in the arrays
! 
! initialize variables
ln = 0.0           ! leaf number
col = 1            ! column number one
nextcol = 2        ! column number two
 
! fill arrays
ln = gdde/pchron          ! calculate leaf number for current day
lncntr = lncntr + 1         ! increment counter
lnarray(lncntr,col) = daynum          ! fill leaf number array
lnarray(lncntr,nextcol) = ln
 
 !set todayln to the value in the lnarray for today.
todayln = lnarray(lncntr,nextcol)
 
 ! fill leaf output array when the integer version of yesterday's leaf
 ! number is less than the integer version of today's leaf number
 
IF ((int(yestln)).LT.(int(todayln))) THEN
  IF (rowcntr.LT.101) THEN
     lnpout(rowcntr,col) = daynum
     lnpout(rowcntr,nextcol) = ln
  END IF
  rowcntr = rowcntr + 1
END IF
! 
yestln = lnarray(lncntr,nextcol)
! 
  !  print *, 'daynum = ', daynum, 'array counter = ', lncntr
END SUBROUTINE leafno
