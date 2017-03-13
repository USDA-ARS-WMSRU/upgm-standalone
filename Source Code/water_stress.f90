subroutine water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!
! this subroutine takes the input water stress factor and based on the
! default gdd for stressed (wsf = 0) and non-stressed (wsf = 1)conditions
! calculates the number of gdd for each growth stage of the current crop.
 
! use the current day's water stress factor to adjust the gdd required
! to reach the next growth stage
 
! debe need to pass the gddwsf array so that phenol cropname has access to
! it.
!
implicit none
!
! Dummy arguments
!
real :: adjgdd,bhfwsf
integer :: i,row
real,dimension(30) :: dummy2
real,dimension(15,5) :: gddwsf
!
! Local variables
!
integer :: j,n
real :: m,wsfhi,wsflo,x,y
!
! gddwsf is an array to hold the gdd values for gn and gs which are read in
! from upgm_crop.dat. gddwsf also holds the two wsf values for each growth
! stage.
! debe added a fifth column in gddwsf to hold the adjgdd value for each growth
! stage.
! debe added the variable (adjgdd) to hold the adjusted gdd value for entering
! a growth stage.
!
!     + + + argument definitions + + +
!     adjgdd - the adjusted gdd required to enter the growth stage.
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     dummy2 - an array to hold the gdd values, both under stressed
!     and non- stressed conditions,required to reach each growth stage of
!     the current crop.
!     gddwsf - an array to hold the gn and gs gdd values plus the
!     high and low water stress factor values.  these are used in
!     caluclating the slope of the line for each growth stage and this
!     is then used to calculate the adjusted gdd value for the current
!     growth stage.
!     column one contains the gn values and is y2.
!     column two contains the gs value and is y1.
!     column three contains wsfhi (high water stress) and is x1.
!     column four contains wsflo (low water stress) and is x2.
!     column five contains the adjgdd value for the stage.
!     i - variable used to control which row in dummy 2 is used. it is passed
!       in from the calling subroutine.
!     row - selects the row to be filled in the gddwsf array.
 
!     + + + local variable definitions + + +
!     j - selects the appropriate column in the selected row in gddwsf array.
!     m - the slope of the line.  the change in y over the change in x.
!     n - used to skip down to the gs values in dummy2 array.
!     wsfhi - a high water stress factor value, eg. .4  0 = complete stress
!     wsflo - a low water stress factor value, eg. .8   1 = no stress
!     x - the x value that is filled with the current water stress factor (bhfwsf)
!       in the point slope equation for the line
!     y - the y value when x is known in the point slope equation for the
!       line. this holds the new adjusted gdd value and is saved in the
!       variable adjgdd.
!
! set parameter values.  currently hardwiring the same value for all stages.
 
wsfhi = 0.4
wsflo = 0.8
!
! initialize counter variables
j = 0
m = 0.0
n = 15
!
! initialize/set local variables
x = bhfwsf
y = 0.0
 
! fill the gddwsf arary:
! fill the first column in gddwsf array with the gn gdd values
!debe  made changes to filling gddwsf array so that the correct
! rows in dummy2 are used for calculations. rows are skipped in
! gddwsf that don't correspond to the row values in dummy2.
j = 1     ! column 1
gddwsf(row,j) = dummy2(i)
! print*, 'in water_stress dummy2(5) = ', dummy2(5), 'dummy2(12) = ', dummy2(12)
j = 2     !column 2
gddwsf(row,j) = dummy2(i+n)
 
j = 3     !column 3
gddwsf(row,j) = wsfhi     ! currently 0.4
 
j = 4     ! column 4
gddwsf(row,j) = wsflo     ! currently 0.8
 
!debe moved the following section here to take care of leaving fps stage at
! 0.0 in gddwsf array.
!debe need to take care of wsf values that are not between wsflo and wsfhi,
! e.g. wsf = 1.0.
if (x>wsflo) then      ! wsflo = 0.8
  gddwsf(row,5) = dummy2(i)   ! = to the gn values
else if (x<wsfhi) then     ! wsfhi = 0.4
  gddwsf(row,5) = dummy2(i+n)     ! = to the gs values
!debe x is between lo and hi wsf values
else if (dummy2(i)==dummy2(i+n)) then      !gn and gs value is the same
  adjgdd = dummy2(i)
  gddwsf(row,5) = adjgdd
else if (dummy2(i)>dummy2(i+n)) then       ! gn value is > gs value, a + slope
   !code a for equation
   !calculate the slope (m) of the line.  maybe later use a function below to
   ! do this.
 
        !m = y2-y1/x2-x1
  m = ((gddwsf(row,1)-gddwsf(row,2))/(gddwsf(row,4)-gddwsf(row,3)))
  y = gddwsf(row,2) + m*x - m*gddwsf(row,3)   !y = y1 + mx - mx1
  adjgdd = y
  gddwsf(row,5) = adjgdd
else if (dummy2(i)<dummy2(i+n)) then        !gn value is < gs value, a - slope
   !code b for equation
  m = ((gddwsf(row,1)-gddwsf(row,2))/(gddwsf(row,4)-gddwsf(row,3)))
  y = gddwsf(row,2) + m*x - m*gddwsf(row,3)
  adjgdd = y
  gddwsf(row,5) = adjgdd
end if
!
end subroutine water_stress
