subroutine leafno(antss,boots,cname,daynum,dgdde,endlgs,epods,eseeds,gdde,ln,lnarray,lncntr,lnpout,     &
                  & pchron,rowcntr,todayln,yestln)

! the leafno subroutine is taken from phenologymms.  it calculates the
! number of leaves per day.
!
! debe rowcntr, todayln, yestln are used to print leaf number when the
! integer value has incremented.
!
implicit none
!
! Dummy arguments
!
character(80) :: cname 
integer :: daynum,lncntr,rowcntr
real :: gdde,pchron,todayln,yestln
real :: ln
integer,dimension(4) :: antss,boots,endlgs,epods,eseeds
real,dimension(20) :: dgdde
real,dimension(400,2) :: lnarray
real,dimension(100,2) :: lnpout
!
! Local variables
!
integer :: col,nextcol
!
!
! debe changed dimensions of lnpout to (100,2) from (60,2)
! local variables
 
!     + + + argument definitions + + +
!     antss - the start of anthesis growth stage. this array
!             includes daynum, year, month and day of when this stage was
!             reached.
!     boots - booting growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.  booting is
!            defined as flag leaf has completed its growth.
!     cname - crop name.
!     daynum - the integer version of day of year
!     dgdde - an array holding the gdde for each growth stage.
!     endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     epods - one pod has reached the maximum length in dry beans (early
!             pod set). in soybean, beginning pod and is when one pod is 
!             3/16" long at one of the four uppermost nodes. this array 
!             includes daynum,year, month and day of when this stage was reached.
!     eseeds - there is one pod with fully developed seeds in dry
!              beans (early seed fill). in soybean, beginning seed and seed 
!              is 1/8" long in the pod at one of the four uppermost nodes.
!              this array includes daynum, year, month and day of when 
!              this stage was reached.
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
if (lncntr < 400) then   
    !print *, 'ln = ', ln, 'lncntr = ', lncntr, 'col = ', col, 'daynum = ', &
    !    & daynum
   ! if (lncntr < 400)
       lncntr = lncntr + 1         ! increment counter
   ! end if   
     lnarray(lncntr,col) = daynum ! fill leaf number array
end if

lnarray(lncntr,nextcol) = ln  
    
 !set todayln to the value in the lnarray for today.
todayln = lnarray(lncntr,nextcol)

 ! fill leaf output array when the integer version of yesterday's leaf
 ! number is less than the integer version of today's leaf number. This
 ! array will fill with values past end of leaf growth stage.
 
if ((int(yestln))<(int(todayln))) then
  if (rowcntr<101) then
     lnpout(rowcntr,col) = daynum
     lnpout(rowcntr,nextcol) = ln
  end if
  rowcntr = rowcntr + 1
end if
!
yestln = lnarray(lncntr,nextcol)

!The following code is to facilitate printing out the daily leaf number which 
!stops incrementing at the end of the leaf growth stage. The same leaf number 
!is then printed until the end of simulation.
!It was decided to put this code here as the leaf number variables are here
!and it is the logical place to look in the future to find the code for printing
!leaf numbers to the maximum leaf number at the end of leaf growth and then
!continue printing that same value until the end of simulation.
!debe added eseeds to allow determining number of leaves for soybean

if ((cname == 'hay millet') .AND. (boots(1) /= 999)) then
    ln = dgdde(9)/pchron
else if ((cname == 'proso millet') .AND. (boots(1) /= 999)) then 
    ln = dgdde(9)/pchron
else if ((cname == 'spring barley') .AND. (boots(1) /= 999)) then
    ln = dgdde(9)/pchron
else if ((cname == 'spring wheat') .AND. (boots(1) /= 999)) then
    ln = dgdde(9)/pchron
else if ((cname == 'winter barley') .AND. (boots(1) /= 999)) then
    ln = dgdde(9)/pchron
else if ((cname == 'winter wheat') .AND. (boots(1) /= 999)) then     
    ln = dgdde(9)/pchron   
else if ((cname == 'corn') .AND. (antss(1) /= 999)) then
    ln = dgdde(7)/pchron    
else if ((cname == 'sunflower') .AND. (antss(1) /= 999)) then
    ln = dgdde(9)/pchron
else if ((cname == 'dry beans') .AND. (epods(1) /= 999)) then
    ln = dgdde(9)/pchron
else if ((cname == 'sorghum') .AND. (endlgs(1) /= 999)) then
    ln = dgdde(6)/pchron
else if ((cname == 'soybean') .AND. (eseeds(1) /= 999)) then
    ln = dgdde(12)/pchron

end if

end subroutine leafno
