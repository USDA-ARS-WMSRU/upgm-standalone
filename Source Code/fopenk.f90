subroutine fopenk(filnum,filnam,filsta)
!
implicit none
!
! Dummy arguments
!
character(*) :: filnam,filsta
integer :: filnum
!
! Local variables
!
integer :: ios
!
! provides error trapped opening of files
!
!       edit history
!       05-feb-99       wjr     original coding
!
!      include 'file.fi'
!
!
! debe 0809 assumed the following definitions
!     + + + argument definitions + + +
!     filnam - name of the file to be opened
!     filnum - number of the file to be opened
!     filsta - status of the file to be opened
 
!     + + + local variable definitions + + +
!     ios - i/o status
 
! ***      write(*,1991) filnum, filnam,filsta
! *** 1991    format('in copenk', i3,a,a)
open (filnum,file=filnam(1:len_trim(filnam)),status=filsta,err=10,iostat=ios)
write (*,1000) filnam(1:len_trim(filnam)),filnum,filsta
 
return
!
 10   write (0,1100) filnam(1:len_trim(filnam)),filnum,filsta,ios
call exit(1)
 1000 format (' opened file: ',a,' on unit ',i3,' with status ',a)
! *** 1000  format('i3  a  a')
 1100 format (' cannot open file: ',a,' on unit ',i3,' with status ',a,         &
             &' and i/o status ',i5)
!
end subroutine fopenk
