SUBROUTINE fopenk(filnum,filnam,filsta)
!
IMPLICIT NONE
!
! Subroutine arguments
!
CHARACTER(*) :: filnam,filsta
INTEGER :: filnum
!
! Local variables
!
INTEGER :: ios
!
! ****************************************************************** wjr
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
OPEN (filnum,FILE=filnam(1:len_trim(filnam)),STATUS=filsta,ERR=10,IOSTAT=ios)
WRITE (*,1000) filnam(1:len_trim(filnam)),filnum,filsta
 
RETURN
!
 10   WRITE (0,1100) filnam(1:len_trim(filnam)),filnum,filsta,ios
CALL exit(1)
 1000 FORMAT (' opened file: ',a,' on unit ',i3,' with status ',a)
! *** 1000  format('i3  a  a')
 1100 FORMAT (' cannot open file: ',a,' on unit ',i3,' with status ',a,         &
             &' and i/o status ',i5)
!             
END SUBROUTINE fopenk
