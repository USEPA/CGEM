subroutine error (message, ierr)

implicit none

character, dimension(100), intent(in) :: message
integer, intent(in) :: ierr

write(6,*) "Error in ",message, "code=",ierr, ". Stopping."
stop

end subroutine
