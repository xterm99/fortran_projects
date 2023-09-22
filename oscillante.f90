program ReadSubmatrix
  implicit none

  integer, parameter :: n = 5  ! Size of the submatrix
  real :: submatrix(n, n)     ! Declare the submatrix

  integer :: i, j  ! File unit number (can be any valid unit number)

  ! Open the file for reading
  open(10, file='matrix.txt', action='read')

  ! Read the 5x5 submatrix from columns 1 to 5 and rows 1 to 5
  do i = 1, n
    read(10, *) (submatrix(i, j), j = 1, n)
  end do

  ! Close the file
  close(10)

  ! Now you have the 5x5 submatrix in the 'submatrix' array, and you can work with it as needed

  ! Print the submatrix for demonstration
  do i = 1, n
    write(*, '(5(f8.4, 2x))') (submatrix(i, j), j = 1, n)
  end do

end program ReadSubmatrix
