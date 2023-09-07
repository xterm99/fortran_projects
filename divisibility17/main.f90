program divisibility17
  implicit none
  double precision, dimension(10) :: numbers
  integer :: i
  double precision :: length
  
  ! Open the file for reading
  open(unit=5, file='numbers.txt')
  
  ! Store each numbers in an array for simpler manipulation
  do i= 1, 10
    read(5, *) numbers(i)
  end do
  
 
end program 
