program trapeziums_area
  double precision, dimension(6) :: a, b, c, d
  double precision, dimension(6) :: areas
  integer :: i
  
  ! Open the file for reading
  open(unit=5, file='trapezium_measures.dat')

  do i = 1, 6
    read(5, *) a(i), b(i), c(i), d(i)
  end do

  ! Close the file
  close(unit=5)

  ! Call the subroutine to calculate the areas
  do i = 1, 6
    call calculate_area(a(i), b(i), c(i), d(i), areas(i))
    write(*, *) "Area of trapezium ", i, " is ", areas(i)
  end do

  contains
  
  ! Subroutine to calculate area of one trapezium
  subroutine calculate_area(a, b, c, d, area)
    double precision , intent(in) :: a, b, c, d 
    double precision , intent(out) :: area
    double precision :: p
    
    p = (a+b+c+d)/2
    area = ((b+d)/(b-d)) * sqrt(abs((p-b)*(p-d)*(p-a-d)*(p-c-d)))
  end subroutine calculate_area

end program trapeziums_area