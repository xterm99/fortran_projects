program area_de_trapecios
  double precision, dimension(6) :: a, b, c, d
  double precision, dimension(6) :: areas
  integer :: i
  
  ! Abre el archivo para lectura
  open(unit=5, file='trapezium_measures.dat')

  do i = 1, 6
    read(5, *) a(i), b(i), c(i), d(i)
  end do

  ! Cierra el archivo
  close(unit=5)

  ! Llama a la subrutina para calcular las áreas
  do i = 1, 6
    call calcular_area(a(i), b(i), c(i), d(i), areas(i))
    write(*, *) "El área del trapecio ", i, " es ", areas(i)
  end do

  contains
  
  ! Subrutina para calcular el área de un trapecio
  subroutine calcular_area(a, b, c, d, area)
    double precision , intent(in) :: a, b, c, d 
    double precision , intent(out) :: area
    double precision :: p
    
    p = (a+b+c+d)/2
    area = ((b+d)/(b-d)) * sqrt(abs((p-b)*(p-d)*(p-a-d)*(p-c-d)))
  end subroutine calcular_area

end program area_de_trapecios
