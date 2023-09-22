module ExtractDataModule
  implicit none

contains

  !-----
  !- Lee un número desde un archivo y lo convierte en un array de dígitos.
  !-----
  subroutine NumberReader(filename, i, rowArray, num_digits)
    implicit none
    character(*), intent(in) :: filename
    integer, intent(in) :: i
    integer, intent(out) :: num_digits
    integer, dimension(:), allocatable, intent(out) :: rowArray
    integer, parameter :: max_digits = 100

    character(max_digits) :: line
    integer :: unit_number, j

    ! Abre el archivo
    open(unit=unit_number, file=filename, status='old', action='read')

    ! Lee y procesa la fila i-ésima del archivo
    do j = 1, i
      read(unit_number, '(A)') line
    end do

    ! Determina el número de dígitos en la fila
    num_digits = len_trim(line)

    ! Asigna el tamaño adecuado al array
    allocate(rowArray(num_digits))

    ! Convierte la cadena de la fila en un array de dígitos
    do j = 1, num_digits
      rowArray(j) = ichar(line(j:j)) - ichar('0')
    end do

    ! Cierra el archivo
    close(unit_number)

  end subroutine NumberReader

  !-----
  !- Invierte un array de enteros.
  !-----
  subroutine ReverseArray(arr_in, n, arr_out)
    implicit none
    integer, intent(in) :: arr_in(:)    ! Array de entrada a invertir
    integer, intent(in) :: n            ! Número de elementos en el array
    integer, intent(out) :: arr_out(n)  ! Array de salida para almacenar el resultado invertido
    integer :: i

    do i = 1, n
      arr_out(i) = arr_in(n - i + 1)
    end do
  end subroutine ReverseArray

end module ExtractDataModule

module Tcalculations
  implicit none

contains

  !-----
  !- Función privada para calcular T_i.
  !-----
  function T_at(inputArray, at) result(res)
    implicit none
    integer, dimension(:), intent(in) :: inputArray
    integer, intent(in) :: at
    integer :: res
    integer :: j

    res = 0
    j = at

    do while (j <= size(inputArray))
      res = res + inputArray(j)
      j = j + 16
    end do

  end function T_at

  !-----
  !- Calcula el valor de T.
  !-----
  function T(inputArray) result(res)
    implicit none
    integer, intent(in) :: inputArray(:)
    integer :: res

    res = T_at(inputArray, 1) - T_at(inputArray, 9) &
    + 7 * (T_at(inputArray, 10) - T_at(inputArray, 2)) &
    + 2 * (T_at(inputArray, 11) - T_at(inputArray, 3)) &
    + 3 * (T_at(inputArray, 12) - T_at(inputArray, 4)) &
    + 4 * (T_at(inputArray, 5) - T_at(inputArray, 13)) &
    + 6 * (T_at(inputArray, 6) - T_at(inputArray, 14)) &
    + 8 * (T_at(inputArray, 15) - T_at(inputArray, 7)) &
    + 5 * (T_at(inputArray, 8) - T_at(inputArray, 16))

  end function T

  !-----
  !- Calcula el resto de la división por 17.
  !-----
  function RemainderBy17(inputArray) result(remainder)
    implicit none
    integer, intent(in) :: inputArray(:)
    integer :: remainder
    integer :: i, currentRemainder

    currentRemainder = 0

    do i = 1, size(inputArray)
      currentRemainder = MOD(currentRemainder * 10 + inputArray(i), 17)
    end do
    remainder = currentRemainder
  end function RemainderBy17

end module Tcalculations

program DivisibilityBy17
  use ExtractDataModule
  use Tcalculations
  implicit none

  integer, dimension(:), allocatable :: rowArray, reversedArray  ! Ajustar el tamaño según sea necesario
  integer :: num_digits, j
  integer :: result_T, rT, rN
  integer :: i

  ! Itera a través de las líneas 1 a 10 en numbers.dat
  do i = 1, 10
    ! Llama a la subrutina NumberReader para leer y almacenar la fila i-ésima
    call NumberReader('numbers.dat', i, rowArray, num_digits)

    ! Llama a la subrutina ReverseArray para invertir rowArray
    allocate(reversedArray(num_digits))
    call ReverseArray(rowArray, num_digits, reversedArray)

    ! Llama a la función T para calcular result_T
    result_T = T(reversedArray)

    ! Calcula el resto de result_T cuando se divide por 17
    rT = modulo(result_T, 17)

    ! Calcula el resto de rowArray cuando se divide por 17
    rN = RemainderBy17(rowArray)

    ! Imprime los resultados para la línea i-ésima
    print *, "Numero", i, "T: ", result_T
    print *, "Numero", i, "rT: ", rT
    print *, "Numero", i, "rN: ", rN

    ! Desasigna los arrays para la línea actual
    deallocate(rowArray)
    deallocate(reversedArray)
  end do

end program DivisibilityBy17
