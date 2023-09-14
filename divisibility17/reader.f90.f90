! Subroutine to read the numbers and store it in a matrix.
! The coordinates (i, j) correspond to number i, digit j.
subroutine NumbearsReaderModule(filename, digit_arrays)
  implicit none
  
  character(*), intent(in) :: filename
  integer, dimension(:,:), intent(out) :: digit_arrays
  integer, parameter :: num_numbers = 10
  integer, parameter :: max_digits = 100

  character(max_digits), dimension(num_numbers) :: number_strings
  integer :: i, j, num_digits

  ! Open the file
  open(unit=10, file=filename, status='old', action='read')

  ! Read and process numbers from the file
  do i = 1, num_numbers
    read(10, '(A)') number_strings(i)

    ! Determine the number of digits in the string
    num_digits = len_trim(number_strings(i))

    ! Convert the string to an array of digits
    do j = 1, num_digits
      digit_arrays(i, j) = ichar(number_strings(i)(j:j)) - ichar('0')
    end do
  end do

  ! Close the file
  close(10)

end subroutine NumbearsReaderModule

! Private function to calculate T_i.
function T_at(inputArray, at) result(res)
  implicit none
  integer, dimension(:), intent(in) :: inputArray
  integer, intent(in) :: at
  integer :: bound, res
  
  ! Check we don't go out of bounds
  ! Have to generalize for bigger numbers
  bound = at + 32
  if (bound > size(inputArray)) then
    res = inputArray(at) + inputArray(at+16)
  else
    res = inputArray(at) + inputArray(at+16) + inputArray(at+32)
  endif
end function T_at

! Function to calculate the value T of a number
function T(inputArray) result(T_result)
  implicit none
  integer, intent(in) :: inputArray(:)
  integer :: T_result
  
  T_result = T_at(inputArray, 0) - T_at(inputArray, 8) &
  + 7 * (T_at(inputArray, 9) - T_at(inputArray, 1)) & 
  + 2 * (T_at(inputArray, 10) - T_at(inputArray, 2)) & 
  + 3 * (T_at(inputArray, 11) - T_at(inputArray, 3)) & 
  + 4 * (T_at(inputArray, 4) - T_at(inputArray, 12)) & 
  + 6 * (T_at(inputArray, 5) - T_at(inputArray, 13)) & 
  + 8 * (T_at(inputArray, 14) - T_at(inputArray, 6)) & 
  + 5 * (T_at(inputArray, 7) - T_at(inputArray, 15)) 

  
end function T


subroutine divide(dividend, divisor, quotient, remainder)
  implicit none
  integer, dimension(:), intent(in) :: dividend
  integer, intent(in) :: divisor
  integer, dimension(:), allocatable, intent(out) :: quotient, remainder
  integer :: n, i
  integer :: temp, carry

  n = size(dividend)

  ! Initialize quotient and remainder arrays
  allocate(quotient(n), remainder(1))

  ! Initialize the remainder with the dividend
  remainder = dividend

  ! Initialize carry
  carry = 0

  ! Loop through each digit of the quotient
  do i = 1, n
    quotient(i) = (remainder(i) + carry) / divisor
    carry = (remainder(i) + carry) - quotient(i) * divisor
  end do

  ! Remove leading zeros from the quotient
  do i = n, 1, -1
    if (quotient(i) /= 0) exit
  end do

end subroutine divide


