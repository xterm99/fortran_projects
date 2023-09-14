module ExtractDataModule
  implicit none

contains

  subroutine NumberReader(filename, i, rowArray, num_digits)
    implicit none
    character(*), intent(in) :: filename
    integer, intent(in) :: i
    integer, intent(out) :: num_digits
    integer, dimension(:), allocatable, intent(out) :: rowArray
    integer, parameter :: max_digits = 100

    character(max_digits) :: line
    integer :: unit_number, j

    ! Open the file
    open(unit=unit_number, file=filename, status='old', action='read')

    ! Read and process the i-th row from the file
    do j = 1, i
      read(unit_number, '(A)') line
    end do

    ! Determine the number of digits in the row
    num_digits = len_trim(line)

    ! Allocate the array with the appropriate size
    allocate(rowArray(num_digits))

    ! Convert the row string to an array of digits
    do j = 1, num_digits
      rowArray(j) = ichar(line(j:j)) - ichar('0')
    end do

    ! Close the file
    close(unit_number)

  end subroutine NumberReader

  subroutine ReverseArray(arr_in, n, arr_out)
    implicit none
    integer, intent(in) :: arr_in(:)    ! Input array to be reversed
    integer, intent(in) :: n            ! Number of elements in the array
    integer, intent(out) :: arr_out(n)  ! Output array to store the reversed result
    integer :: i

    do i = 1, n
      arr_out(i) = arr_in(n - i + 1)
    end do
  end subroutine ReverseArray

end module ExtractDataModule

module Tcalculations
  implicit none

contains

  ! Private function to calculate T_i.
  function T_at(inputArray, at) result(res)
    implicit none
    integer, dimension(:), intent(in) :: inputArray
    integer, intent(in) :: at
    integer :: bound
    integer :: res

    ! Check we don't go out of bounds
    bound = at + 32
    if (bound > size(inputArray)) then
      res = inputArray(at) + inputArray(at+16)
    else
      res = inputArray(at) + inputArray(at+16) + inputArray(at+32)
    endif
  end function T_at

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

  integer, dimension(:), allocatable :: rowArray, reversedArray  ! Adjust the size as needed
  integer :: num_digits, j
  integer :: result_T, rT, rN
  integer :: i

  ! Loop through lines 1 to 10 in numbers.dat
  do i = 1, 10
    ! Call the NumberReader subroutine to read and store the i-th row
    call NumberReader('numbers.dat', i, rowArray, num_digits)

    ! Call the ReverseArray subroutine to reverse the rowArray
    allocate(reversedArray(num_digits))
    call ReverseArray(rowArray, num_digits, reversedArray)

    ! Call the T function to calculate the result_T
    result_T = T(reversedArray)

    ! Calculate the remainder of result_T when divided by 17
    rT = modulo(result_T, 17)

    ! Calculate the remainder of rowArray when divided by 17
    rN = RemainderBy17(rowArray)

    ! Print the results for the i-th line
    print *, "Line", i, "Result T:", result_T
    print *, "Line", i, "Remainder of Result T by 17:", rT
    print *, "Line", i, "Remainder of rowArray by 17:", rN

    ! Deallocate arrays for the current line
    deallocate(rowArray)
    deallocate(reversedArray)
  end do

end program DivisibilityBy17
