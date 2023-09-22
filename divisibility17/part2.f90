program Calcular_Ti
  implicit none
  integer, parameter :: c = 900578
  double precision :: p = 2.04582859d0
  double precision :: q = 5.94467390d0
  integer, dimension(c + 1) :: N
  integer :: i, Ti
  integer :: S
  integer :: T, rN, rT

  ! Calcular N con c + 1 valores y almacenarlos en orden inverso
  do i = c, 0, -1
    N(c - i + 1) = p * (COS(q * DBLE(i) / (DBLE(c) + q)) ** 2) + q * (SIN(17 * q * q * DBLE(i) / (DBLE(c) - p)) ** 2)
  end do

  ! Calcular los valores de Ti para i = 0 a 15
  do i = 0, 15
    Ti = T_at(N, i)
    print *, 'T', i, ' = ', Ti
  end do

  ! Calcular la suma S de N
  S = SUM(N)
  
  ! Calcular T
  T = T_at(N, 0) ! T con i = 0
  
  ! Calcular rN
  rN = RemainderBy17(N)
  
  ! Calcular rT
  rT = modulo(T, 17)
  
  print *, 'S = ', S
  print *, 'T = ', T
  print *, 'rN = ', rN
  print *, 'rT = ', rT

contains

  ! Función privada para calcular T_i.
  function T_at(inputArray, at) result(res)
    integer, dimension(:), intent(in) :: inputArray
    integer, intent(in) :: at
    integer :: res
    integer :: j

    res = 0
    j = at + 1

    do while (j <= c+1)
      res = res + inputArray(j)
      j = j + 16  ! Agregar 16 para obtener la siguiente posición
    end do

  end function T_at

  ! Función privada para calcular el resto de la división por 17.
  function RemainderBy17(inputArray) result(remainder)
    integer, dimension(:), intent(in) :: inputArray
    integer :: remainder
    integer :: i, currentRemainder

    currentRemainder = 0

    do i = 1, size(inputArray)
      currentRemainder = MOD(currentRemainder * 10 + inputArray(i), 17)
    end do
    remainder = currentRemainder
  end function RemainderBy17

end program Calcular_Ti
