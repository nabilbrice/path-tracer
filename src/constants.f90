module constants_mod
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  real(dp), parameter :: pi = 4.0_dp * atan(1.0_dp)
end module constants_mod
