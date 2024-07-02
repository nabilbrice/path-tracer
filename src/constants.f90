module constants_mod
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  real(dp), parameter :: pi = 4.0_dp * atan(1.0_dp)

  ! The geometry of the spacetime is fixed,
  ! so it can be set as parameters here
  ! This is the general relativistic compactness parameter,
  ! given by 1 - u = (1 - 2M / R),
  ! where M is the mass, R is the surface radius
  real(dp), parameter :: gr_compactness = 0.5_dp
end module constants_mod
