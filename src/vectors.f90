module vectors
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  implicit none
  private

  public :: cross_product

contains
  function cross_product(a, b) result(c)
    real(r64), dimension(3), intent(in)  :: a, b
    real(r64), dimension(3)              :: c

    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
    
  end function cross_product
  
end module vectors

