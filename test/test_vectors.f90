module test_vectors
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use vectors, only: operator(.cross.)
  implicit none
  private

  public :: collect_vectors_tests

contains
  !> Collects the vectors testsuite
  subroutine collect_vectors_tests(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [new_unittest("cross product", test_cross_product)]
  end subroutine collect_vectors_tests

  !> Unit test for the cross product function
  subroutine test_cross_product(error)
    type(error_type), allocatable, intent(out) :: error

    real(r64), dimension(3) :: a, b, c, expect
    integer :: i

    a = [1.0, 0.0, 0.0]
    b = [0.0, 1.0, 0.0]

    c = a .cross. b
    expect = [0.0, 0.0, 1.0]

    do i=1,3
       call check(error, c(i) == expect(i))
    end do
    
  end subroutine test_cross_product

end module test_vectors

program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive,     only: run_testsuite
  use test_vectors,  only: collect_vectors_tests
  implicit none
  integer :: stat

  stat = 0

  ! Run the testsuite for the appropriate collection of tests
  call run_testsuite(collect_vectors_tests, error_unit, stat)

  if (stat > 0) then
     write(error_unit, '(i0, 1x, a)') stat, "vectors test(s) failed!"
     error stop
  end if

end program tester

