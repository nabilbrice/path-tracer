module test_io
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use driver_io, only: save_to_ppm
  implicit none
  private

  public :: collect_driver_io

contains
  !> Collects the driver_io testsuites
  subroutine collect_driver_io(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [new_unittest("save to ppm", test_save_to_ppm)]
  end subroutine collect_driver_io

  !> Unit test for saving to ppm file with the standard data
  !> The output should be a file called "test.ppm",
  !> viewable using a standard image viewer
  subroutine test_save_to_ppm(error)
    type(error_type), allocatable, intent(out) :: error

    integer, dimension(3,6) :: data

    ! The standard data
    data = reshape( [ &
      255, 0, 0,      &
      0, 255, 0,      &
      0, 0, 255,      &
      255, 255, 0,    &
      255, 255, 255,  &
      0, 0, 0         & 
      ], shape(data))

    call save_to_ppm("./test.ppm", data)
    
  end subroutine test_save_to_ppm
  
end module test_io

program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite
  use test_io,   only: collect_driver_io
  implicit none
  integer :: stat

  stat = 0

  ! Run the testsuite for the appropriate collection of tests
  call run_testsuite(collect_driver_io, error_unit, stat)

  if (stat > 0) then
     write(error_unit, '(i0, 1x, a)') stat, "io test(s) failed!"
     error stop
  end if

end program tester

