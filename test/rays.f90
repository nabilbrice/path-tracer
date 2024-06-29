module test_rays
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use rays, only: ray_type, sphere_type
  implicit none
  private

  public :: collect_rays_tests

contains
  !> Collects the rays testsuite
  subroutine collect_rays_tests(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [                                   &
         new_unittest("ray travelling",             &
                      test_ray_get_position),       &
         new_unittest("ray-sphere intersecting",    &
                      test_ray_sphere_intersection) &
         ]
  end subroutine collect_rays_tests

  !> Unit test for the cross product function
  subroutine test_ray_get_position(error)
    type(error_type), allocatable, intent(out) :: error

    type(ray_type) :: ray
    real(r64), dimension(3) :: position, expect
    integer :: i

    call ray%new([real(r64) :: 0.0,0.0,1.0], [real(r64) :: 1.0,1.0,0.0])

    position = ray%get_position(3.0_r64)
    expect = [real(r64) :: 3.0, 3.0, 1.0]

    do i=1,3
       call check(error, position(i) == expect(i))
    end do

  end subroutine test_ray_get_position

  !> Unit test for the ray-sphere intersection
  subroutine test_ray_sphere_intersection(error)
    type(error_type), allocatable, intent(out) :: error

    type(ray_type)          :: ray
    real(r64), dimension(3) :: origin, direction
    real(r64)               :: param, expect
    type(sphere_type)       :: sphere

    origin = [real(r64) :: -10.0, 0.0, 0.0]
    direction = [real(r64) :: 1.0, 0.0, 0.0]

    call sphere%new([real(r64) :: 0.0, 0.0, 0.0], 1.0_r64)
    call ray%new(origin, direction)

    param = ray%intersect_sphere(sphere)
    expect = 9.0_r64

    call check(error, param == expect)

  end subroutine test_ray_sphere_intersection

end module test_rays

program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite
  use test_rays, only: collect_rays_tests
  implicit none
  integer :: stat

  stat = 0

  call run_testsuite(collect_rays_tests, error_unit, stat)

  if (stat > 0) then
     write(error_unit, '(i0, 1x, a)') stat, "rays test(s) failed!"
     error stop
  end if

end program tester

