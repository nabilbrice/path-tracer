module test_rays
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use constants_mod, only: pi
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use rays_mod
  implicit none
  private

  public :: collect_rays_tests

contains
  !> Collects the rays testsuite
  subroutine collect_rays_tests(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [                                    &
         new_unittest("ray travelling",              &
                      test_ray_get_position),        &
         new_unittest("ray-sphere intersecting",     &
         test_ray_sphere_intersection), &
         new_unittest("normalised spherical coordinates",  &
         test_get_surface_coord),                    &
         new_unittest("ray-sphere gr intersecting",  &
         test_ray_sphere_gr_intersection),           &
         new_unittest("get gr position",             &
         test_gr_get_position)]
  end subroutine collect_rays_tests

  !> Unit test for the cross product function
  subroutine test_ray_get_position(error)
    type(error_type), allocatable, intent(out) :: error

    type(ray_type) :: ray
    real(r64), dimension(3) :: position, expect
    integer :: i

    ray % origin = [0_r64,0_r64,1_r64]
    ray % direction = [1_r64,1_r64,0_r64]

    position = ray .at. 3.0_r64
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

    ray % origin = [real(r64) :: -10.0, 0.0, 0.0]
    ray % direction = [real(r64) :: 1.0, 0.0, 0.0]

    call new_sphere(sphere, [real(r64) :: 0.0, 0.0, 0.0], 1.0_r64, 0.0_r64)

    param = intersect_sphere(ray, sphere)
    expect = 9.0_r64

    call check(error, param == expect)

  end subroutine test_ray_sphere_intersection

  subroutine test_ray_sphere_gr_intersection(error)
    type(error_type), allocatable, intent(out) :: error

    type(ray_type)          :: ray
    real(r64), dimension(3) :: origin, direction
    real(r64)               :: param, expect
    type(sphere_type)       :: sphere

    ! Offset is outside the typical than the radius of the sphere
    ray % origin = [real(r64) :: 0.0, 0.0, 10.0]
    ray % direction = [real(r64) :: 0.0, 0.0, -1.0]

    call new_sphere(sphere, [real(r64) :: 0.0, 0.0, 0.0], 1.0_r64, 0.0_r64)

    param = gr_intersect_sphere(ray, sphere)
    call check(error, (param > -1.0_r64))

  end subroutine test_ray_sphere_gr_intersection

  !> Unit test for the local coordinate transformation
  subroutine test_get_surface_coord(error)
    type(error_type), allocatable, intent(out) :: error

    type(sphere_type) :: sphere
    real(r64), dimension(3) :: location
    real(r64), dimension(2) :: surface_coord
    real(r64), dimension(2) :: expect

    call new_sphere(sphere, [real(r64) :: 0.0, 0.0, 0.0], 1.0_r64, 0.0_r64)
    location = [real(r64) :: 1.0, 0.0, 0.0]
    expect = [real(r64) :: 0.5, 0.5]
    surface_coord = get_surface_coord(sphere, location)
    call check(error, surface_coord(1), expect(1))
    call check(error, surface_coord(2), expect(2))

    location = [real(r64) :: 0.0, -1.0, 0.0]
    expect = [real(r64) :: 0.5, 0.25]
    surface_coord = get_surface_coord(sphere, location)
    call check(error, surface_coord(1), expect(1))
    call check(error, surface_coord(2), expect(2))

  end subroutine test_get_surface_coord

  !> Unit test for the local coordinate transformation
  subroutine test_gr_get_position(error)
    type(error_type), allocatable, intent(out) :: error

    type(ray_type)    :: ray
    type(sphere_type) :: sphere
    real(r64), dimension(3) :: origin, direction, intersect_position
    real(r64), dimension(2) :: surface_coord
    real(r64) :: param, latitude

    ! Offset is outside the typical than the radius of the sphere
    ray % origin = [real(r64) :: 1.0, 0.0, 10.0]
    ray % direction = [real(r64) :: 0.0, 0.0, -1.0]

    call new_sphere(sphere, [real(r64) :: 0.0, 0.0, 0.0], 1.0_r64, 0.0_r64)

    intersect_position = gr_get_position(ray, gr_intersect_sphere(ray, sphere))
    surface_coord = get_surface_coord(sphere, intersect_position)
    latitude = surface_coord(1) * pi
    call check(error, gr_intersect_sphere(ray, sphere), cos(latitude))

  end subroutine test_gr_get_position

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

