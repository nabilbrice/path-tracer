module test_io
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use driver_io, only: save_to_ppm
  use camera_mod, only: pixel_type, camera_type
  implicit none
  private

  public :: collect_driver_io

contains
  !> Collects the driver_io testsuites
  subroutine collect_driver_io(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [new_unittest("save to ppm", test_save_to_ppm), &
         new_unittest("larger ppm", test_larger_ppm),   &
         new_unittest("sphere ppm", test_draw_sphere) ]
  end subroutine collect_driver_io

  !> Unit test for saving to ppm file with the standard data
  !> The output should be a file called "test.ppm",
  !> viewable using a standard image viewer
  subroutine test_save_to_ppm(error)
    type(error_type), allocatable, intent(out) :: error

    type(pixel_type), dimension(3, 2) :: pixel_grid

    pixel_grid(1,1)%readout = [255, 0, 0]
    pixel_grid(2,1)%readout = [0, 255, 0]
    pixel_grid(3,1)%readout = [0, 0, 255]
    pixel_grid(1,2)%readout = [255, 255, 0]
    pixel_grid(2,2)%readout = [255, 255, 255]
    pixel_grid(3,2)%readout = [0, 0, 0]

    call save_to_ppm("./test.ppm", pixel_grid)

  end subroutine test_save_to_ppm

  subroutine test_larger_ppm(error)
    type(error_type), allocatable, intent(out) :: error

    type(pixel_type), dimension(256,256) :: pixel_grid
    integer :: column, row

    do row=1,size(pixel_grid,2)
       do column=1,size(pixel_grid,1)
          pixel_grid(column,row)%readout(1) = column
          pixel_grid(column,row)%readout(2) = row
          pixel_grid(column,row)%readout(3) = 0
       end do
    end do

    call save_to_ppm("./background.ppm", pixel_grid)

  end subroutine test_larger_ppm

  subroutine test_draw_sphere(error)
    use rays, only: sphere_type
    type(error_type), allocatable, intent(out) :: error

    type(sphere_type) :: sphere
    type(camera_type) :: camera

    call sphere%new([real(r64) :: 0.0, 0.0, 0.0], 1.0_r64)

    call camera%build(1.5_r64, 512, 512)

    call camera%raytrace(sphere)

    call save_to_ppm("./sphere.ppm", camera%pixel_grid)

  end subroutine test_draw_sphere

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
