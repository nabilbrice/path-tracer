module camera
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use vectors, only: normalise
  use rays, only: ray_type, sphere_type
  implicit none

  ! This could be replaced by a constant parameter like
  ! integer, parameter :: READOUT_DIM = 3
  ! which would then be exported elsewhere
  type :: pixel_type
     integer, dimension(3) :: readout
  end type pixel_type

  !> The camera_type stores and manages the pixel layout
  !> and the pixel readout
  type :: camera_type
     !> Currently, assume the size of the camera
     real(r64), dimension(3) :: position
     type(pixel_type), allocatable :: pixel_grid(:,:)
   contains
     procedure :: build => build_camera
     procedure :: get_pixel_position
     procedure :: raytrace
  end type camera_type

contains
  subroutine build_camera(camera, distance, image_width, image_height)
    class(camera_type), intent(inout) :: camera
    real(r64), intent(in) :: distance
    integer, intent(in) :: image_width, image_height

    ! Build the camera to be fixed on the z-axis
    camera%position = [real(r64) :: 0.0, 0.0, distance]
    ! Allocate space for storing pixel readouts
    allocate(camera%pixel_grid(image_width, image_height))

  end subroutine build_camera

  !> Calculates the position of a pixel from its index
  !> in the pixel grid
  function get_pixel_position(camera, i, j) result(position)
    class(camera_type), intent(in) :: camera
    integer, intent(in) :: i, j
    real(r64), dimension(3) :: position

    ! The size of the camera is 2.0 by 2.0
    ! TODO: camera position should really be distance
    position = camera%position(3) * [                    &
        -1.0 + 2.0*(i - 1.0)/size(camera%pixel_grid, 1), &
         1.0 - 2.0*(j - 1.0)/size(camera%pixel_grid, 2), &
         0.0                                       ] &
         + camera%position

  end function get_pixel_position

  !> Creates a sample ray, using perspective
  subroutine sample_as_eye(camera, i, j, ray)
    class(camera_type), intent(in)  :: camera
    integer,            intent(in)  :: i, j
    type(ray_type),     intent(out) :: ray

    call ray%new( camera%position, &
         normalise(camera%get_pixel_position(i,j) - 2.0 * camera%position) )

  end subroutine sample_as_eye

  !> Creates a sample ray, as a detector at infinity
  !> TODO

  !> Actually the full raytrace subroutine
  subroutine raytrace(camera, sphere)
    class(camera_type), intent(inout) :: camera
    type(sphere_type), intent(in)     :: sphere

    type(ray_type) :: ray
    integer :: i, j
    real(r64) :: param

    ! The contents of these loops can be made
    ! into an elemental procedure
    do j=1,size(camera%pixel_grid, 2)
       do i=1,size(camera%pixel_grid,1)
          ! Create a ray at the pixel
          call sample_as_eye(camera,i,j,ray)
          ! Then test for intersection with the sphere
          param = ray%intersect_sphere(sphere)
          ! If there is an intersection, color the pixel
          if (param > 0.0) then
             camera%pixel_grid(i,j)%readout &
                  = [100, 100, 100] &
                  + [100, 100, 100] * sphere%get_normal(ray%get_position(param))
          else
             ! Pixels begin as empty
             camera%pixel_grid(i,j)%readout = [0, 0, 0]
          end if
       end do
    end do

  end subroutine raytrace

end module camera
