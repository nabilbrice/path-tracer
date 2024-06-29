module camera_mod
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use constants_mod, only: pi
  use vectors, only: normalise
  use rays_mod, only: ray_type
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
     type(pixel_type), allocatable :: image(:,:)
   contains
     procedure :: build => build_camera
     procedure :: get_pixel_offset
  end type camera_type

contains
  subroutine build_camera(camera, distance, image_width, image_height)
    class(camera_type), intent(inout) :: camera
    real(r64), intent(in) :: distance
    integer, intent(in) :: image_width, image_height

    ! Build the camera to be fixed on the z-axis
    camera%position = [real(r64) :: 0.0, 0.0, distance]
    ! Allocate space for storing pixel readouts
    allocate(camera%image(image_width, image_height))

  end subroutine build_camera

  !> Calculates the position of a pixel
  !> relative to the camera position
  function get_pixel_offset(camera, i, j) result(offset)
    class(camera_type), intent(in) :: camera
    integer, intent(in) :: i, j
    real(r64), dimension(3) :: offset

    ! The size of the camera is 2.0 by 2.0
    ! TODO: camera position should really be distance
    offset = camera%position(3) * [                    &
        -1.0 + 2.0*(i - 1.0)/size(camera%image, 1), &
         1.0 - 2.0*(j - 1.0)/size(camera%image, 2), &
         0.0                                       ]

  end function get_pixel_offset

  !> Creates a sample ray, using perspective
  subroutine sample_as_eye(camera, i, j, ray)
    class(camera_type), intent(in)  :: camera
    integer,            intent(in)  :: i, j
    type(ray_type),     intent(out) :: ray

    call ray%new( camera%position, &
         normalise(camera%get_pixel_offset(i,j) - camera%position) )

  end subroutine sample_as_eye

  !> Creates a sample ray, as a detector at infinity
  !> TODO

  !> Gets the readout from a texture map
  function get_readout(map, surface_coord) result(readout)
    type(pixel_type), intent(in) :: map(:,:)
    real(r64), dimension(2), intent(in) :: surface_coord
    integer, dimension(3) :: readout

    integer :: map_width, map_height
    map_width = size(map,1)
    map_height = size(map, 2)
    map_width = int(real(map_width, r64) * surface_coord(2) * 0.9_r64) + 1
    map_height = int(real(map_height, r64) * surface_coord(1) * 0.9_r64) + 1

    readout = map(map_width, map_height)%readout

  end function get_readout

end module camera_mod
