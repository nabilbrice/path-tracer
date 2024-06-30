module pixels_mod
  use, intrinsic :: iso_fortran_env, only: r64 => real64, i32 => int32
  implicit none
  private

  public :: pixel_type, get_readout

  !> The pixel_type could be replaced with a parameter
  !> like READOUT_DIMS = 3
  type :: pixel_type
     integer, dimension(3) :: readout
  end type pixel_type

contains
  !> Gets the readout from an image through normalised coordinates
  !> i.e. reals from 0 to 1
  !> This function can be used instead of indexing into the image
  !> directly
  function get_readout(image, normalised_coord) result(readout)
    type(pixel_type), intent(in) :: image(:,:)
    real(r64), dimension(2), intent(in) :: normalised_coord
    integer(i32), dimension(3) :: readout

    integer :: image_width, image_height
    image_width = size(image,1)
    image_height = size(image, 2)
    image_width = int(real(image_width, r64) * normalised_coord(2) * 0.9) + 1
    image_height = int(real(image_height, r64) * normalised_coord(1) * 0.9) + 1

    readout = image(image_width, image_height)%readout

  end function get_readout

end module pixels_mod