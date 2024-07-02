module surfaces_mod
  use, intrinsic :: iso_fortran_env, only: i32 => int32
  use pixels_mod, only: pixel_type

  implicit none

contains
  !> Subroutine to generate a gridded pattern for a surface map
  subroutine generate_gridded_surface(image)
    type(pixel_type), intent(out) :: image(:,:)

    integer(i32) :: j, k, image_width, image_height
    image_width = size(image,1)
    image_height = size(image,2)

    do k=1,image_height
       do j=1,image_width
          if ((modulo(k,8)/=0).and.(modulo(j,8)/=0)) then
             image(j,k)%readout = [integer(i32) :: 255, 0, 0]
          end if
       end do
    end do

  end subroutine generate_gridded_surface

  !> Subroutine to generate a checkered pattern for a surface map
  subroutine generate_chequered_surface(image)
    type(pixel_type), intent(out) :: image(:,:)

    integer(i32) :: j, k, image_width, image_height
    image_width = size(image,1)
    image_height = size(image,2)

    do k=1,image_height
       do j=1, image_width
          select case (modulo(j+k, 2))
          case (0)
             image(j,k)%readout = [integer(i32) :: 255, 0, 0]
          case (1)
             image(j,k)%readout = [integer(i32) :: 0, 255, 0]
          end select
       end do
    end do

  end subroutine generate_chequered_surface

end module surfaces_mod
