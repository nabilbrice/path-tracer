program surface_generator
  use, intrinsic :: iso_fortran_env, only: i32 => int32
  use pixels_mod, only: pixel_type
  use surfaces_mod, only: generate_gridded_surface, &
                          generate_chequered_surface
  use io_mod
  implicit none

  type(pixel_type), dimension(512,512) :: image
  type(pixel_type), dimension(16,16)   :: cheq_image

  call generate_gridded_surface(image)
  call generate_chequered_surface(cheq_image)

  ! Save to the data array binary format used in this app
  call save_to_dab("./grid_map.dab", image)
  call save_to_dab("./chequered_map.dab", cheq_image)

end program surface_generator
