program surface_generator
  use, intrinsic :: iso_fortran_env, only: i32 => int32
  use pixels_mod, only: pixel_type
  use surfaces_mod, only: generate_gridded_surface
  use io_mod, only: save_to_dat
  implicit none

  type(pixel_type), dimension(512,512) :: image

  call generate_gridded_surface(image)

  call save_to_dat("./grid_map.dat", image)

end program surface_generator
