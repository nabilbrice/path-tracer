program main
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use camera_mod, only: camera_type
  use rays_mod, only: sphere_type
  use io_mod, only: save_to_ppm
  use raytracer_mod, only: raytrace
  implicit none

  type(sphere_type) :: sphere
  type(camera_type) :: camera

  ! Construct the scene
  call sphere%new([real(r64) :: 0.0, 0.0, 0.0], 1.0_r64)
  call camera%build(1.5_r64, 512, 512)

  call raytrace(camera, sphere)

  call save_to_ppm("./image.ppm", camera%image)

end program main
