module raytracer_mod
  use, intrinsic :: iso_fortran_env, only: r64 => real64, i32 => int32
  use rays_mod, only: ray_type, sphere_type
  use pixels_mod, only: pixel_type, get_readout
  use camera_mod, only: camera_type, sample_as_eye, sample_at_infinity
  use io_mod, only: load_from_dat
  implicit none
  private

  public :: raytrace

contains
  !> Actually the full raytrace subroutine
  ! TODO: needs to be moved to a higher level to avoid circular
  ! dependencies with driver_io
  subroutine raytrace(camera, sphere)
    class(camera_type), intent(inout) :: camera
    type(sphere_type), intent(in)     :: sphere

    type(ray_type) :: ray
    integer(i32) :: i, j
    real(r64) :: param
    real(r64), dimension(2) :: surface_coord
    type(pixel_type), allocatable :: map(:,:)

    ! This actually belongs with building a sphere
    map = load_from_dat("./grid_map.dat")

    ! The contents of these loops can be made
    ! into an elemental procedure
    do j=1,size(camera%image, 2)
       do i=1,size(camera%image,1)
          ! Create a ray at the pixel
          call sample_at_infinity(camera,i,j,ray)
          ! Then test for intersection with the sphere
          param = ray%intersect_sphere(sphere)
          ! If there is an intersection, color the pixel
          if (param > 0.0) then
             surface_coord = sphere%get_surface_coord(ray%get_position(param))
             camera%image(i,j)%readout = get_readout(map, surface_coord)
          else
             ! Pixels begin as empty
             camera%image(i,j)%readout = [0, 0, 0]
          end if
       end do
    end do

  end subroutine raytrace
end module raytracer_mod
