module raytracer_mod
  use, intrinsic :: iso_fortran_env, only: r64 => real64, i32 => int32
  use vectors, only: normalise
  use rays_mod
  use pixels_mod, only: pixel_type, spectrum
  use camera_mod, only: camera_type, sample_as_eye, sample_at_infinity
  use io_mod
  implicit none
  private

  public :: raytrace, gr_raytrace

contains
  !> Actually the full raytrace subroutine
  ! TODO: needs to be moved to a higher level to avoid circular
  ! dependencies with driver_io
  subroutine raytrace(camera, sphere)
    class(camera_type), intent(inout) :: camera
    type(sphere_type), intent(in)     :: sphere

    type(ray_type) :: ray
    integer(i32) :: i, j
    real(r64) :: intersect_param
    real(r64), dimension(2) :: surface_coord
    type(pixel_type), allocatable :: map(:,:)

    ! This actually belongs with building a sphere
    map = load_from_ppm("./earthmap.ppm")

    ! The contents of these loops can be made
    ! into an elemental procedure
    do j=1,size(camera%image, 2)
       do i=1,size(camera%image,1)
          ! Create a ray at the pixel
          call sample_at_infinity(camera,i,j,ray)
          ! Then test for intersection with the sphere
          intersect_param = ray .intersect. sphere
          ! If there is an intersection, color the pixel
          if (intersect_param > 0.0) then
             surface_coord = get_surface_coord(sphere, ray .at. intersect_param)
             camera%image(i,j)%spectrum = spectrum(map, surface_coord)
          else
             ! Pixels begin as empty
             camera%image(i,j)%spectrum = [0, 0, 0]
          end if
       end do
    end do

  end subroutine raytrace

  subroutine gr_raytrace(camera, sphere)
    class(camera_type), intent(inout) :: camera
    type(sphere_type), intent(in)     :: sphere

    type(ray_type) :: ray
    integer(i32) :: i, j
    real(r64) :: param
    real(r64), dimension(2) :: surface_coord
    type(pixel_type), allocatable :: map(:,:)

    ! This actually belongs with building a sphere
    map = load_from_ppm("./earthmap.ppm")

    ! The contents of these loops can be made
    ! into an elemental procedure
    do j=1,size(camera%image, 2)
       do i=1,size(camera%image,1)
          ! Create a ray at the pixel
          call sample_at_infinity(camera,i,j,ray)
          ! Then test for intersection with the sphere
          param = gr_intersect_sphere(ray, sphere)
          ! If there is an intersection, color the pixel
          if (param > -0.99) then
             surface_coord = get_surface_coord( sphere, &
                  gr_get_position(ray, param) )
             camera%image(i,j)%spectrum = spectrum(map, surface_coord)
          end if
       end do
    end do

  end subroutine gr_raytrace

end module raytracer_mod
